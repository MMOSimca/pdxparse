{-|
Module      : HOI4.Handlers.Industry
Description : Equipment, production and resources

Handlers for equipment and its production, licenses, resources and their
rights, and the military industrial organizations.
-}
module HOI4.Handlers.Industry (
        stateResource
    ,   hasResourcesInCountry
    ,   hasResourcesInCollection
    ,   createEquipmentVariant
    ,   sendEquipment
    ,   hasLicense
    ,   addResource
    ,   modifyBuildingResources
    ,   addEquipment
    ,   giveResourceRights
    ,   hasResourcesRights
    ,   hasResourcesAmount
    ,   addEquipmentSubsidy
    ,   addEquipmentProduction
    ,   createProductionLicense
    ,   mioTooltip
    ,   mioScope
    ) where

import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (foldM)

import Abstract -- everything
import qualified Doc -- everything
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, withCurrentIndent)
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Templates
import HOI4.Types -- everything

import HOI4.Handlers.Core (bareAtom, joinClauses, msgToPP, plainMsg', preMessage, preStatement, statedFrom)
import HOI4.Handlers.Generic (numericCompare)

-- | Handler for a resource's own name used as a trigger, which compares how
-- much of the resource the state has.
stateResource :: (HOI4Info g, Monad m) => Text -> StatementHandler g m
stateResource res = numericCompare "more than" "less than"
    (MsgStateResource (iconText res)) (MsgStateResourceVar (iconText res))

-- | Handler for @has_resources_in_country@, which asks how much of a resource a
-- country has to hand.
--
-- Resources come in whole units, so a test for more than forty-nine of something
-- is a test for fifty, and reads better written that way.
-- | How much of a resource is asked for, and of what kind of stock. The same
-- fields are written whether the question is put to one country or to a
-- collection of them.
data ResourceCheck = ResourceCheck
        {   rc_resource :: Maybe Text
        ,   rc_amount :: Maybe (Text, Double)
        ,   rc_qualifier :: Text
        ,   rc_collection :: Maybe Text
        }

parseResourceCheck :: String -> GenericScript -> ResourceCheck
parseResourceCheck what = foldl' addLine (ResourceCheck Nothing Nothing "" Nothing)
    where
        addLine rc [pdx| resource = ?r |] = rc { rc_resource = Just r }
        addLine rc [pdx| amount > !n |] = rc { rc_amount = Just ("at least", n + 1) }
        addLine rc [pdx| amount < !n |] = rc { rc_amount = Just ("less than", n) }
        addLine rc [pdx| amount = !n |] = rc { rc_amount = Just ("exactly", n) }
        -- Whether what the country digs up itself counts, or only what it buys.
        addLine rc [pdx| extracted = yes |] = rc { rc_qualifier = "extracted " }
        addLine rc [pdx| only_imported = yes |] = rc { rc_qualifier = "imported " }
        addLine rc [pdx| extracted = no |] = rc
        addLine rc [pdx| only_imported = no |] = rc
        -- Counts what the country's buildings take rather than what it holds.
        -- Nothing in the wording tells the two apart as yet.
        addLine rc [pdx| buildings = %_ |] = rc
        addLine rc [pdx| collection = ?coll |] = rc { rc_collection = Just coll }
        addLine rc stmt = warn (UnknownSection (T.pack what) stmt) rc

hasResourcesInCountry :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasResourcesInCountry stmt@[pdx| %_ = @scr |] =
    case parseResourceCheck "has_resources_in_country" scr of
        ResourceCheck (Just res) (Just (comp, amt)) qualifier _ ->
            msgToPP $ MsgHasResourcesInCountry qualifier comp amt (iconText res)
        _ -> preStatement stmt
hasResourcesInCountry stmt = preStatement stmt

-- | As 'hasResourcesInCountry', over every country a named collection gathers
-- rather than the one in scope. Script writes the collection as @collection:@
-- and its name, which is localized under that name in capitals.
hasResourcesInCollection :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasResourcesInCollection stmt@[pdx| %_ = @scr |] =
    case parseResourceCheck "has_resources_in_collection" scr of
        ResourceCheck (Just res) (Just (comp, amt)) qualifier (Just coll) -> do
            let collkey = fromMaybe coll (T.stripPrefix "collection:" coll)
            collloc <- getGameL10n ("COLLECTION_" <> T.toUpper collkey)
            msgToPP $ MsgHasResourcesInCollection qualifier comp amt (iconText res) collloc
        _ -> preStatement stmt
hasResourcesInCollection stmt = preStatement stmt

createEquipmentVariant :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createEquipmentVariant stmt@[pdx| %_ = @scr |] = do
    let (_name, _) = extractStmt (matchLhsText "name") scr
        (_typed, _) = extractStmt (matchLhsText "type") scr
    name <- case _name of
        Just [pdx| %_ = ?txt |] -> return txt
        _ -> return "<!-- Check Script -->"
    typed <- case _typed of
        Just [pdx| %_ = $txt |] -> getGameL10n txt
        _ -> return "<!-- Check Script -->"
    msgToPP $ MsgCreateEquipmentVariant typed name
createEquipmentVariant stmt = preStatement stmt

--------------------------------
-- Handler for send_equipment --
--------------------------------
data SendEquipment = SendEquipment
        {   se_equipment :: Text
        ,   se_amount :: Text
        ,   se_old_prioritised :: Bool
        ,   se_target :: Maybe Text
        }

newSE :: SendEquipment
newSE = SendEquipment undefined undefined False Nothing

sendEquipment  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
sendEquipment stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_se =<< foldM addLine newSE scr
    where
        addLine :: SendEquipment -> GenericStatement -> PPT g m SendEquipment
        addLine se [pdx| equipment = $txt |] = do
            txtd <- getGameL10n txt
            return se { se_equipment = txtd }
        addLine se [pdx| type = $txt |] = do
            txtd <- getGameL10n txt
            return se { se_equipment = txtd }
        addLine se [pdx| amount = !amt |] =
            let amtd = T.pack $ show (amt :: Int)  in
            return se { se_amount = amtd }
        addLine se [pdx| amount = $amt |] =
            return se { se_amount = amt }
        addLine se [pdx| old_prioritised = %rhs |]
            | GenericRhs "yes" [] <- rhs = return se { se_old_prioritised = True }
            | GenericRhs "no"  [] <- rhs = return se { se_old_prioritised = False }
        addLine se [pdx| target = $vartag:$var |] = do
            flagd <- eflag (Just HOI4Country) (Right (vartag,var))
            return se { se_target = flagd }
        addLine se [pdx| target = $tag |] = do
            flagd <- eflag (Just HOI4Country) (Left tag)
            return se { se_target = flagd }
        addLine se stmt
            = warn (UnknownSection "send_equipment" stmt) $ return se
        pp_se se = do
            let target = fromMaybe "<!-- Check Script -->" (se_target se)
            return $ MsgSendEquipment (se_amount se) (se_equipment se) target (se_old_prioritised se)
sendEquipment stmt = preStatement stmt

------------------------------
-- handler for has_license  --
------------------------------

-- | Whether the country holds a production license, which names either an
-- archetype or one exact piece of equipment, and may name who granted it.
hasLicense :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasLicense stmt@[pdx| %_ = @scr |] = do
    let (mfrom, s1) = extractStmt (matchLhsText "from") scr
        (marchetype, s2) = extractStmt (matchLhsText "archetype") s1
        (mequipment, _) = extractStmt (matchLhsText "equipment") s2
    whoflag <- case mfrom of
        Just [pdx| %_ = ?who |] -> flagText (Just HOI4Country) who
        _ -> return ""
    mwhat <- case (marchetype, mequipment) of
        (Just [pdx| %_ = ?arch |], _) -> Just <$> getGameL10n arch
        (_, Just [pdx| %_ = @equip |]) ->
            case fst (extractStmt (matchLhsText "type") equip) of
                Just [pdx| %_ = ?ty |] -> Just <$> getGameL10n ty
                _ -> return Nothing
        _ -> return Nothing
    case mwhat of
        Just what -> msgToPP $ MsgHasLicense whoflag what
        Nothing -> preStatement stmt
hasLicense stmt = preStatement stmt

------------------------------
-- Handler for add_resource --
------------------------------
data AddResource = AddResource
        {   ar_type :: Text
        ,   ar_amount :: Maybe Double
        ,   ar_amountvar :: Maybe Text
        ,   ar_state :: Maybe Double
        ,   ar_statepron :: Maybe Text
        }

newAR :: AddResource
newAR = AddResource undefined Nothing Nothing Nothing Nothing

addResource  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addResource stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_ar =<< foldM addLine newAR scr
    where
        addLine :: AddResource -> GenericStatement -> PPT g m AddResource
        addLine ar [pdx| type = $txt |] = return ar { ar_type = txt }
        addLine ar [pdx| amount = !num |] = return ar { ar_amount = Just num }
        addLine ar [pdx| amount = $txt |] = return ar { ar_amountvar = Just txt }
        addLine ar [pdx| state = !num |] = return ar { ar_state = Just num }
        -- The state may also be a pronoun for the state in scope.
        addLine ar [pdx| state = $txt |] = return ar { ar_statepron = Just txt }
        addLine ar [pdx| show_state_in_tooltip = %_ |] = return ar
        addLine ar stmt = warn (UnknownSection "add_resource" stmt) $ return ar
        pp_ar ar = do
            let buildicon = iconText $ ar_type ar
            stateloc <- case (ar_state ar, ar_statepron ar) of
                (Just num, _) -> getStateLoc (round num)
                (_, Just pron) -> eGetStateText (Left pron)
                _ -> return ""
            buildloc <- getGameL10n $ "PRODUCTION_MATERIALS_" <> T.toUpper (ar_type ar)
            case (ar_amount ar, ar_amountvar ar) of
                    (Just amount, _) -> return $ MsgAddResource buildicon buildloc amount stateloc
                    (_, Just amountvar) -> return $ MsgAddResourceVar buildicon buildloc amountvar stateloc
                    _ -> return $ preMessage stmt
addResource stmt = preStatement stmt

-------------------------------------------
-- Handler for modify_building_resources --
-------------------------------------------
foldCompound "modifyBuildingResources" "ModifyBuildingResources" "mbr"
    []
    [CompField "building" [t|Text|] Nothing True
    ,CompField "resource" [t|Text|] Nothing True
    ,CompField "amount" [t|Double|] Nothing True
    ]
    [|  do
        buildicon <- buildingIcon _building
        let resourceicon = iconText _resource
        return $ MsgModifyBuildingResources buildicon resourceicon _amount
    |]

------------------------------
-- Handler for add_equipment_to_stockpile --
------------------------------
data AddEquipment = AddEquipment
        {   ae_type :: Text
        ,   ae_amount :: Maybe Double
        ,   ae_amountvar :: Maybe Text
        ,   ae_producer :: Maybe (Either Text (Text, Text))
        ,   ae_variant :: Maybe Text
        }

newAE :: AddEquipment
newAE = AddEquipment undefined Nothing Nothing Nothing Nothing

addEquipment  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addEquipment stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppAe =<< foldM addLine newAE scr
    where
        addLine :: AddEquipment -> GenericStatement -> PPT g m AddEquipment
        addLine ae [pdx| type = ?txt |] = return ae { ae_type = txt }
        addLine ae [pdx| amount = !num |] = return ae { ae_amount = Just num }
        addLine ae [pdx| amount = ?txt |] = return ae { ae_amountvar = Just txt }
        addLine ae [pdx| producer = ?tag |] = return ae { ae_producer = Just (Left tag) }
        addLine ae [pdx| producer = $vartag:$var |] = return ae { ae_producer = Just (Right (vartag, var)) }
        addLine ae [pdx| variant_name = ?txt |] = return ae { ae_variant = Just txt }
        addLine ae stmt = warn (UnknownSection "add_equipment_to_stockpile" stmt) $ return ae
        ppAe ae = do
            let variant = fromMaybe "" $ ae_variant ae
            flaglocm <- case ae_producer ae of
                Just producer -> eflag (Just HOI4Country) producer
                _ -> return Nothing
            let flagloc = fromMaybe "" flaglocm
            equiploc <- getGameL10n $ ae_type ae
            case (ae_amount ae, ae_amountvar ae) of
                    (Just amount, _) -> return $ MsgAddEquipmentToStockpile amount flagloc equiploc variant
                    (_, Just amountvar) -> return $ MsgAddEquipmentToStockpileVar amountvar flagloc equiploc variant
                    _ -> return $ preMessage stmt
addEquipment stmt = preStatement stmt

--------------------------------------
-- Handler for give_resource_rights --
--------------------------------------
data GiveRights = GiveRights
        {   gr_receiver :: Text
        ,   gr_state :: Maybe Int
        ,   gr_statevar :: Maybe Text
        ,   gr_resource :: Maybe [Text]
        }

newGR :: GiveRights
newGR = GiveRights undefined Nothing Nothing Nothing

giveResourceRights :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
giveResourceRights stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppGR =<< foldM addLine newGR scr
    where
        addLine :: GiveRights -> GenericStatement -> PPT g m GiveRights
        addLine gr [pdx| receiver = ?txt |] = return gr { gr_receiver = txt }
        addLine gr [pdx| state = !num |] = return gr { gr_state = Just num }
        addLine gr [pdx| state = ?txt |] = return gr { gr_statevar = Just txt }
        addLine gr [pdx| resources = @scr |] =
            let ress = mapMaybe getbareRess scr in
            return gr { gr_resource = Just ress }
        addLine gr stmt = warn (UnknownSection "give_resource_rights" stmt) $ return gr
        ppGR :: GiveRights -> PPT g m ScriptMessage
        ppGR gr = do
            flag_loc <- flagText (Just HOI4Country) (gr_receiver gr)
            resloc <- case gr_resource gr of
                Just resl -> do
                    reslloc <- traverse getGameL10n resl
                    return $ T.intercalate ", " resl <> " "
                _ -> return mempty
            case (gr_state gr, gr_statevar gr) of
                (Just state, _) -> do
                    state_loc <- getStateLoc state
                    return $ MsgGiveResourceRights flag_loc state_loc resloc
                (_, Just state) -> do
                    state_loc <- eGetStateText (Left state)
                    return $ MsgGiveResourceRights flag_loc state_loc resloc
                _ -> return $ preMessage stmt

        getbareRess (StatementBare (GenericLhs e [])) = Just e
        getbareRess stmt = warn (UnknownSection "give_resource_rights array" stmt) Nothing
giveResourceRights stmt = preStatement stmt

-- | Whether anyone holds the rights to a state's resources. The receiver and
-- the state may each be left out where the scope already names one of them.
hasResourcesRights :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasResourcesRights stmt@[pdx| %_ = @scr |] = do
    let (mreceiver, s1) = extractStmt (matchLhsText "receiver") scr
        (mstate, s2) = extractStmt (matchLhsText "state") s1
        (mresources, _) = extractStmt (matchLhsText "resources") s2
    whoflag <- case mreceiver of
        Just [pdx| %_ = ?who |] -> flagText (Just HOI4Country) who
        _ -> return ""
    -- With no state named the trigger asks about the state in scope.
    whereloc <- case mstate of
        Just st -> statedFrom (Just st)
        _ -> return "the state"
    resloc <- case mresources of
        Just [pdx| %_ = @ress |] -> do
            locs <- traverse getGameL10n (mapMaybe bareAtom ress)
            return $ if null locs then "" else T.intercalate ", " locs <> " "
        _ -> return ""
    msgToPP $ MsgHasResourcesRights whoflag whereloc resloc
hasResourcesRights stmt = preStatement stmt

-------------------------------------
-- Handler for has_resources_amount --
-------------------------------------
data HasResAmount = HasResAmount
        {   hra_resource :: Maybe Text
        ,   hra_comp :: Text
        ,   hra_amount :: Double
        ,   hra_state :: Maybe Int
        ,   hra_delivered :: Bool
        }

newHRA :: HasResAmount
newHRA = HasResAmount Nothing "at least" 0 Nothing False

-- | Handler for @has_resources_amount@, which asks how much of a resource a
-- state has. @delivered@ asks after what actually reaches the state's
-- controller, which is what is left once the modifiers on it have applied.
hasResourcesAmount :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasResourcesAmount stmt@[pdx| %_ = @scr |] = case hra_resource hra of
        Just res -> do
            stateloc <- traverse getStateLoc (hra_state hra)
            msgToPP $ MsgHasResourcesAmount (hra_delivered hra) (hra_comp hra)
                        (hra_amount hra) (iconText res) (fromMaybe "" stateloc)
        Nothing -> preStatement stmt
    where
        hra = foldl' addLine newHRA scr
        addLine h [pdx| resource = ?r |] = h { hra_resource = Just r }
        addLine h [pdx| amount > !n |] = h { hra_comp = "more than", hra_amount = n }
        addLine h [pdx| amount < !n |] = h { hra_comp = "less than", hra_amount = n }
        addLine h [pdx| amount = !n |] = h { hra_comp = "at least", hra_amount = n }
        addLine h [pdx| state = !n |] = h { hra_state = Just n }
        addLine h [pdx| delivered = yes |] = h { hra_delivered = True }
        addLine h [pdx| delivered = no |] = h { hra_delivered = False }
        addLine h stmt = warn (UnknownSection "has_resources_amount" stmt) h
hasResourcesAmount stmt = preStatement stmt

-------------------------------------
-- Handler for add_equipment_subsidy --
-------------------------------------
-- | Handler for @add_equipment_subsidy@, which sets aside industrial capacity to
-- pay for equipment bought from someone else. The sellers it may be spent with
-- are named either outright or by a scripted trigger.
addEquipmentSubsidy :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addEquipmentSubsidy stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Left 0, [], Nothing) scr of
        (Just eqtype, cic, sellers, mtrigger) -> do
            eqloc <- T.strip <$> getGameL10n eqtype
            sellerlocs <- traverse (flagText (Just HOI4Country)) sellers
            msgToPP $ case cic of
                Left n -> MsgAddEquipmentSubsidy n eqloc
                            (joinClauses sellerlocs) (fromMaybe "" mtrigger)
                Right v -> MsgAddEquipmentSubsidyVar v eqloc
                            (joinClauses sellerlocs) (fromMaybe "" mtrigger)
        _ -> preStatement stmt
    where
        addLine (t, c, s, g) [pdx| equipment_type = $ty |] = (Just ty, c, s, g)
        addLine (t, c, s, g) [pdx| cic = !n |] = (t, Left n, s, g)
        -- The capacity set aside may be held in a variable or a script
        -- constant instead of being written as a number.
        addLine (t, c, s, g) [pdx| cic = $v |] = (t, Right v, s, g)
        addLine (t, c, s, g) [pdx| cic = $vartag:$var |] = (t, Right (vartag <> ":" <> var), s, g)
        addLine (t, c, s, g) [pdx| seller_tags = @tags |] =
            (t, c, s ++ [tag | StatementBare (GenericLhs tag []) <- tags], g)
        addLine (t, c, s, g) [pdx| seller_trigger = $trg |] = (t, c, s, Just trg)
        addLine acc stmt = warn (UnknownSection "add_equipment_subsidy" stmt) acc
addEquipmentSubsidy stmt = preStatement stmt

----------------------------------------
-- Handler for add_equipment_production --
----------------------------------------
data AddEquipProd = AddEquipProd
        {   aep_type :: Maybe Text
        ,   aep_creator :: Maybe Text
        ,   aep_version :: Maybe Text
        ,   aep_name :: Maybe Text
        ,   aep_factories :: Maybe Double
        ,   aep_progress :: Maybe Double
        ,   aep_efficiency :: Maybe Double
        }

newAEP :: AddEquipProd
newAEP = AddEquipProd Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Handler for @add_equipment_production@, which opens a production line ready
-- built rather than leaving the player to set one up.
addEquipmentProduction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addEquipmentProduction stmt@[pdx| %_ = @scr |] = case aep_type aep of
        Just eqtype -> do
            eqloc <- T.strip <$> getGameL10n eqtype
            creator <- traverse (flagText (Just HOI4Country)) (aep_creator aep)
            msgToPP $ MsgAddEquipmentProduction
                        (fromMaybe 1 (aep_factories aep)) eqloc
                        (fromMaybe "" (aep_version aep)) (fromMaybe "" creator)
                        (fromMaybe "" (aep_name aep))
                        (fromMaybe 0 (aep_progress aep))
                        (fromMaybe 0 (aep_efficiency aep))
        Nothing -> preStatement stmt
    where
        aep = foldl' addLine newAEP scr
        addLine a [pdx| equipment = @escr |] = foldl' addEquip a escr
        addLine a [pdx| name = ?n |] = a { aep_name = Just n }
        addLine a [pdx| requested_factories = !n |] = a { aep_factories = Just n }
        addLine a [pdx| progress = !n |] = a { aep_progress = Just n }
        addLine a [pdx| efficiency = !n |] = a { aep_efficiency = Just n }
        -- How many of the thing to make. The line runs until it is called off
        -- either way, so it is the factories put on it that are worth reading.
        addLine a [pdx| amount = %_ |] = a
        -- Which organization builds it, which the wiki says nothing about yet.
        addLine a [pdx| industrial_manufacturer = %_ |] = a
        addLine a stmt = warn (UnknownSection "add_equipment_production" stmt) a

        addEquip a [pdx| type = $t |] = a { aep_type = Just t }
        addEquip a [pdx| creator = ?c |] = a { aep_creator = Just c }
        addEquip a [pdx| version_name = ?v |] = a { aep_version = Just v }
        addEquip a [pdx| version = %_ |] = a
        addEquip a stmt = warn (UnknownSection "add_equipment_production@equipment" stmt) a
addEquipmentProduction stmt = preStatement stmt

-----------------------------------------
-- Handler for create_production_license --
-----------------------------------------
-- | Handler for @create_production_license@, which lets another country build
-- one of this country's designs.
createProductionLicense :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createProductionLicense stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing, Nothing) scr of
        (Just eqtype, target, version, cost) -> do
            eqloc <- T.strip <$> getGameL10n eqtype
            targetloc <- case target of
                Just who -> fromMaybe "" <$> eflag (Just HOI4Country) who
                Nothing -> return ""
            msgToPP $ MsgCreateProductionLicense targetloc eqloc
                        (fromMaybe "" version) (fromMaybe 1 cost)
        _ -> preStatement stmt
    where
        addLine (t, tg, v, c) [pdx| equipment = @escr |] = foldl' addEquip (t, tg, v, c) escr
        addLine (t, tg, v, c) [pdx| target = $vartag:$var |] = (t, Just (Right (vartag, var)), v, c)
        addLine (t, tg, v, c) [pdx| target = ?tgt |] = (t, Just (Left tgt), v, c)
        addLine (t, tg, v, c) [pdx| cost_factor = !n |] = (t, tg, v, Just n)
        addLine acc [pdx| new_prioritised = %_ |] = acc
        addLine acc stmt = warn (UnknownSection "create_production_license" stmt) acc

        addEquip (t, tg, v, c) [pdx| type = $ty |] = (Just ty, tg, v, c)
        addEquip (t, tg, v, c) [pdx| version_name = ?vn |] = (t, tg, Just vn, c)
        addEquip acc [pdx| version = %_ |] = acc
        addEquip acc stmt = warn (UnknownSection "create_production_license@equipment" stmt) acc
createProductionLicense stmt = preStatement stmt

-- | Name whatever a military industrial organization tooltip points at. The token
-- may be reached through @mio:@; one read out of a variable names nothing we can
-- look up.
mioTooltip :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
mioTooltip msg stmt@[pdx| %_ = @scr |] =
    case mapMaybe named scr of
        (token : _) -> mioNamed msg stmt token
        [] -> preStatement stmt
    where
        named = \case
            [pdx| trait = ?token |] -> Just token
            [pdx| policy = ?token |] -> Just token
            [pdx| mio = ?token |] -> Just token
            _ -> Nothing
mioTooltip msg stmt@[pdx| %_ = ?token |] = mioNamed msg stmt token
mioTooltip _ stmt = preStatement stmt

mioNamed :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> GenericStatement -> Text -> PPT g m IndentedMessages
mioNamed msg stmt token
    | "var:" `T.isPrefixOf` token = preStatement stmt
    | otherwise = do
        names <- getMioNames
        -- Most tokens are localized under their own name; the rest are named by
        -- the key their entry in the organization files gives.
        mloc <- getGameL10nIfPresent bare
        nameloc <- case mloc of
            Just loc -> return (Just loc)
            Nothing -> traverse getGameL10n (HM.lookup bare names)
        case nameloc of
            Just loc -> msgToPP (msg (Doc.oneLine loc) bare)
            Nothing -> preStatement stmt
    where bare = fromMaybe token (T.stripPrefix "mio:" token)

-------------------------------------------------
-- Handlers for military industrial organizations --
-------------------------------------------------

-- | Handler for a @mio:@ scope, whose block holds whatever is being done to one
-- military industrial organization. The organization is named as a heading, with
-- what kind of manufacturer it is after the name, since the name alone rarely
-- says.
mioScope :: (HOI4Info g, Monad m) => StatementHandler g m
mioScope stmt = case stmt of
    Statement (GenericLhs _ [token]) _ (CompoundRhs scr) -> withCurrentIndent $ \_ -> do
        name <- mioName token
        kind <- mioKind token
        let named = maybe name (\k -> name <> " ('''" <> k <> "''')") kind
        header <- plainMsg' (named <> ":")
        scriptMsgs <- ppMany scr
        return (header : scriptMsgs)
    _ -> preStatement stmt

-- | Handler for @is_military_industrial_organization@, which asks whether the
-- organization in scope is the one named. The scope is usually reached through a
-- variable, so the name is the only thing that says which one is meant.
isMio :: (HOI4Info g, Monad m) => StatementHandler g m
isMio [pdx| %_ = $token |] = do
    name <- mioName token
    msgToPP (MsgIsMio name token)
isMio stmt = preStatement stmt
