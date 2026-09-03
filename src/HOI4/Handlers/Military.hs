{-|
Module      : HOI4.Handlers.Military
Description : Armies, navies and air forces

Handlers for the sizes of the armed forces, divisions and their
templates, ships, aces, and what moves or damages units.
-}
module HOI4.Handlers.Military (
        gainXp
    ,   hasArmySize
    ,   countryLockAllDivisionTemplate
    ,   changeDivisionTemplate
    ,   damageUnits
    ,   numDivisionsInStates
    ,   createRailwayGun
    ,   numPlanesStationedInRegions
    ,   addMines
    ,   setDivisionForceAllowRecruiting
    ,   addAce
    ,   hasNavySize
    ,   hasDeployedAirForceSize
    ,   divisionTemplate
    ,   createUnit
    ,   divisionsInState
    ,   deleteUnits
    ,   teleportArmies
    ,   transferNavy
    ,   shipsIn
    ,   navalStrengthComparison
    ,   setDivisionTemplateLock
    ,   clearDivisionTemplateCap
    ,   createShip
    ,   transferShip
    ,   addUnitsToDivisionTemplate
    ,   setDivisionTemplateCap
    ,   transferUnitsFraction
    ) where

import Data.Foldable (fold)
import Data.List (findIndex, foldl', partition)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative ((<|>))
import Control.Monad (foldM)

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (boldText, typewriterText, plainNum, plainPc, reducedNum)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, withCurrentIndent)
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (bareInt, joinClauses, msgToPP, preMessage, preStatement, statedFrom)

-- | Handler for @gain_xp@, which hands experience to whoever the surrounding
-- scope is about. The trigger of the same name, which asks about a combat, is a
-- block and is left to fall through.
gainXp :: (HOI4Info g, Monad m) => StatementHandler g m
gainXp [pdx| %_ = !amt |] = msgToPP $ MsgGainXp amt
gainXp stmt = preStatement stmt

hasArmySize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasArmySize stmt@[pdx| %_ = @scr |] = do
    let (_size, _) = extractStmt (matchLhsText "size") scr
        (_type, _) = extractStmt (matchLhsText "type") scr
    (comp, amt) <- case _size of
        Just [pdx| %_ < !num |] -> return ("less than", num)
        Just [pdx| %_ > !num |] -> return ("more than", num)
        _ -> return ("<!-- Check Script -->", 0)
    typed <- case _type of
        Just [pdx| %_ = $txt |] -> if txt == "anti_tank" then return " anti-tank" else return $ " " <> txt
        _ -> return " "
    msgToPP $ MsgHasArmySize comp amt typed
hasArmySize stmt = preStatement stmt

--------------------------------------------------------
-- handlers for the country-wide division template locks --
--------------------------------------------------------

countryLockAllDivisionTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
-- The reason the lock is given is a localization key the game shows in place of
-- the wording it would otherwise use, which says nothing of what changes.
countryLockAllDivisionTemplate [pdx| %_ = @scr |] =
    msgToPP $ MsgLockDivision (null [() | [pdx| is_locked = no |] <- scr])
countryLockAllDivisionTemplate [pdx| %_ = no |] = msgToPP $ MsgLockDivision False
countryLockAllDivisionTemplate [pdx| %_ = yes |] = msgToPP $ MsgLockDivision True
countryLockAllDivisionTemplate stmt = preStatement stmt

-- | Which template a division is built to. Script writes the name either bare
-- or in a field of a block.
changeDivisionTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
changeDivisionTemplate stmt@[pdx| %_ = @scr |] =
    case fst (extractStmt (matchLhsText "division_template") scr) of
        Just [pdx| %_ = ?tmpl |] -> msgToPP $ MsgChangeDivisionTemplate tmpl
        _ -> preStatement stmt
changeDivisionTemplate [pdx| %_ = ?tmpl |] = msgToPP $ MsgChangeDivisionTemplate tmpl
changeDivisionTemplate stmt = preStatement stmt

-------------------------------
-- handler for damage_units  --
-------------------------------

-- | Hurts the units somewhere without a word from the game about it. The
-- @limit@ is a condition on whoever owns them, so it keeps a block of its own
-- under the line saying what is damaged.
damageUnits :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
damageUnits [pdx| %_ = @scr |] = do
    let isField st = any (`matchLhsText` st)
            ["province", "state", "region", "damage", "org_damage", "str_damage"
            ,"ratio", "template", "army", "navy"]
        (fields, rest) = partition isField scr
        field k = listToMaybe [st | st <- fields, matchLhsText k st]
        saidYes k = not (null [() | [pdx| $lbl = yes |] <- fields, sameKey lbl k])
        amount k = case field k of
            Just [pdx| %_ = !n |] -> Just (n :: Double)
            _ -> Nothing
        -- A ratio is a share of what the unit has, anything else a flat amount.
        ratio = saidYes "ratio"
        showAmt n = boldText (Doc.doc2text (if ratio then reducedNum plainPc n else plainNum n))
        parts = [ showAmt n <> " of their " <> lbl
                | (k, lbl) <- [ ("damage", "organisation and strength")
                              , ("org_damage", "organisation")
                              , ("str_damage", "strength") ]
                , Just n <- [amount k] ]
        what | saidYes "army" && not (saidYes "navy") = "armies"
             | saidYes "navy" && not (saidYes "army") = "navies"
             | otherwise = "units"
    whereloc <- case (field "province", field "state", field "region") of
        (Just [pdx| %_ = !prov |], _, _) -> return $ "province " <> T.pack (show (prov :: Int))
        (_, Just st, _) -> statedFrom (Just st)
        (_, _, Just [pdx| %_ = !reg |]) -> getRegionLoc reg
        _ -> return ""
    header <- msgToPP $ MsgDamageUnits what whereloc
        (if null parts then "an unstated amount" else T.intercalate " and " parts)
    script_pp'd <- ppMany rest
    return (header ++ script_pp'd)
damageUnits stmt = preStatement stmt

-----------------------------------------
-- handler for num_divisions_in_states --
-----------------------------------------

numDivisionsInStates :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
numDivisionsInStates stmt@[pdx| %_ = @scr |] = do
    let (mcount, s1) = extractStmt (matchLhsText "count") scr
        (mstates, _) = extractStmt (matchLhsText "states") s1
    stateslocs <- case mstates of
        Just [pdx| %_ = @sts |] -> traverse getStateLoc (mapMaybe bareState sts)
        _ -> return []
    let whereloc = if null stateslocs then "the states named"
                   else T.intercalate ", " stateslocs
    case mcount of
        Just [pdx| %_ > !num |] -> msgToPP $ MsgNumDivisionsInStates "more than" num whereloc
        Just [pdx| %_ < !num |] -> msgToPP $ MsgNumDivisionsInStates "fewer than" num whereloc
        Just [pdx| %_ = !num |] -> msgToPP $ MsgNumDivisionsInStates "exactly" num whereloc
        _ -> preStatement stmt
    where
        bareState (StatementBare (IntLhs n)) = Just n
        bareState st = warn (UnknownSection "num_divisions_in_states array" st) Nothing
numDivisionsInStates stmt = preStatement stmt

------------------------------------
-- handler for create_railway_gun --
------------------------------------

createRailwayGun :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createRailwayGun stmt@[pdx| %_ = @scr |] = do
    let (mequip, rest) = extractStmt (matchLhsText "equipment") scr
        (mloc, _) = extractStmt (matchLhsText "location") rest
    case mequip of
        Just [pdx| %_ = ?equip |] -> do
            equiploc <- getGameL10n equip
            -- With nowhere named the gun is created in the country's capital,
            -- which the line need not spell out.
            whereloc <- case mloc of
                Just [pdx| %_ = !prov |] ->
                    return $ "province " <> T.pack (show (prov :: Int))
                _ -> return ""
            msgToPP $ MsgCreateRailwayGun equiploc whereloc
        _ -> preStatement stmt
createRailwayGun stmt = preStatement stmt

--------------------------------------------------
-- handler for num_planes_stationed_in_regions   --
--------------------------------------------------

numPlanesStationedInRegions :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
numPlanesStationedInRegions stmt@[pdx| %_ = @scr |] = do
    let (mvalue, rest) = extractStmt (matchLhsText "value") scr
        (mregions, _) = extractStmt (matchLhsText "regions") rest
    regionlocs <- case mregions of
        Just [pdx| %_ = @regs |] -> traverse getRegionLoc (mapMaybe bareInt regs)
        _ -> return []
    let whereloc = if null regionlocs then "the regions named"
                   else T.intercalate ", " regionlocs
    case mvalue of
        Just [pdx| %_ > !num |] -> msgToPP $ MsgNumPlanesStationedInRegions "more than" num whereloc
        Just [pdx| %_ < !num |] -> msgToPP $ MsgNumPlanesStationedInRegions "fewer than" num whereloc
        Just [pdx| %_ = !num |] -> msgToPP $ MsgNumPlanesStationedInRegions "exactly" num whereloc
        _ -> preStatement stmt
numPlanesStationedInRegions stmt = preStatement stmt

----------------------------
-- handler for add_mines  --
----------------------------

-- | Mines laid in a strategic region, which is named by its id.
addMines :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addMines stmt@[pdx| %_ = @scr |] = do
    let (mregion, rest) = extractStmt (matchLhsText "region") scr
        (mamount, _) = extractStmt (matchLhsText "amount") rest
    whereloc <- case mregion of
        Just [pdx| %_ = !num |] -> getRegionLoc num
        Just [pdx| %_ = ?var |] -> return (typewriterText var)
        _ -> return "<!-- Check Script -->"
    case mamount of
        Just [pdx| %_ = !num |] -> msgToPP $ MsgAddMines "" whereloc num
        Just [pdx| %_ = ?var |] -> msgToPP $ MsgAddMinesVar "" whereloc (typewriterText var)
        _ -> preStatement stmt
addMines stmt = preStatement stmt

-------------------------------------------------------
-- handler for set_division_force_allow_recruiting    --
-------------------------------------------------------

setDivisionForceAllowRecruiting :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setDivisionForceAllowRecruiting stmt@[pdx| %_ = @scr |] = do
    let (mtemplate, rest) = extractStmt (matchLhsText "division_template") scr
        allowed = null [() | [pdx| force_allow_recruiting = no |] <- rest]
    case mtemplate of
        Just [pdx| %_ = ?tmpl |] -> msgToPP $ MsgSetDivisionForceAllowRecruiting tmpl allowed
        _ -> preStatement stmt
setDivisionForceAllowRecruiting stmt = preStatement stmt

--------------------------------------
-- Handler for add_ace --
--------------------------------------

data AddAce = AddAce
        {   aa_name :: Text
        ,   aa_surname :: Text
        ,   aa_callsign :: Text
        }

newAA :: AddAce
newAA = AddAce undefined undefined undefined

addAce  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addAce stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppAa =<< foldM addLine newAA scr
    where
        addLine :: AddAce -> GenericStatement -> PPT g m AddAce
        addLine aa [pdx| name = ?txt |] = return aa { aa_name = txt }
        addLine aa [pdx| surname = ?txt |] = return aa { aa_surname = txt }
        addLine aa [pdx| callsign = ?txt |] = return aa { aa_callsign = txt }
        addLine aa [pdx| type = ?_ |] = return aa
        addLine aa [pdx| is_female = ?_ |] = return aa
        addLine aa stmt = warn (UnknownSection "add_ace" stmt) $ return aa
        ppAa aa = do
            return $ MsgAddAce (aa_name aa) (aa_callsign aa) (aa_surname aa)
addAce stmt = preStatement stmt

-------------------------------
-- Handler for has_navy_size --
------------------------------

data NavySize = NavySize
        {   ns_size :: Maybe Double
        ,   ns_sizevar :: Maybe Text
        ,   ns_comp :: Text
        ,   ns_type :: Maybe Text
        ,   ns_archetype :: Maybe Text
        }

newNS :: NavySize
newNS = NavySize Nothing Nothing undefined Nothing Nothing

hasNavySize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasNavySize stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppNS =<< foldM addLine newNS scr
    where
        addLine :: NavySize -> GenericStatement -> PPT g m NavySize
        addLine ns [pdx| size < !num |] = return ns { ns_comp = "less than", ns_size = Just num}
        addLine ns [pdx| size > !num |] = return ns { ns_comp = "more than", ns_size = Just num }
        addLine ns [pdx| size < $num |] = return ns { ns_comp = "less than", ns_sizevar = Just num}
        addLine ns [pdx| size > $num |] = return ns { ns_comp = "more than", ns_sizevar = Just num }
        addLine ns [pdx| type = $txt |] = return ns { ns_type = Just txt }
        addLine ns [pdx| unit = $txt |] = return ns { ns_type = Just txt }
        addLine ns [pdx| archetype = ?txt |] = return ns { ns_archetype = Just txt }
        addLine ns stmt = warn (UnknownSection "has_navy_size" stmt) $ return ns
        ppNS ns = do
            typed <- case(ns_type ns, ns_archetype ns) of
                (Just txt, _) -> getGameL10n txt
                (_, Just txt) -> getGameL10n txt
                _ -> return ""
            case (ns_size ns, ns_sizevar ns) of
                (Just amt, _) -> return $ MsgHasNavySize (ns_comp ns) amt typed
                (_, Just amt) -> return $ MsgHasNavySizeVar (ns_comp ns) amt typed
                _ -> return $ preMessage stmt
hasNavySize stmt = preStatement stmt

----------------------------------------------
-- Handler for has_deployed_air_force_size --
----------------------------------------------

data AirForceSize = AirForceSize
        {   afs_size :: Maybe Double
        ,   afs_sizevar :: Maybe Text
        ,   afs_comp :: Text
        ,   afs_type :: Maybe Text
        }

newAFS :: AirForceSize
newAFS = AirForceSize Nothing Nothing "least" Nothing

-- | Handler for @has_deployed_air_force_size@, which asks after the aircraft a
-- country has in the air. The game reads the comparison as a bound rather than
-- a strict one -- at least so many, at most so many -- and names the kind of
-- aircraft where the trigger is given one.
hasDeployedAirForceSize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasDeployedAirForceSize stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppAFS =<< foldM addLine newAFS scr
    where
        addLine :: AirForceSize -> GenericStatement -> PPT g m AirForceSize
        addLine afs [pdx| size < !num |] = return afs { afs_comp = "most", afs_size = Just num }
        addLine afs [pdx| size > !num |] = return afs { afs_comp = "least", afs_size = Just num }
        addLine afs [pdx| size = !num |] = return afs { afs_comp = "least", afs_size = Just num }
        addLine afs [pdx| size < $num |] = return afs { afs_comp = "most", afs_sizevar = Just num }
        addLine afs [pdx| size > $num |] = return afs { afs_comp = "least", afs_sizevar = Just num }
        addLine afs [pdx| size = $num |] = return afs { afs_comp = "least", afs_sizevar = Just num }
        addLine afs [pdx| type = ?txt |] = return afs { afs_type = Just txt }
        addLine afs stmt = warn (UnknownSection "has_deployed_air_force_size" stmt) $ return afs
        ppAFS afs = do
            typed <- maybe (return "") getGameL10n (afs_type afs)
            case (afs_size afs, afs_sizevar afs) of
                (Just amt, _) -> return $ MsgHasDeployedAirForceSize (afs_comp afs) amt typed
                (_, Just amt) -> return $ MsgHasDeployedAirForceSizeVar (afs_comp afs) amt typed
                _ -> return $ preMessage stmt
hasDeployedAirForceSize stmt = preStatement stmt

-----------------------------------
-- Handler for division_template --
-----------------------------------

divisionTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
divisionTemplate stmt@[pdx| %_ = @scr |] = do
    let (mname, _) = extractStmt (matchLhsText "name") scr
    case mname of
        Just [pdx| %_ = ?txt |] -> msgToPP $ MsgDivisionTemplate txt
        _ -> preStatement stmt
divisionTemplate  stmt = preStatement stmt

-----------------------------
-- Handler for create_unit --
-----------------------------

createUnit :: (Monad m, HOI4Info g) => StatementHandler g m
createUnit stmt@[pdx| %_ = @scr |] = do
    let (division,rest) = extractStmt (matchLhsText "division") scr
        (owner, _) = extractStmt (matchLhsText "owner") rest
    divis <- case division of
        Just [pdx| %_ = ?txt |] -> do
            let divtempi = fromMaybe 0 $ findString "division_template" txt
                divtemps = T.drop 1 $ T.dropWhile (/= '\"') (T.drop divtempi txt)
            return $ T.dropEnd 1 (T.dropWhileEnd (/= '\"') divtemps)
        _ -> return "<!-- Check game Script -->"
    owner <- case owner of
        Just [pdx| %_ = ?txt |] -> flagText (Just HOI4Country) txt
        _ -> return "<!-- Check game Script -->"
    msgToPP $ MsgCreateUnit owner divis
    where
    findString :: Text -> Text -> Maybe Int
    findString search str = findIndex (T.isPrefixOf search) (T.tails str)
createUnit stmt = preStatement stmt

------------------------------------
-- handler for divisions_in_state --
------------------------------------

data DivisionsInState = DivisionsInState
        {   ds_size :: Double
        ,   ds_comp :: Text
        ,   ds_type :: Maybe Text
        ,   ds_state :: Maybe Int
        ,   ds_statevar :: Maybe (Either Text (Text, Text))
        ,   ds_border_state :: Maybe Int
        ,   ds_border_statevar :: Maybe (Either Text (Text, Text))
        }

newDS :: DivisionsInState
newDS = DivisionsInState undefined undefined Nothing Nothing Nothing Nothing Nothing

divisionsInState :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Double -> Text -> Text -> Text -> ScriptMessage) -> StatementHandler g m
divisionsInState msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppDS =<< foldM addLine newDS scr
    where
        addLine :: DivisionsInState -> GenericStatement -> PPT g m DivisionsInState
        addLine ds [pdx| size > !num |] = return ds { ds_comp = "more than", ds_size = num }
        addLine ds [pdx| size < !num |] = return ds { ds_comp = "less than", ds_size = num }
        addLine ds [pdx| amount > !num |] = return ds { ds_comp = "more than", ds_size = num }
        addLine ds [pdx| amount < !num |] = return ds { ds_comp = "less than", ds_size = num }
        addLine ds [pdx| type = $txt |] = return ds { ds_type = Just txt }
        -- Script names the division kind under either label.
        addLine ds [pdx| unit = $txt |] = return ds { ds_type = Just txt }
        addLine ds [pdx| state = !num |] = return ds {ds_state = Just num}
        addLine ds [pdx| state = $vartag:$var |] = return ds { ds_statevar = Just (Right (vartag,var))}
        addLine ds [pdx| state = $var |] = return ds { ds_statevar = Just (Left var)}
        addLine ds [pdx| border_state = !num |] = return ds { ds_border_state = Just num}
        addLine ds [pdx| border_state = $vartag:$var |] = return ds { ds_border_statevar = Just (Right (vartag,var))}
        addLine ds [pdx| border_state = $var |] = return ds { ds_border_statevar = Just (Left var)}
        addLine ds stmt = warn (UnknownSection "divisionsInState" stmt) $ return ds
        ppDS :: DivisionsInState -> PPT g m ScriptMessage
        ppDS ds = do
            borderstate <- case (ds_border_state ds, ds_border_statevar ds) of
                (Just state,_) -> getStateLoc state
                (_,Just estate) -> eGetStateText estate
                _-> return "<!-- Check Script -->"
            typeloc <- maybe (return "") getGameL10n (ds_type ds)
            stateloc <- case (ds_state ds, ds_statevar ds) of
                (Just state,_) -> getStateLoc state
                (_,Just (Left state)) -> do
                    mstate <- eGetState (Left state)
                    return $ fromMaybe "" mstate
                (_,Just (Right state)) -> do
                    mstate <- eGetState (Right state)
                    return $ fromMaybe "" mstate
                _-> return ""
            return $ msg (ds_comp ds) (ds_size ds) typeloc stateloc borderstate
divisionsInState _ stmt = preStatement stmt

-------------------------------------------------------
-- handler for delete_units and delete_unit_template --
-------------------------------------------------------

data DeleteUnits = DeleteUnits
        {   du_division_template :: Maybe Text
        ,   du_disband :: Bool
        ,   du_state :: Maybe Text
        ,   du_id :: Maybe Text
        }

newDU :: DeleteUnits
newDU = DeleteUnits Nothing False Nothing Nothing

deleteUnits :: forall g m. (HOI4Info g, Monad m) =>
    (Bool -> Text -> Text -> ScriptMessage) -> StatementHandler g m
deleteUnits msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppDU =<< foldM addLine newDU scr
    where
        addLine :: DeleteUnits -> GenericStatement -> PPT g m DeleteUnits
        addLine du [pdx| division_template = ?txt |] = return du { du_division_template = Just txt }
        addLine du [pdx| disband = %rhs |]
            | GenericRhs "yes" [] <- rhs = return du { du_disband = True }
            | otherwise = return du
        addLine du [pdx| state = !num |] = do
            stateloc <- getStateLoc num
            return du { du_state = Just stateloc }
        -- The state may also be a pronoun or a variable naming one.
        addLine du [pdx| state = $txt |] = do
            stateloc <- eGetStateText (Left txt)
            return du { du_state = Just stateloc }
        -- A single division picked out by id, which script keeps in a
        -- variable (the game gives ids out at run time).
        addLine du [pdx| id = $var |] = return du { du_id = Just var }
        addLine du stmt = warn (UnknownSection "deleteUnits" stmt) $ return du
        ppDU :: DeleteUnits -> PPT g m ScriptMessage
        ppDU du = case du_id du of
            Just divid -> return $ MsgDeleteUnitById (du_disband du) (typewriterText divid)
            _ -> return $ msg (du_disband du) (fromMaybe "" (du_division_template du)) (fromMaybe "" (du_state du))
deleteUnits _ stmt = preStatement stmt

-------------------------------------
-- handler for teleport_armies     --
-------------------------------------

-- | Moves the armies in a state elsewhere. The @limit@ is a condition on
-- whoever owns them rather than a narrowing of the destination, so it keeps a
-- block of its own under the line saying where they go.
teleportArmies :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
teleportArmies [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let isTo st = any (`matchLhsText` st) ["to_state", "to_state_array", "to_province"]
        -- Script sometimes names a state and a province both. The first named
        -- is where they go; the rest is the game's own fallback and says
        -- nothing more.
        (tos, rest) = partition isTo scr
        mto = listToMaybe tos
    header <- case mto of
        Just [pdx| to_state_array = $vartag:$var |] -> return $ MsgTeleportArmiesArray (vartag <> "." <> var)
        Just [pdx| to_state_array = $arr |] -> return $ MsgTeleportArmiesArray arr
        Just [pdx| to_province = !num |] -> return $ MsgTeleportArmiesProvince num
        Just to@[pdx| to_state = %_ |] -> MsgTeleportArmies <$> statedFrom (Just to)
        -- With nowhere named the game sends them to their owner's capital.
        _ -> return MsgTeleportArmiesCapital
    script_pp'd <- ppMany rest
    return ((i, header) : script_pp'd)
teleportArmies stmt = preStatement stmt

-------------------------------------
-- handler for transfer_navy       --
-------------------------------------

transferNavy :: (HOI4Info g, Monad m) => StatementHandler g m
transferNavy stmt@[pdx| %_ = @scr |] = do
    let (mtarget, rest) = extractStmt (matchLhsText "target") scr
        exile = not (null [() | [pdx| is_government_in_exile = yes |] <- rest])
    case mtarget of
        Just [pdx| %_ = $tag |] -> do
            who <- flagText (Just HOI4Country) tag
            msgToPP $ MsgTransferNavy who exile
        _ -> preStatement stmt
transferNavy stmt = preStatement stmt

-------------------------------------------------------
-- handler for ships_in_area and ships_in_state_ports --
-------------------------------------------------------

data ShipsIn = ShipsIn
        {   si_size :: Double
        ,   si_comp :: Text
        ,   si_type :: Maybe Text
        ,   si_where :: Maybe Text
        }

newSI :: ShipsIn
newSI = ShipsIn 0 "at least" Nothing Nothing

-- | How many ships of a kind the country has somewhere: a strategic region for
-- @ships_in_area@, a state's ports for @ships_in_state_ports@. The two are
-- written alike but for the field naming the place.
shipsIn :: forall g m. (HOI4Info g, Monad m) =>
    Text -- ^ Label of the field naming the place
        -> (Int -> PPT g m Text) -- ^ How to name the place from its id
        -> (Text -> Double -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
shipsIn wherelabel wherelocof msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppSI =<< foldM addLine newSI scr
    where
        addLine :: ShipsIn -> GenericStatement -> PPT g m ShipsIn
        addLine si [pdx| size > !num |] = return si { si_comp = "more than", si_size = num }
        addLine si [pdx| size < !num |] = return si { si_comp = "less than", si_size = num }
        addLine si [pdx| size = !num |] = return si { si_comp = "at least", si_size = num }
        addLine si [pdx| type = $txt |] = return si { si_type = Just txt }
        addLine si st@[pdx| $label = %_ |] | sameKey label wherelabel = case st of
            [pdx| %_ = !num |] -> do
                whereloc <- wherelocof num
                return si { si_where = Just whereloc }
            _ -> do
                whereloc <- statedFrom (Just st)
                return si { si_where = Just whereloc }
        addLine si st = warn (UnknownSection "shipsIn" st) $ return si
        ppSI :: ShipsIn -> PPT g m ScriptMessage
        ppSI si = do
            -- With no kind named the trigger counts every ship there is.
            typeloc <- maybe (return "ships") getGameL10n (si_type si)
            return $ msg (si_comp si) (si_size si) typeloc
                         (fromMaybe "<!-- Check Script -->" (si_where si))
shipsIn _ _ _ stmt = preStatement stmt

-------------------------------------------
-- handler for is_power_balance_in_range --
-------------------------------------------

data NavalStrengthComparison = NavalStrengthComparison
        {   nsc_other :: Text
        ,   nsc_ratio :: Double
        ,   nsc_comp :: Text
        ,   nsc_sub_unit_def_weights :: [(Text,Double)]
        }

newNSC :: NavalStrengthComparison
newNSC = NavalStrengthComparison "<!--Check Script-->" 0 "<!--Check Script-->" []

navalStrengthComparison :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
navalStrengthComparison stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppNSC =<< foldM addLine newNSC scr
    where
        addLine :: NavalStrengthComparison -> GenericStatement -> PPT g m NavalStrengthComparison
        addLine nsc [pdx| other = ?txt |] = return nsc { nsc_other = txt }
        addLine nsc [pdx| ratio > !amt |] = return nsc { nsc_ratio = amt, nsc_comp = "more" }--right
        addLine nsc [pdx| ratio < !amt |] = return nsc { nsc_ratio = amt, nsc_comp = "less" }--left
        addLine nsc [pdx| sub_unit_def_weights = @scr |] = return $ foldl' addLine' nsc scr
        addLine nsc stmt = warn (UnknownSection "navalStrengthComparison" stmt) $ return nsc
        addLine' :: NavalStrengthComparison -> GenericStatement -> NavalStrengthComparison
        addLine' nsc [pdx| $shiptype = !weight |] = do
            let oldweight = nsc_sub_unit_def_weights nsc
            nsc { nsc_sub_unit_def_weights = oldweight ++ [(shiptype, weight)]}
        addLine' nsc stmt = warn (UnknownSection "navalStrengthComparison weights" stmt) nsc

        ppNSC :: NavalStrengthComparison -> PPT g m ScriptMessage
        ppNSC nsc = do
            otherflag <- flagText (Just HOI4Country) (nsc_other nsc)
            weighttext <- case nsc_sub_unit_def_weights nsc of
                [] -> return ""
                weights -> do
                    weightype <- mapM (\wt -> do
                        let (shiptype, weight) = wt
                        shiploc <- getGameL10n shiptype
                        let weighttxt = Doc.doc2text (plainNum weight)
                        return $ weighttxt <> " for " <> shiploc)
                        weights
                    return $ " with weight " <> T.intercalate ", " weightype
            return $ MsgavalStrengthComparison (nsc_ratio nsc) (nsc_comp nsc) otherflag weighttext
navalStrengthComparison stmt = preStatement stmt

-- | Handler for @set_division_template_lock@, which stops a template being edited
-- and its divisions being trained or disbanded, or lets them be again.
setDivisionTemplateLock :: (HOI4Info g, Monad m) => StatementHandler g m
setDivisionTemplateLock stmt@[pdx| %_ = @scr |] = case (template, locked) of
    (Just name, Just yn) -> msgToPP (MsgSetDivisionTemplateLock name yn)
    _ -> preStatement stmt
    where
        (template, locked) = foldl' addLine (Nothing, Nothing) scr
        addLine (t, l) [pdx| division_template = ?name |] = (Just name, l)
        addLine (t, l) [pdx| is_locked = yes |] = (t, Just True)
        addLine (t, l) [pdx| is_locked = no |] = (t, Just False)
        addLine acc stmt = warn (UnknownSection "set_division_template_lock" stmt) acc
setDivisionTemplateLock stmt = preStatement stmt

-- | Handler for @clear_division_template_cap@, which lifts the limit on how many
-- divisions of a template the country may hold.
clearDivisionTemplateCap :: (HOI4Info g, Monad m) => StatementHandler g m
clearDivisionTemplateCap stmt@[pdx| %_ = @scr |] =
    case [name | [pdx| division_template = ?name |] <- scr] of
        (name : _) -> msgToPP (MsgClearDivisionTemplateCap name)
        [] -> preStatement stmt
clearDivisionTemplateCap stmt = preStatement stmt

----------------------------
-- Handler for create_ship --
----------------------------
data CreateShip = CreateShip
        {   cs_type :: Maybe Text
        ,   cs_variant :: Maybe Text
        ,   cs_name :: Maybe Text
        ,   cs_creator :: Maybe (Either Text (Text, Text))
        ,   cs_amount :: Double
        }

newCS :: CreateShip
newCS = CreateShip Nothing Nothing Nothing Nothing 1

-- | Handler for @create_ship@, which puts a ship straight into the reserve
-- fleet. @creator@ names whose design it is built to, which is what decides
-- what the ship can do; left out, it is the country's own.
createShip :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createShip stmt@[pdx| %_ = @scr |] = case cs_type cs of
        Just shiptype -> do
            shiploc <- T.strip <$> getGameL10n shiptype
            creator <- case cs_creator cs of
                Just who -> fromMaybe "" <$> eflag (Just HOI4Country) who
                Nothing -> return ""
            msgToPP $ MsgCreateShip (cs_amount cs) shiploc
                        (fromMaybe "" (cs_variant cs)) (fromMaybe "" (cs_name cs)) creator
        Nothing -> preStatement stmt
    where
        cs = foldl' addLine newCS scr
        addLine c [pdx| type = $t |] = c { cs_type = Just t }
        addLine c [pdx| equipment_variant = ?v |] = c { cs_variant = Just v }
        addLine c [pdx| name = ?n |] = c { cs_name = Just n }
        addLine c [pdx| amount = !n |] = c { cs_amount = n }
        addLine c [pdx| creator = $vartag:$var |] = c { cs_creator = Just (Right (vartag, var)) }
        addLine c [pdx| creator = ?t |] = c { cs_creator = Just (Left t) }
        -- Whether a ship already in for a refit may be picked. Says nothing
        -- about what is created.
        addLine c [pdx| exclude_refitting = %_ |] = c
        addLine c stmt = warn (UnknownSection "create_ship" stmt) c
createShip stmt = preStatement stmt

------------------------------
-- Handler for transfer_ship --
------------------------------
-- | Handler for @transfer_ship@, which hands one ship of a kind over to another
-- country. @prefer_name@ picks which one, where the fleet has a ship by that
-- name to give.
transferShip :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
transferShip stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing) scr of
        (Just shiptype, target, name) -> do
            shiploc <- T.strip <$> getGameL10n shiptype
            targetloc <- case target of
                Just who -> fromMaybe "" <$> eflag (Just HOI4Country) who
                Nothing -> return ""
            msgToPP $ MsgTransferShip shiploc (fromMaybe "" name) targetloc
        _ -> preStatement stmt
    where
        addLine (t, tg, n) [pdx| type = $ty |] = (Just ty, tg, n)
        addLine (t, tg, n) [pdx| target = $vartag:$var |] = (t, Just (Right (vartag, var)), n)
        addLine (t, tg, n) [pdx| target = ?tgt |] = (t, Just (Left tgt), n)
        addLine (t, tg, n) [pdx| prefer_name = ?nm |] = (t, tg, Just nm)
        addLine acc [pdx| exclude_refitting = %_ |] = acc
        addLine acc stmt = warn (UnknownSection "transfer_ship" stmt) acc
transferShip stmt = preStatement stmt

----------------------------------------------
-- Handler for add_units_to_division_template --
----------------------------------------------
-- | Handler for @add_units_to_division_template@, which adds battalions or
-- support companies to a template, and so to every division already built to it.
addUnitsToDivisionTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addUnitsToDivisionTemplate stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, []) scr of
        (Just name, units@(_:_)) -> do
            unitlocs <- traverse getGameL10n units
            msgToPP (MsgAddUnitsToDivisionTemplate name (joinClauses unitlocs))
        _ -> preStatement stmt
    where
        addLine (n, us) [pdx| template_name = ?nm |] = (Just nm, us)
        addLine (n, us) [pdx| regiments = @rscr |] = (n, us ++ unitNames rscr)
        addLine (n, us) [pdx| support = @rscr |] = (n, us ++ unitNames rscr)
        addLine (n, us) [pdx| regimental_support = @rscr |] = (n, us ++ unitNames rscr)
        addLine acc stmt = warn (UnknownSection "add_units_to_division_template" stmt) acc
        -- The number beside each unit is the column it goes in, not how many of
        -- it to add, so there is nothing in it worth reading on the wiki.
        unitNames uscr = [unit | [pdx| $unit = %_ |] <- uscr]
addUnitsToDivisionTemplate stmt = preStatement stmt

------------------------------------------
-- Handler for set_division_template_cap --
------------------------------------------
-- | Handler for @set_division_template_cap@, which limits how many divisions of
-- a template the country may hold. The cap is often a script constant rather
-- than a number written out.
setDivisionTemplateCap :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setDivisionTemplateCap stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing) scr of
        (Just name, mcap, mcapvar) -> do
            constant <- maybe (return Nothing) constantValue mcapvar
            case (mcap, constant, mcapvar) of
                (Just cap, _, _) -> msgToPP (MsgSetDivisionTemplateCap name cap)
                (_, Just cap, _) -> msgToPP (MsgSetDivisionTemplateCap name cap)
                (_, _, Just capvar) -> msgToPP (MsgSetDivisionTemplateCapVar name capvar)
                _ -> msgToPP (MsgSetDivisionTemplateCap name 1)
        _ -> preStatement stmt
    where
        addLine (n, c, cv) [pdx| division_template = ?nm |] = (Just nm, c, cv)
        addLine (n, c, cv) [pdx| division_cap = !amt |] = (n, Just amt, cv)
        addLine (n, c, cv) [pdx| division_cap = $var |] = (n, c, Just var)
        addLine acc stmt = warn (UnknownSection "set_division_template_cap" stmt) acc
setDivisionTemplateCap stmt = preStatement stmt

-------------------------------------
-- Handler for transfer_units_fraction --
-------------------------------------
data TransferUnits = TransferUnits
        {   tu_target :: Maybe (Either Text (Text, Text))
        ,   tu_size :: Maybe Double
        ,   tu_army :: Maybe Double
        ,   tu_navy :: Maybe Double
        ,   tu_air :: Maybe Double
        ,   tu_stockpile :: Maybe Double
        ,   tu_keep_leaders :: Bool
        }

newTU :: TransferUnits
newTU = TransferUnits Nothing Nothing Nothing Nothing Nothing Nothing False

-- | Handler for @transfer_units_fraction@, which hands part of the country's
-- forces to another. @size@ sets the share for everything not given a share of
-- its own, so each arm is written out with whichever of the two applies to it.
transferUnitsFraction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
transferUnitsFraction stmt@[pdx| %_ = @scr |] = case tu_target tu of
        Just target -> do
            targetloc <- fromMaybe "" <$> eflag (Just HOI4Country) target
            basemsg <- msgToPP (MsgTransferUnitsFraction targetloc (tu_keep_leaders tu))
            shares <- indentUp (fold <$> traverse share
                [ ("army", tu_army tu), ("navy", tu_navy tu), ("air force", tu_air tu)
                , ("stockpile", tu_stockpile tu) ])
            return (basemsg ++ shares)
        Nothing -> preStatement stmt
    where
        tu = foldl' addLine newTU scr
        addLine t [pdx| target = $vartag:$var |] = t { tu_target = Just (Right (vartag, var)) }
        addLine t [pdx| target = ?tgt |] = t { tu_target = Just (Left tgt) }
        addLine t [pdx| size = !n |] = t { tu_size = Just n }
        addLine t [pdx| army_ratio = !n |] = t { tu_army = Just n }
        addLine t [pdx| navy_ratio = !n |] = t { tu_navy = Just n }
        addLine t [pdx| air_ratio = !n |] = t { tu_air = Just n }
        addLine t [pdx| stockpile_ratio = !n |] = t { tu_stockpile = Just n }
        addLine t [pdx| keep_unit_leaders = yes |] = t { tu_keep_leaders = True }
        addLine t [pdx| keep_unit_leaders = no |] = t { tu_keep_leaders = False }
        -- The block form lists ids of the leaders to keep. Script only ever
        -- writes it empty here, which keeps no one, same as the default; a
        -- list with ids in it means some leaders do stay behind.
        addLine t [pdx| keep_unit_leaders = @ids |]
            = if null ids then t else t { tu_keep_leaders = True }
        -- Which leaders go along, and which organizations they are moved
        -- between; neither says anything about how much is handed over.
        addLine t [pdx| keep_unit_leaders_trigger = %_ |] = t
        addLine t [pdx| source_organization = %_ |] = t
        addLine t [pdx| target_organization = %_ |] = t
        addLine t stmt = warn (UnknownSection "transfer_units_fraction" stmt) t

        share (what, mratio) = case mratio <|> tu_size tu of
            Just ratio -> msgToPP (MsgTransferUnitsShare what ratio)
            Nothing -> return []
transferUnitsFraction stmt = preStatement stmt
