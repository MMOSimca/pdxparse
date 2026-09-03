{-|
Module      : HOI4.Handlers.States
Description : States and provinces

Handlers for buildings and their levels, railways, victory points,
province names, and who holds a state's provinces.
-}
module HOI4.Handlers.States (
        buildingLevel
    ,   constructBuildingInRandomProvince
    ,   buildingTypeLevel
    ,   victoryPoints
    ,   setProvinceName
    ,   addBuildingConstruction
    ,   setBuildingLevel
    ,   freeBuildingSlots
    ,   buildRailway
    ,   canBuildRailway
    ,   hasRailwayConnection
    ,   setStateProvinceController
    ,   damageBuilding
    ,   addProvinceModifier
    ,   anyProvinceBuildingLevel
    ) where

import Data.List (foldl', intercalate)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (foldM)

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (iquotes, typewriterText, colourPc, reducedNum)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT)
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (msgToPP, preMessage, preStatement)
import HOI4.Handlers.Generic (numericCompare, parseTV, parseVV, TextValue (..), ValueValue (..))

-- | Handler for a building's own name used as a trigger, which compares how many
-- levels of the building the state has.
buildingLevel :: (HOI4Info g, Monad m) => Text -> StatementHandler g m
buildingLevel building = numericCompare "more than" "fewer than"
    (MsgBuildingLevel (iconText building)) (MsgBuildingLevelVar (iconText building))

-- | Handler for @construct_building_in_random_province@, whose block names the
-- building to put up and how many levels of it.
constructBuildingInRandomProvince :: (HOI4Info g, Monad m) => StatementHandler g m
constructBuildingInRandomProvince stmt@[pdx| %_ = @scr |] = case scr of
    [[pdx| $building = !level |]] ->
        msgToPP $ MsgConstructBuildingInRandomProvince (iconText building) level
    _ -> preStatement stmt
constructBuildingInRandomProvince stmt = preStatement stmt

-- | Handler for the @type@/@level@ building effects that may carry a little
-- more: @remove_building@ names the province a landmark stands in, and a few
-- @add_offsite_building@ scripts write an @instant_build@ flag. The flag
-- changes nothing the reader sees -- an offsite building appears at once
-- either way -- and the province goes into the message where one is given.
buildingTypeLevel :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Double -> Text -> ScriptMessage) -- ^ Message constructor for a number
        -> (Text -> Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor for a variable
        -> StatementHandler g m
buildingTypeLevel valmsg varmsg stmt@[pdx| %_ = @scr |] = do
    let sift (mprov, rest) = \case
            [pdx| province = !num |] -> (Just num, rest)
            [pdx| instant_build = %_ |] -> (mprov, rest)
            line -> (mprov, rest ++ [line])
        (mprov, scr') = foldl' sift (Nothing, []) scr
    provloc <- maybe (return "") getProvinceLoc mprov
    let tv = parseTV "type" "level" scr'
    case (tv_what tv, tv_value tv, tv_var tv) of
        (Just what, Just value, _) -> do
            (icon, loc) <- tryLocAndIcon what
            msgToPP $ valmsg icon loc value provloc
        (Just what, _, Just var) -> do
            (icon, loc) <- tryLocAndIcon what
            msgToPP $ varmsg icon loc var provloc
        _ -> preStatement stmt
buildingTypeLevel _ _ stmt = preStatement stmt

-- | Handler for @set_victory_points@ and @add_victory_points@, which write a
-- province and an amount. The province is named by its victory point, which
-- for these two nearly always exists (they are what put it there).
victoryPoints :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Double -> ScriptMessage) -- ^ Message constructor for a number
        -> (Text -> Text -> ScriptMessage) -- ^ Message constructor for a variable
        -> StatementHandler g m
victoryPoints valmsg varmsg stmt@[pdx| %_ = @scr |] = do
    let vv = parseVV "province" "value" scr
    case vv_what vv of
        Just prov -> do
            provloc <- getProvinceLoc (round prov)
            case (vv_value vv, vv_var vv) of
                (Just value, _) -> msgToPP $ valmsg provloc value
                (_, Just var) -> msgToPP $ varmsg provloc var
                _ -> preStatement stmt
        _ -> preStatement stmt
victoryPoints _ _ stmt = preStatement stmt

-- | Handler for @set_province_name@, which renames a province -- nearly always
-- one with a victory point, whose current name the message shows.
setProvinceName :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setProvinceName stmt@[pdx| %_ = @scr |] = do
    let tv = parseTV "name" "id" scr
    case tv_what tv of
        Just name -> do
            (_, nameloc) <- tryLocMaybe name
            case (tv_value tv, tv_var tv) of
                (Just provid, _) -> do
                    provloc <- getProvinceLoc (round provid)
                    msgToPP $ MsgSetProvinceName "" nameloc provloc
                (_, Just var) -> msgToPP $ MsgSetProvinceNameVar "" nameloc var
                _ -> preStatement stmt
        _ -> preStatement stmt
setProvinceName stmt = preStatement stmt

-------------------------------------------
-- Handler for add_building_construction --
-------------------------------------------

-- | Which provinces of a state an effect touches: the body of the @province@
-- block that @add_building_construction@, @set_building_level@ and
-- @add_province_modifier@ all share.
data ProvSel = ProvSel
    { ps_ids :: Maybe [Double]
    , ps_all :: Bool
    , ps_coastal :: Bool
    , ps_naval :: Bool
    , ps_border :: Bool
    , ps_border_country :: Maybe (Either Text (Text, Text))
    , ps_supply_node :: Bool
    , ps_vp :: Bool
    , ps_vp_comp :: Text
    , ps_vp_num :: Maybe Double
    , ps_level_comp :: Text
    , ps_level :: Maybe Double
    } deriving Show

newProvSel :: ProvSel
newProvSel = ProvSel Nothing False False False False Nothing False False "" Nothing "" Nothing

-- | One line of a @province@ block.
provSelLine :: String -> ProvSel -> GenericStatement -> ProvSel
provSelLine what ps stmt = case stmt of
    [pdx| all_provinces = %rhs |] -> ifYes rhs ps { ps_all = True }
    [pdx| limit_to_coastal = %rhs |] -> ifYes rhs ps { ps_coastal = True }
    [pdx| limit_to_naval_base = %rhs |] -> ifYes rhs ps { ps_naval = True }
    [pdx| limit_to_border = %rhs |] -> ifYes rhs ps { ps_border = True }
    [pdx| limit_to_supply_node = %rhs |] -> ifYes rhs ps { ps_supply_node = True }
    [pdx| limit_to_border_country = $txt |] -> ps { ps_border_country = Just (Left txt) }
    [pdx| limit_to_border_country = $vartag:$var |] -> ps { ps_border_country = Just (Right (vartag, var)) }
    [pdx| id = !num |] -> ps { ps_ids = Just (fromMaybe [] (ps_ids ps) ++ [num]) }
    [pdx| limit_to_victory_point > !num |] -> ps { ps_vp_comp = "higher than", ps_vp_num = Just num }
    [pdx| limit_to_victory_point < !num |] -> ps { ps_vp_comp = "lower than", ps_vp_num = Just num }
    [pdx| limit_to_victory_point = %rhs |] -> ifYes rhs ps { ps_vp = True }
    [pdx| level > !num |] -> ps { ps_level_comp = "higher than", ps_level = Just num }
    [pdx| level < !num |] -> ps { ps_level_comp = "lower than", ps_level = Just num }
    [pdx| first = %_ |] -> ps -- picks the first valid province after reduction
    _ -> warn (UnknownSection (T.pack (what ++ "@province")) stmt) ps
    where
        ifYes (GenericRhs "yes" []) set = set
        ifYes _ _ = ps

-- | Word the province qualifiers the way the three effects say them.
ppProvSel :: forall g m. (HOI4Info g, Monad m) => ProvSel -> PPT g m Text
ppProvSel ps = do
    allmsg <- opt ps_all MsgAllProvinces
    bordmsg <- opt ps_border MsgLimitToBorder
    coastmsg <- opt ps_coastal MsgLimitToCoastal
    navmsg <- opt ps_naval MsgLimitToNavalBase
    supplymsg <- opt ps_supply_node MsgLimitToSupplyNode
    victmsg <- case (ps_vp ps, ps_vp_num ps) of
        (False, Just num) -> messageText $ MsgLimitToVictoryPoint False (ps_vp_comp ps) num
        (True, Nothing) -> messageText $ MsgLimitToVictoryPoint True "" 0
        _ -> return ""
    bordcountmsg <- case ps_border_country ps of
        Just country -> do
            mflagloc <- eflag (Just HOI4Country) country
            messageText $ MsgLimitToBorderCountry (fromMaybe "<!--CHECK SCRIPT-->" mflagloc)
        _ -> return ""
    provmsg <- case ps_ids ps of
            -- Each province is named by its victory point where it has one;
            -- the helper's text already says "province", so the list needs no
            -- word of its own for it.
            Just ids -> (", on " <>) . T.intercalate ", " <$> traverse (getProvinceLoc . round) ids
            _ -> return ""
    levelmsg <- case ps_level ps of
        Just level -> messageText $ MsgProvinceLevel (ps_level_comp ps) level
        _ -> return ""
    return $ allmsg <> bordmsg <> coastmsg <> navmsg <> supplymsg <> victmsg <> bordcountmsg <> provmsg <> levelmsg
    where
        opt f msg = if f ps then messageText msg else return ""

-- | What @add_building_construction@ and @set_building_level@ both write: the
-- building, how many levels, and which provinces.
data BuildStmt = BuildStmt
    { bld_type :: Text
    , bld_level :: Maybe Double
    , bld_levelvar :: Maybe Text
    , bld_instant :: Bool
    , bld_prov :: ProvSel
    } deriving Show

parseBuildStmt :: String -> GenericScript -> BuildStmt
parseBuildStmt what = foldl' addLine (BuildStmt "" Nothing Nothing False newProvSel)
    where
        addLine b [pdx| type = $build |] = b { bld_type = build }
        addLine b [pdx| level = !num |] = b { bld_level = Just num }
        addLine b [pdx| level = $txt |] = b { bld_levelvar = Just txt }
        addLine b [pdx| level = $vartag:$var |] = b { bld_levelvar = Just (vartag <> ":" <> var) }
        addLine b [pdx| instant_build = yes |] = b { bld_instant = True }
        addLine b [pdx| instant_build = %_ |] = b
        addLine b [pdx| province = !num |] = b { bld_prov = (bld_prov b) { ps_ids = Just [num] } }
        addLine b [pdx| province = @pscr |] = b { bld_prov = foldl' (provSelLine what) (bld_prov b) pscr }
        addLine b [pdx| level > !num |] = b { bld_prov = (bld_prov b) { ps_level_comp = "higher than", ps_level = Just num } }
        addLine b [pdx| level < !num |] = b { bld_prov = (bld_prov b) { ps_level_comp = "lower than", ps_level = Just num } }
        addLine b stmt = warn (UnknownSection (T.pack what) stmt) b

addBuildingConstruction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addBuildingConstruction stmt@[pdx| %_ = @scr |] = msgToPP =<< do
    let b = parseBuildStmt "add_building_construction" scr
        buildicon = iconText (bld_type b)
    prov <- ppProvSel (bld_prov b)
    buildloc <- getGameL10n (bld_type b)
    return $ case (bld_level b, bld_levelvar b) of
        (Just val, _) -> MsgAddBuildingConstruction (bld_instant b) buildicon buildloc val prov
        (_, Just var) -> MsgAddBuildingConstructionVar (bld_instant b) buildicon buildloc var prov
        -- Script may leave out how many levels to build, in which case
        -- the game builds the one. A @level@ written with a comparison
        -- rather than an @=@ is not that number: it is a condition on
        -- what is already standing there, and has gone into 'prov'.
        _ -> MsgAddBuildingConstruction (bld_instant b) buildicon buildloc 1 prov
addBuildingConstruction stmt = warn (UnknownSection "add_building_construction" stmt) $ preStatement stmt

setBuildingLevel :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setBuildingLevel stmt@[pdx| %_ = @scr |] = msgToPP =<< do
    let b = parseBuildStmt "set_building_level" scr
        buildicon = iconText (bld_type b)
    prov <- ppProvSel (bld_prov b)
    buildloc <- getGameL10n (bld_type b)
    return $ case (bld_level b, bld_levelvar b) of
        (Just val, _) -> MsgSetBuildingLevel buildicon buildloc val prov
        (_, Just var) -> MsgSetBuildingLevelVar buildicon buildloc var prov
        _ -> preMessage stmt
setBuildingLevel stmt = preStatement stmt

-------------------------------------
-- Handler for free_building_slots --
-------------------------------------
data FreeBuildingSlots = FreeBuildingSlots
        {   fbs_building :: Text
        ,   fbs_size :: Double
        ,   fbs_comp :: Text
        ,   fbs_include_locked :: Bool
        ,   fbs_province :: Maybe Int
        }

newFBS :: FreeBuildingSlots
newFBS = FreeBuildingSlots undefined undefined undefined False Nothing

freeBuildingSlots  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
freeBuildingSlots stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_fbs =<< foldM addLine newFBS scr
    where
        addLine :: FreeBuildingSlots -> GenericStatement -> PPT g m FreeBuildingSlots
        addLine fbs [pdx| building = $txt |] = do
            return fbs { fbs_building = txt }
        addLine fbs [pdx| size < !amt |] =
            let comp = "less than" in
            return fbs { fbs_comp = comp, fbs_size = amt }
        addLine fbs [pdx| size > !amt |] =
            let comp = "more than" in
            return fbs { fbs_comp = comp, fbs_size = amt}
        addLine fbs [pdx| size = !amt |] =
            let comp = "more than" in
            return fbs { fbs_comp = comp, fbs_size = amt}
        addLine fbs [pdx| include_locked = yes |] =
            return fbs { fbs_include_locked = True }
        addLine fbs [pdx| include_locked = %_ |] =
            return fbs { fbs_include_locked = False }
        addLine fbs [pdx| province = !num |] =
            return fbs { fbs_province = Just num }
        addLine fbs stmt
            = warn (UnknownSection "free_building_slots" stmt) $ return fbs
        pp_fbs fbs = do
            let buildicon = iconText $ fbs_building fbs
                provloc = maybe "" (\p -> " in province (" <> T.pack (show p) <> ")") (fbs_province fbs)
                buildiconloc = buildicon <> provloc
            return $ MsgFreeBuildingSlots (fbs_comp fbs) (fbs_size fbs) buildiconloc (fbs_include_locked fbs)
freeBuildingSlots stmt = preStatement stmt

--------------------------------
-- Handler for build_railway --
--------------------------------

-- | What @build_railway@ and @can_build_railway@ both write: a railway level
-- and where it runs, as a path, a pair of states, or a pair of provinces.
data Railway = Railway
        {   rail_level :: Double
        ,   rail_path :: Maybe [Double]
        ,   rail_start_state :: Maybe Text
        ,   rail_target_state :: Maybe Text
        ,   rail_start_province :: Maybe Double
        ,   rail_target_province :: Maybe Double
        }

parseRailway :: forall g m. (HOI4Info g, Monad m) => String -> GenericScript -> PPT g m Railway
parseRailway what = foldM addLine (Railway 1 Nothing Nothing Nothing Nothing Nothing)
    where
        addLine :: Railway -> GenericStatement -> PPT g m Railway
        addLine br stmt@[pdx| $lhs = %rhs |] = case lhs of
            "level" -> case rhs of
                (floatRhs -> Just num) -> return br { rail_level = num }
                _ -> warn (BadValue (T.pack (what ++ " level")) stmt) $ return br
            "build_only_on_allied" -> return br
            "fallback" -> return br
            "controller_priority" -> return br
            "path" -> case rhs of
                CompoundRhs arr ->
                    let provs = mapMaybe provinceFromArray arr in
                    return br { rail_path = Just provs }
                _ -> warn (BadValue (T.pack (what ++ " path")) stmt) $ return br
            "start_state" -> (\loc -> br { rail_start_state = loc }) <$> stateRhs stmt rhs
            "target_state" -> (\loc -> br { rail_target_state = loc }) <$> stateRhs stmt rhs
            "start_province" -> return br { rail_start_province = floatRhs rhs }
            "target_province" -> return br { rail_target_province = floatRhs rhs }
            _other -> warn (UnknownSection (T.pack what) stmt) $ return br
        addLine br stmt
            = warn (UnknownSection (T.pack what) stmt) $ return br

        -- A state written as its id, a variable, or a tagged variable.
        stateRhs :: GenericStatement -> GenericRhs -> PPT g m (Maybe Text)
        stateRhs stmt = \case
            IntRhs num -> Just <$> getStateLoc num
            GenericRhs vartag [var] -> eGetState (Right (vartag, var))
            GenericRhs txt [] -> eGetState (Left txt)
            _ -> warn (BadValue (T.pack (what ++ " state")) stmt) $ return Nothing

        provinceFromArray :: GenericStatement -> Maybe Double
        provinceFromArray (StatementBare (IntLhs e)) = Just $ fromIntegral e
        provinceFromArray stmt = warn (UnknownSection "generator array" stmt) Nothing

-- | Name each province a railway path runs through, with its victory point
-- name where it has one.
railwayPathText :: (HOI4Info g, Monad m) => [Double] -> PPT g m Text
railwayPathText path = do
    provlocs <- traverse (getProvinceLoc . round) path
    return $ "through " <> T.intercalate ", " provlocs

buildRailway  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
buildRailway stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_br =<< parseRailway "build_railway" scr
    where
        pp_br br = case rail_path br of
            Just path -> MsgBuildRailwayPath (rail_level br) <$> railwayPathText path
            _ -> case (rail_start_state br, rail_target_state br,
                       rail_start_province br, rail_target_province br) of
                    (Just start, Just end, _,_) -> return $ MsgBuildRailway (rail_level br) start end
                    (_,_, Just start, Just end) ->
                        MsgBuildRailwayProv (rail_level br)
                            <$> getProvinceLoc (round start)
                            <*> getProvinceLoc (round end)
                    _ -> return $ preMessage stmt
buildRailway stmt = preStatement stmt

canBuildRailway  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
canBuildRailway stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_cbr =<< parseRailway "can_build_railway" scr
    where
        pp_cbr cbr = case rail_path cbr of
            Just path -> MsgCanBuildRailwayPath <$> railwayPathText path
            _ -> case (rail_start_state cbr, rail_target_state cbr,
                       rail_start_province cbr, rail_target_province cbr) of
                    (Just start, Just end, _,_) -> return $ MsgCanBuildRailway start end
                    (_,_, Just start, Just end) ->
                        MsgCanBuildRailwayProv
                            <$> getProvinceLoc (round start)
                            <*> getProvinceLoc (round end)
                    _ -> return $ preMessage stmt
canBuildRailway stmt = preStatement stmt

-- | Whether the railway @can_build_railway@ would build is already there. The
-- two are written with the same fields.
hasRailwayConnection :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasRailwayConnection stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_hrc =<< parseRailway "has_railway_connection" scr
    where
        pp_hrc hrc = case rail_path hrc of
            Just path -> MsgHasRailwayConnectionPath <$> railwayPathText path
            _ -> case (rail_start_state hrc, rail_target_state hrc,
                       rail_start_province hrc, rail_target_province hrc) of
                    (Just start, Just end, _,_) -> return $ MsgHasRailwayConnection start end
                    (_,_, Just start, Just end) ->
                        MsgHasRailwayConnectionProv
                            <$> getProvinceLoc (round start)
                            <*> getProvinceLoc (round end)
                    _ -> return $ preMessage stmt
hasRailwayConnection stmt = preStatement stmt

-----------------------------------------------
-- handler for set_state_province_controller  --
-----------------------------------------------

-- | Hands the provinces of a state to someone. The @limit@ is a condition on
-- whoever holds each province now, so it keeps a block of its own under the
-- line saying who takes them.
setStateProvinceController :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setStateProvinceController stmt@[pdx| %_ = @scr |] = do
    let (mcontroller, rest) = extractStmt (matchLhsText "controller") scr
    case mcontroller of
        Just [pdx| %_ = ?who |] -> do
            whoflag <- flagText (Just HOI4Country) who
            header <- msgToPP $ MsgSetStateProvinceController whoflag
            script_pp'd <- ppMany rest
            return $ header ++ script_pp'd
        _ -> preStatement stmt
setStateProvinceController stmt = preStatement stmt

---------------------------------
-- Handler for damage_building --
---------------------------------

data DamageBuilding = DamageBuilding
        {   db_type :: Text
        ,   db_damage :: Maybe Double
        ,   db_damagevar :: Maybe Text
        ,   db_province :: Maybe Int
        ,   db_provincevar :: Maybe Text
        ,   db_state :: Maybe Int
        ,   db_repair :: Maybe Double
        }

newDB :: DamageBuilding
newDB = DamageBuilding "" Nothing Nothing Nothing Nothing Nothing Nothing

damageBuilding :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
damageBuilding stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppDB =<< foldM addLine newDB scr
    where
        addLine :: DamageBuilding -> GenericStatement -> PPT g m DamageBuilding
        addLine db [pdx| type = ?txt |] = return db { db_type = txt }
        addLine db [pdx| tags = ?txt |] = return db { db_type = txt }
        addLine db [pdx| tags = @_ |] = return db { db_type = "<!-- Multiple tags check script -->" }
        addLine db [pdx| damage = !num |] = return db { db_damage = Just num }
        addLine db [pdx| damage = $txt |] = return db { db_damagevar = Just txt }
        addLine db [pdx| province = !num |] = return db {db_province = Just num}
        -- A province picked at run time out of a variable.
        addLine db [pdx| province = $vartag:$var |] = return db {db_provincevar = Just (vartag <> ":" <> var)}
        -- A building of a state rather than of a province (a fort sits in a
        -- province, a factory in a state).
        addLine db [pdx| state = !num |] = return db {db_state = Just num}
        -- How the damage repairs: negative slows the rebuild down.
        addLine db [pdx| repair_speed_modifier = !num |] = return db {db_repair = Just num}
        addLine db stmt = warn (UnknownSection "damage_building" stmt) $ return db
        ppDB db = do
            let typeicon = iconText (db_type db)
            whereloc <- case (db_state db, db_province db, db_provincevar db) of
                (Just state, _, _) -> getStateLoc state
                (_, Just prov, _) -> getProvinceLoc prov
                (_, _, Just provvar) -> return $ "province " <> typewriterText provvar
                _ -> return ""
            let repairtext = case db_repair db of
                    Just r -> " (repair speed " <> templateColor' (reducedNum (colourPc False) r) <> ")"
                    _ -> ""
            typeloc <- getGameL10n (db_type db)
            case (db_damage db, db_damagevar db) of
                (Just amt, _) -> return $ MsgDamageBuilding typeicon typeloc amt whereloc repairtext
                (_, Just amt) -> return $ MsgDamageBuildingVar typeicon typeloc amt whereloc repairtext
                _ -> return $ preMessage stmt
damageBuilding stmt = preStatement stmt

---------------------------------------
-- handler for add_province_modifier --
---------------------------------------

addProvinceModifier :: forall g m. (HOI4Info g, Monad m) => Bool -> StatementHandler g m
addProvinceModifier addrem stmt@[pdx| %_ = @scr |] =
    msgToPP =<< pp_apm (foldl' addLine ([], newProvSel) scr)
    where
        addLine :: ([Text], ProvSel) -> GenericStatement -> ([Text], ProvSel)
        addLine (mods, ps) [pdx| static_modifiers = @mscr |] = (mapMaybe getbaremods mscr, ps)
        addLine (mods, ps) [pdx| province = !num |] = (mods, ps { ps_ids = Just [num] })
        addLine (mods, ps) [pdx| province = @pscr |] = (mods, foldl' (provSelLine "add_province_modifier") ps pscr)
        addLine acc stmt = warn (UnknownSection "add_province_modifier" stmt) acc

        getbaremods (StatementBare (GenericLhs e [])) = Just e
        getbaremods stmt = warn (UnknownSection "static_modifiers array" stmt) Nothing

        pp_apm :: ([Text], ProvSel) -> PPT g m ScriptMessage
        pp_apm (mods, ps) = do
            modlocs <- mapM (\m -> do
                loc <- getGameL10n m
                return $ "<!--" <> m <> "-->" <> Doc.doc2text (iquotes loc))
                mods
            let modloc
                    | length modlocs > 1 = "modifiers " <> T.intercalate ", " modlocs
                    | length modlocs == 1 = "modifier " <> T.concat modlocs
                    | otherwise = "<!--CHECK SCRIPT-->"
            prov <- ppProvSel ps
            return $ MsgAddProvinceModifier addrem modloc prov
addProvinceModifier _ stmt = warn (UnknownSection "add_province_modifier" stmt) $ preStatement stmt

---------------------------------------------
-- Handler for any_province_building_level --
---------------------------------------------
data AnyProvBuilding = AnyProvBuilding
        {   apb_building :: Maybe Text
        ,   apb_comp :: Text
        ,   apb_level :: Double
        ,   apb_provinces :: [Double]
        ,   apb_all :: Bool
        ,   apb_border :: Bool
        ,   apb_coastal :: Bool
        ,   apb_naval :: Bool
        ,   apb_victory_point :: Bool
        }

newAPB :: AnyProvBuilding
newAPB = AnyProvBuilding Nothing "at least" 0 [] False False False False False

-- | Handler for @any_province_building_level@, which asks whether any province
-- of the state, of those the @province@ block picks out, has a building of the
-- level given.
anyProvinceBuildingLevel :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
anyProvinceBuildingLevel stmt@[pdx| %_ = @scr |] = case apb_building apb of
        Just bld -> msgToPP $ MsgAnyProvinceBuildingLevel (iconText bld)
                        (apb_comp apb) (apb_level apb) whichProvinces
        Nothing -> preStatement stmt
    where
        apb = foldl' addLine newAPB scr
        addLine a [pdx| building = $b |] = a { apb_building = Just b }
        addLine a [pdx| level > !n |] = a { apb_comp = "more than", apb_level = n }
        addLine a [pdx| level < !n |] = a { apb_comp = "fewer than", apb_level = n }
        addLine a [pdx| level = !n |] = a { apb_comp = "at least", apb_level = n }
        addLine a [pdx| province = !n |] = a { apb_provinces = apb_provinces a ++ [n] }
        addLine a [pdx| province = @pscr |] = foldl' addProv a pscr
        addLine a stmt = warn (UnknownSection "any_province_building_level" stmt) a

        addProv a [pdx| id = !n |] = a { apb_provinces = apb_provinces a ++ [n] }
        addProv a [pdx| all_provinces = yes |] = a { apb_all = True }
        addProv a [pdx| limit_to_border = yes |] = a { apb_border = True }
        addProv a [pdx| limit_to_coastal = yes |] = a { apb_coastal = True }
        addProv a [pdx| limit_to_naval_base = yes |] = a { apb_naval = True }
        addProv a [pdx| limit_to_victory_point = yes |] = a { apb_victory_point = True }
        addProv a [pdx| $_ = no |] = a
        addProv a stmt = warn (UnknownSection "any_province_building_level@province" stmt) a

        -- Which provinces of the state count. Nothing said means all of them,
        -- which is also what @all_provinces@ asks for outright.
        whichProvinces = T.intercalate ", " (filter (not . T.null) [limits, listed])
        limits = T.intercalate " and " $ concat
            [ [ "on the border" | apb_border apb ]
            , [ "on the coast" | apb_coastal apb ]
            , [ "with a naval base" | apb_naval apb ]
            , [ "with victory points" | apb_victory_point apb ]
            ]
        listed = case apb_provinces apb of
            [] -> ""
            provs -> T.pack (concat ["(", intercalate "), (" (map (show . round) provs), ")"])
anyProvinceBuildingLevel stmt = preStatement stmt
