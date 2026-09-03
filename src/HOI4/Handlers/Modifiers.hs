{-|
Module      : HOI4.Handlers.Modifiers
Description : Modifiers

Handlers for the static modifier blocks written under ideas, traits and
the like, for dynamic modifiers, and for the bonuses to research and
equipment.
-}
module HOI4.Handlers.Modifiers (
        plainmodifiermsg
    ,   handleModifier
    ,   sortmods
    ,   modifierMSG
    ,   handleResearchBonus
    ,   handleHiddenModifier
    ,   handleTargetedModifier
    ,   handleEquipmentBonus
    ,   addEquipmentBonus
    ,   addDynamicModifier
    ,   ppDynModBox
    ,   removeDynamicModifier
    ,   projectInBlock
    ,   stripProjectPrefix
    ,   hasDynamicModifier
    ) where

import Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import Data.List (elemIndex, foldl', sortOn)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative ((<|>))

import Debug.Trace

import Abstract -- everything
import MessageTools (formatDays)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, getCurrentIndent, withCurrentIndent
                     , withCurrentIndentCustom, getGameInterfaceNamed, LocArg (..)
                     , getCurrentScope)
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.ModifierTable (modifiersTable)
import HOI4.Types -- everything

import HOI4.Handlers.Core (msgToPP, plainMsg', preStatement, tooltipText)
import HOI4.Handlers.Generic (numericLoc, textAtom, withLocAtom, withLocAtom')
import HOI4.Handlers.Research (masteryGainModifier)

-----------------------
-- modifier handlers --
-----------------------
plainmodifiermsg :: forall g m. (HOI4Info g, Monad m) =>
        ScriptMessage -> StatementHandler g m
plainmodifiermsg msg stmt@[pdx| %_ = @scr |] = do
    let (mmod, _) = extractStmt (matchLhsText "modifier") scr
    modmsg <- case mmod of
        Just stmt@[pdx| modifier = @_ |] -> indentUp $ handleModifier stmt
        _ -> preStatement stmt
    basemsg <- msgToPP msg
    return $ basemsg ++ modmsg
plainmodifiermsg _ stmt = preStatement stmt

-- | Write out a block of modifiers: sort them the way the game lists them, then
-- word each one.
ppModifiers :: forall g m. (HOI4Info g, Monad m) =>
    Bool -> Text -> GenericScript -> PPT g m IndentedMessages
ppModifiers hidden targ scr = do
    keys <- getModKeys
    sm <- sortmods scr keys
    fold <$> traverse (modifierMSG hidden targ) sm

handleModifier :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleModifier [pdx| %_ = @scr |] = ppModifiers False "" scr
handleModifier stmt = preStatement stmt

sortmods :: forall g m. (HOI4Info g, Monad m) => GenericScript -> [Text] -> PPT g m GenericScript
sortmods scr keys = modsrec' keys scr [] [] [] []
    where
    modsrec' :: forall g m. (HOI4Info g, Monad m) => [Text] -> GenericScript ->[(Int,GenericStatement)] -> GenericScript -> GenericScript -> GenericScript -> PPT g m GenericScript
    modsrec' _ [] ord_mod unord_mod hid_mod custom =
        let moo = map snd $ sortOn fst ord_mod in
        return $  moo ++ reverse unord_mod ++ reverse hid_mod ++ reverse custom
    modsrec' keys (stmt:xs) ord_mod unord_mod hid_mod custom = case stmt of
        [pdx| hidden_modifier = %_|] ->
            let hr = stmt:hid_mod in
            modsrec' keys xs ord_mod unord_mod hr custom
        [pdx| custom_modifier_tooltip = %_|] ->
            let cr = stmt:custom in
            modsrec' keys xs ord_mod unord_mod hid_mod cr
        [pdx| $mod = %_|] -> case elemIndex mod keys of
            Just num ->
                let mor = (num,stmt):ord_mod in
                modsrec' keys xs mor unord_mod hid_mod custom
            Nothing ->
                let sr = stmt:unord_mod in
                modsrec' keys xs ord_mod sr hid_mod custom
        _ -> let sr = stmt:unord_mod in modsrec' keys xs ord_mod sr hid_mod custom

-- | Write out one family modifier's number once its localization is found
-- under the given key: look the key up, dress the name, and write the value to
-- the given precision with the given message. A key with no localization falls
-- back to the raw statement.
famModNum :: (HOI4Info g, Monad m) =>
    Bool -> Text -> Maybe Int -> Text
    -> (Text -> Maybe Int -> Double -> ScriptMessage)
    -> StatementHandler g m
famModNum hidden targ dec key msg stmt = do
    mloc <- getGameL10nIfPresent key
    case mloc of
        Just loc -> numericLocPrec (locprep hidden targ loc) dec msg stmt
        Nothing -> preStatement stmt

-- | As 'famModNum' for the variable spelling, which always comes out as
-- 'MsgModifierVar'.
famModVar :: (HOI4Info g, Monad m) =>
    Bool -> Text -> Text -> Text -> StatementHandler g m
famModVar hidden targ key var stmt = do
    mloc <- getGameL10nIfPresent key
    case mloc of
        Just loc -> msgToPP $ MsgModifierVar (locprep hidden targ loc) var
        Nothing -> preStatement stmt

modifierMSG :: forall g m. (HOI4Info g, Monad m) =>
        Bool -> Text -> StatementHandler g m
-- Not modifiers but a word on where the dynamic modifier applies: it is also
-- read in combat for whoever attacks or defends, even from outside the state.
modifierMSG _ _ [pdx| attacker_modifier = yes |] = msgToPP MsgModifierAttackerSide
modifierMSG _ _ [pdx| defender_modifier = yes |] = msgToPP MsgModifierDefenderSide
modifierMSG _ targ stmt@[pdx| $specmod = @scr|]
    | specmod == "hidden_modifier" = ppModifiers True targ scr
    | otherwise = do
        terrain <- getTerrain
        if specmod `elem` terrain || specmod `elem` ["fort", "river", "night"]
        then do
            ter <- getGameL10n specmod
            termsg <- plainMsg' ("'''" <> ter <> "''':")
            modmsg <- fold <$> indentUp (traverse (modifierMSG False targ) scr)
            return $ termsg : modmsg
        else warn (UnknownSection "modifier block" stmt) $ preStatement stmt
modifierMSG hidden targ stmt@[pdx| $mod = !(_ :: Double) |] = let lmod = T.toLower mod in case HM.lookup lmod modifiersTable of
    Just (key, msg, dec) -> do
        loc <- getGameL10n key
        numericLocPrec (locprep hidden targ loc) dec msg stmt
    Nothing
        | "cat_" `T.isPrefixOf` lmod ->
            famModNum hidden targ (familyPrecision lmod) lmod MsgModifierPcNegReduced stmt
        | ("production_speed_" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("state_production_speed_" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("experience_gain_" `T.isPrefixOf` lmod && "_combat_factor" `T.isSuffixOf` lmod) ||
            ("trait_" `T.isPrefixOf` lmod && "_xp_gain_factor" `T.isSuffixOf` lmod) ||
            ("repair_speed" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("state_repair_speed" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) -> --precision 2
            famModNum hidden targ (familyPrecision lmod) ("modifier_" <> lmod) MsgModifierPcPosReduced stmt
        | "unit_" `T.isPrefixOf` lmod && "_design_cost_factor" `T.isSuffixOf` lmod ->
            famModNum hidden targ (familyPrecision lmod) ("modifier_" <> lmod) MsgModifierPcNegReduced stmt
        | "modifier_army_sub_" `T.isPrefixOf` lmod ||
            (("operation_" `T.isPrefixOf` lmod && "_outcome" `T.isSuffixOf` lmod ) ||
            -- Every special project speed, whether it is the one project's own
            -- (@sp_land_stronghold_network_speed_factor@) or a whole category's
            -- (@sp_tag_radar_speed_factor@), is named by the modifier itself.
            ("sp_" `T.isPrefixOf` lmod && "_speed_factor" `T.isSuffixOf` lmod) ||
            "sp_tag_" `T.isPrefixOf` lmod ||
            "_preferred_weight_factor"` T.isSuffixOf` lmod ||
            ("specialization_" `T.isPrefixOf` lmod && "_speed_factor" `T.isSuffixOf` lmod)) ->
            famModNum hidden targ (familyPrecision lmod) lmod MsgModifierPcPosReduced stmt
        | "operation_" `T.isPrefixOf` lmod && ("_risk" `T.isSuffixOf` lmod || "_cost" `T.isSuffixOf` lmod ) ||
            "_design_cost_factor" `T.isSuffixOf` lmod ->
            famModNum hidden targ (familyPrecision lmod) lmod MsgModifierPcNegReduced stmt
        | ("state_resource_" `T.isPrefixOf` lmod && not ("state_resource_cost_" `T.isPrefixOf` lmod)) || --precision 0
            ("country_resource_" `T.isPrefixOf` lmod && not ("country_resource_cost_" `T.isPrefixOf` lmod)) || --precision 0
            "temporary_state_resource_" `T.isPrefixOf` lmod -> --precision 0
            famModNum hidden targ (familyPrecision lmod) lmod MsgModifierColourPos stmt
        -- Mastery towards part of the doctrine tree, which the game words by
        -- filling the part's name into a sentence of its own for each kind of
        -- part there is.
        | "_mastery_gain_factor" `T.isSuffixOf` lmod -> do --precision 0
            mpart <- masteryGainModifier lmod
            case mpart of
                Just (wrapper, name) -> do
                    loc <- getGameL10nArgs (HM.singleton "ARG" (LocText name)) wrapper
                    let loc' = locprep hidden targ loc in
                        numericLocPrec loc' (Just 0) MsgModifierPcPosReduced stmt
                Nothing -> preStatement stmt
        -- A flat number of levels rather than a share of anything, and more of
        -- them is better.
        |  "_max_level_terrain_limit" `T.isSuffixOf` lmod -> --precision 0
            famModNum hidden targ (familyPrecision lmod) ("modifier_" <> lmod) MsgModifierColourPos stmt
        |  "_intel_decryption_bonus" `T.isSuffixOf` lmod -> --precision 0
            famModNum hidden targ (familyPrecision lmod) ("modifier_" <> lmod) MsgModifierColourPos stmt
        | "country_resource_cost_" `T.isPrefixOf` lmod -> --precision 0
            famModNum hidden targ (familyPrecision lmod) lmod MsgModifierColourNeg stmt
        | "production_cost_max_" `T.isPrefixOf` lmod -> --precision 0
            famModNum hidden targ (familyPrecision lmod) ("modifier_" <> lmod) MsgModifierYellow stmt
        | otherwise -> do
            moddef <- getModifierDefinitions
            case HM.lookup mod moddef of
                Just (_, scrmsg, dec) -> famModNum hidden targ dec mod scrmsg stmt
                Nothing -> preStatement stmt
-- A modifier block can carry a sentence of its own saying what the modifiers in
-- it come to; 'sortmods' has already moved it to the end of the block. The key
-- may be quoted, and the sentence may run over several lines.
modifierMSG _ _ stmt@[pdx| custom_modifier_tooltip = ?key |] = do
    mloc <- getGameL10nIfPresent key
    maybe (preStatement stmt) (tooltipText MsgCustomModifierTooltip) mloc
modifierMSG hidden targ stmt@[pdx| $mod = $var|] =  let lmod = T.toLower mod in case HM.lookup lmod modifiersTable of
    Just (key, _, _) -> do
        loc <- getGameL10n key
        let loc' = locprep hidden targ loc
        msgToPP $ MsgModifierVar loc' var
    Nothing
        | "cat_" `T.isPrefixOf` lmod -> famModVar hidden targ lmod var stmt
        | ("production_speed_" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("state_production_speed_" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("unit_" `T.isPrefixOf` lmod && "_design_cost_factor" `T.isSuffixOf` lmod) ||
            ("experience_gain_" `T.isPrefixOf` lmod && "_combat_factor" `T.isSuffixOf` lmod) ||
            ("trait_" `T.isPrefixOf` lmod && "_xp_gain_factor" `T.isSuffixOf` lmod) ||
            ("repair_speed" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            "_max_level_terrain_limit" `T.isSuffixOf` lmod ||
            ("state_repair_speed" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ->
            famModVar hidden targ ("modifier_" <> lmod) var stmt
        | "modifier_army_sub_" `T.isPrefixOf` lmod ||
            ("operation_" `T.isPrefixOf` lmod && "_outcome" `T.isSuffixOf` lmod) ||
            ("sp_" `T.isPrefixOf` lmod && "_speed_factor" `T.isSuffixOf` lmod) ||
            "sp_tag_" `T.isPrefixOf` lmod ||
            ("country_resource_" `T.isPrefixOf` lmod && not ("country_resource_cost_" `T.isPrefixOf` lmod)) ||
            "_design_cost_factor" `T.isSuffixOf` lmod ||
            "state_resource_" `T.isPrefixOf` lmod ||
            "country_resource_cost_" `T.isPrefixOf` lmod ||
            "temporary_state_resource_" `T.isPrefixOf` lmod ->
            famModVar hidden targ lmod var stmt
        | "_mastery_gain_factor" `T.isSuffixOf` lmod -> do
            mpart <- masteryGainModifier lmod
            case mpart of
                Just (wrapper, name) -> do
                    loc <- getGameL10nArgs (HM.singleton "ARG" (LocText name)) wrapper
                    let loc' = locprep hidden targ loc in
                        msgToPP $ MsgModifierVar loc' var
                Nothing -> preStatement stmt
        | otherwise -> do
            moddef <- getModifierDefinitions
            case HM.lookup mod moddef of
                Just _ -> famModVar hidden targ mod var stmt
                Nothing -> preStatement stmt
modifierMSG _ _ stmt = preStatement stmt

-- | The decimal places the documentation gives for a modifier family: the
-- modifiers named after a building, a unit, a resource or the like, which are
-- documented by their shape rather than one at a time, and which 'modifierMSG'
-- likewise recognises by their shape rather than from 'modifiersTable'.
familyPrecision :: Text -> Maybe Int
familyPrecision lmod = Just $
    if any (`T.isPrefixOf` lmod)
            [ "experience_gain_"          -- experience_gain_<Unit>_combat_factor
            , "sp_", "specialization_"    -- <SpecialProject>_speed_factor
            , "operation_"               -- <Operation>_cost, _outcome and _risk
            , "state_resource_", "country_resource_", "temporary_state_resource_"
            , "production_cost_max_"     -- production_cost_max_<NavalEquipment>
            , "cat_"                     -- <IdeaCategory>_category_type_cost_factor
            , "modifier_army_sub_" ]
        || any (`T.isSuffixOf` lmod)
            [ "_intel_decryption_bonus"
            , "_max_level_terrain_limit" ] -- <Building>[_<Terrain>]_max_level_terrain_limit
        then 0
        -- The speed, repair, design cost, trait experience and preferred weight
        -- families are all written to two places.
        else 2

-- | Write out a modifier's value to the number of decimal places the game
-- writes it to. 'Nothing' leaves it with as many places as it was written with.
numericLocPrec :: (HOI4Info g, Monad m) =>
    Text
        -> Maybe Int
        -> (Text -> Maybe Int -> Double -> ScriptMessage)
        -> StatementHandler g m
numericLocPrec what dec msg = numericLoc what (\loc -> msg loc dec)

locprep :: Bool -> Text -> Text -> Text
locprep hidden targ loc = (if hidden then "(Hidden)" else "") <> named
    where
        loc' = if ": " `T.isSuffixOf` loc then T.dropEnd 2 loc else loc
        named | T.null targ = loc'
              | otherwise = targ <> " " <> T.strip loc'

handleResearchBonus :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleResearchBonus [pdx| %_ = @scr |] = fold <$> traverse handleResearchBonus' scr
    where
        handleResearchBonus' stmt@[pdx| $tech = !(_ :: Double) |] = numericLocPrec (T.toLower tech <> "_research") Nothing MsgModifierPcPosReduced stmt
        handleResearchBonus' scr = preStatement scr
handleResearchBonus stmt = preStatement stmt

handleHiddenModifier :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleHiddenModifier [pdx| %_ = @scr |] = ppModifiers True "" scr
handleHiddenModifier stmt = preStatement stmt

handleTargetedModifier :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleTargetedModifier stmt@[pdx| %_ = @scr |] = do
    let (tag, rest) = extractStmt (matchLhsText "tag") scr
    tagmsg <- case tag of
        Just [pdx| tag = $country |] -> flagText (Just HOI4Country) country
        _ -> return "CHECK SCRIPT"
    ppModifiers False tagmsg rest
handleTargetedModifier stmt = preStatement stmt

handleEquipmentBonus :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleEquipmentBonus stmt@[pdx| %_ = @scr |] = fold <$> traverse modifierEquipMSG scr
        where
            modifierEquipMSG [pdx| $tech = @scr |] = do
                let (_, rest) = extractStmt (matchLhsText "instant") scr
                techloc <- getGameL10n tech
                -- The game picks the equipment out of the tooltip in yellow;
                -- the wiki writes that emphasis as bold.
                techmsg <- plainMsg' ("'''" <> techloc <> "'''")
                modmsg <- indentUp $ ppModifiers False "" rest
                return $ techmsg : modmsg
            modifierEquipMSG stmt = preStatement stmt
handleEquipmentBonus stmt = preStatement stmt

-- | The same equipment bonuses an idea can carry, given to the country on their
-- own. The name is a localization key the game titles the bonus with, and the
-- bonuses themselves are written exactly as an idea writes them.
addEquipmentBonus :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addEquipmentBonus stmt@[pdx| %_ = @scr |] = case extractStmt (matchLhsText "bonus") scr of
    (Just bonus, rest) -> do
        let (mname, _) = extractStmt (matchLhsText "name") rest
        header <- case mname of
            Just [pdx| %_ = $key |] -> MsgAddEquipmentBonus <$> getGameL10n key
            _ -> return MsgAddEquipmentBonusUnnamed
        headmsg <- msgToPP header
        bonusmsg <- indentUp $ handleEquipmentBonus bonus
        return $ headmsg ++ bonusmsg
    _ -> preStatement stmt
addEquipmentBonus stmt = preStatement stmt

-------------------------------------------------
-- Handler for add_dynamic_modifier --
-------------------------------------------------
data AddDynamicModifier = AddDynamicModifier
    { adm_modifier :: Text
    , adm_scope :: Maybe (Either Text (Text, Text))
    , adm_days :: Maybe Double
    }

newADM :: AddDynamicModifier
newADM = AddDynamicModifier undefined Nothing Nothing

addDynamicModifier :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addDynamicModifier stmt@[pdx| %_ = @scr |] =
    pp_adm (foldl' addLine newADM scr)
    where
        addLine adm [pdx| modifier = $mod |] = adm { adm_modifier = mod }
        addLine adm [pdx| scope = $tag |] = adm { adm_scope = Just (Left tag) }
        addLine adm [pdx| scope = $vartag:$var |] = adm { adm_scope = Just (Right (vartag, var)) }
        addLine adm [pdx| days = !amt |] = adm { adm_days = Just amt }
        addLine adm stmt = warn (UnknownSection "add_dynamic_modifier" stmt) adm
        pp_adm adm = do
            let days = maybe "" formatDays (adm_days adm)
            -- Nothing but a bare @add_dynamic_modifier@ says the country running
            -- the effect gets it, and that goes without saying.
            who <- case adm_scope adm of
                Nothing -> return ""
                Just admscope -> do
                    thescope <- getCurrentScope
                    dynflag <- case thescope of
                        Just HOI4Country -> eflag (Just HOI4Country) admscope
                        Just HOI4ScopeState -> eGetState admscope
                        Just HOI4From -> return $ Just "FROM"
                        _ -> return $ Just "<!-- check script -->"
                    return $ fromMaybe "<!-- check script -->" dynflag
            mmod <- HM.lookup (adm_modifier adm) <$> getDynamicModifiers
            case mmod of
                Just dmod -> do
                    effects <- resolveDynModEffects (dmodEffects dmod)
                    -- The @enable@ trigger says when the modifier counts for
                    -- anything, but nearly every one is written @always = yes@,
                    -- which is a placeholder standing in for no condition at all.
                    trigger <- if all isAlways (dmodEnable dmod)
                        then return []
                        else indentUp $ ppMany (dmodEnable dmod)
                    limit <- withCurrentIndent $ \i ->
                        return $ if null trigger then [] else (i+1, MsgLimit) : trigger
                    body <- if null effects
                        -- Every value it grants is zero at this point, so an
                        -- effect box would come out empty. Name it and leave it.
                        then do
                            icon <- dynModIcon dmod
                            let locName = fromMaybe (dmodName dmod) (dmodLocName dmod)
                            msgToPP $ MsgAddDynamicModifierNamed who days icon (dmodName dmod) locName
                        else do
                            header <- msgToPP $ MsgAddDynamicModifier who days
                            box <- ppDynModBox dmod effects
                            return (header ++ box)
                    return (body ++ limit)
                Nothing -> trace ("add_dynamic_modifier: Modifier " ++ T.unpack (adm_modifier adm) ++ " not found") $ preStatement stmt
        isAlways [pdx| always = yes |] = True
        isAlways _ = False
addDynamicModifier stmt = warn (UnknownSection "add_dynamic_modifier" stmt) $ preStatement stmt

-- | Fill in the numbers a dynamic modifier's entries name instead of writing
-- out. Script keeps such a modifier's values in variables so that it can raise
-- and lower what the modifier grants as the game goes on, and what a reader
-- wants where it is handed out is what it grants then, which is what those
-- variables start the game holding.
--
-- An entry whose value comes to zero is dropped, as the game drops it from the
-- tooltip: a modifier of zero grants nothing. A variable no script ever writes
-- to holds zero as well, so a name that resolves to nothing goes the same way.
resolveDynModEffects :: (HOI4Info g, Monad m) => GenericScript -> PPT g m GenericScript
resolveDynModEffects effects = do
    vars <- getInitialVariables
    constants <- getScriptConstants
    let value var = HM.lookup var vars <|> HM.lookup var constants
    return $ mapMaybe (resolve value) effects
    where
        resolve value stmt@[pdx| $key = $var |]
            -- A handful of modifiers are switches rather than amounts, and are
            -- written out as they stand.
            | var `notElem` ["yes", "no"] = case value var of
                Just val | val /= 0 -> Just (Statement (GenericLhs key []) OpEq (FloatRhs val))
                _ -> Nothing
        resolve _ stmt = Just stmt

-- | The image a dynamic modifier is shown by, or the empty text if it has none.
dynModIcon :: (HOI4Info g, Monad m) => HOI4DynamicModifier -> PPT g m Text
dynModIcon dmod = maybe (return "") getGameInterfaceNamed (dmodIcon dmod)

-- | Present a dynamic modifier as an effect box: its name and image as the
-- heading, and what it grants listed under that.
ppDynModBox :: (HOI4Info g, Monad m) =>
    HOI4DynamicModifier -> GenericScript -> PPT g m IndentedMessages
ppDynModBox dmod effects = do
    curind <- getCurrentIndent
    let curindent = fromMaybe 1 curind
        name = fromMaybe (dmodName dmod) (dmodLocName dmod)
    modicon <- dynModIcon dmod
    modmsg <- withCurrentIndentCustom 1 $ \_ ->
        fold <$> traverse (modifierMSG False "") effects
    return $ ((0, MsgEffectBox name (dmodName dmod) modicon "") : modmsg)
        ++ [(0, MsgEffectBoxEnd curindent)]

removeDynamicModifier :: (HOI4Info g, Monad m) => StatementHandler g m
removeDynamicModifier stmt@[pdx| %_ = $txt |] = withLocAtom MsgRemoveDynamicMod stmt
-- A @scope@ narrows which copy of the modifier goes, and @days@ is left over
-- from adding it; the name is the only part of the block that says what is
-- taken away.
removeDynamicModifier stmt@[pdx| %_ = @dyn |] =
    case [modstmt | modstmt@[pdx| modifier = %_ |] <- dyn] of
        (modstmt : _) -> withLocAtom MsgRemoveDynamicMod modstmt
        [] -> case dyn of
            [stmtd@[pdx| %_ = $txt |]] -> withLocAtom MsgRemoveDynamicMod stmtd
            _ -> preStatement stmt
removeDynamicModifier stmt = preStatement stmt

-- | Some effects name a special project in a field of a block, along with who
-- works on it and where. Which project it is is the whole of what a reader
-- needs; the scientist and the facility are the game's own bookkeeping.
projectInBlock :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
projectInBlock msg stmt@[pdx| %_ = @scr |] =
    case fst (extractStmt (matchLhsText "project") scr) of
        Just proj -> withLocAtom' msg stripProjectPrefix proj
        _ -> preStatement stmt
projectInBlock msg stmt = withLocAtom' msg stripProjectPrefix stmt

-- | Script names a special project with a @sp:@ in front of it, where the name
-- itself is localized under the key without it.
stripProjectPrefix :: Text -> Text
stripProjectPrefix key = fromMaybe key (T.stripPrefix "sp:" key)

hasDynamicModifier :: (HOI4Info g, Monad m) => StatementHandler g m
hasDynamicModifier stmt@[pdx| %_ = @dyn |] = if length dyn == 2
    then textAtom "scope" "modifier" MsgHasDynamicModFlag flagMaybeText stmt
    else case dyn of
        [stmtd@[pdx| %_ = $txt |]] ->  withLocAtom MsgHasDynamicMod stmtd
        _-> preStatement stmt
-- Script also names the modifier on the right with nothing around it.
hasDynamicModifier stmt@[pdx| %_ = $_ |] = withLocAtom MsgHasDynamicMod stmt
hasDynamicModifier stmt = preStatement stmt
