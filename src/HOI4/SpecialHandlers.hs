
module HOI4.SpecialHandlers (
        handleIdeas
    ,   handleTimedIdeas
    ,   handleSwapIdeas
    ,   handleModifier
    ,   showIdea
    ,   plainmodifiermsg
    ,   modifierMSG
    ,   handleResearchBonus
    ,   handleHiddenModifier
    ,   handleTargetedModifier
    ,   handleEquipmentBonus
    ,   addDynamicModifier
    ,   removeDynamicModifier
    ,   hasDynamicModifier
    ,   ScriptChunk (..)
    ,   chunkScript
    ,   ppDynModChunk
    ,   ppIdeaSlotChunk
    ,   addPowerBalanceModifier
    ,   relationModifier
    ,   addMastery
    ,   addMasteryBonus
    ,   setCanBeFiredInAdvisorRole
    ,   addRelationRuleOverride
    ,   reduceFocusCompletionCost
    ,   mioScope
    ,   hasDoctrine
    ,   removeCountryLeaderRole
    ,   canBeCountryLeader
    ,   createIntelligenceAgency
    ,   agencyUpgradeLink
    ,   addFieldMarshalRole
    ,   addAdvisorRole
    ,   removeAdvisorRole
    ,   addLeaderRole
    ,   createLeader
    ,   promoteCharacter
    ,   setCharacterName
    ,   withCharacter
    ,   createOperativeLeader
    ,   handleTrait
    ,   addRemoveLeaderTrait
    ,   addRemoveUnitTrait
    ,   addTimedTrait
    ,   swapLeaderTrait
    ,   eventOptionTooltip
    ,   customTriggerTooltip
    ,   customOverrideTooltip
    ,   tooltipWith
    ,   showUnitLeader
    ,   mioTooltip
    ,   characterListTooltip
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Char (chr, isDigit, toUpper)
import Data.List (foldl', groupBy, sortOn, elemIndex, find)
import Data.Maybe

import Control.Applicative ((<|>))
import Control.Monad.State (gets)
import Data.Foldable (fold)

import Abstract -- everything
import qualified Doc -- everything
import HOI4.Messages -- everything
import MessageTools (iquotes
                    , plainNum
                    , formatDays
                    , typewriterText)
import QQ -- everything
import SettingsTypes ( PPT, IsGameData (..), GameData (..), IsGameState (..), GameState (..)
                     , scope
                     , indentUp, getCurrentIndent, withCurrentIndent, withCurrentIndentCustom
                     , LocArg (..)
                     , concatMapM
                     , getGameInterface, getGameInterfaceNamed, getGameInterfaceIfPresent)
import StatementUtils -- everything
import {-# SOURCE #-} HOI4.Common (ppMany, ppOne)
import HOI4.ModifierTable (modifiersTable)
import HOI4.Types -- everything
import HOI4.Localization
import HOI4.WikiTables (doctrineFolders, agencyUpgradeBranches, focusPages, focusPageSplits, focusTagPages)
import Debug.Trace
import HOI4.Handlers -- everything

-----------------
-- handle idea --
-----------------

handleSwapIdeas :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleSwapIdeas stmt@[pdx| %_ = @scr |]
    = pp_si (parseTA "add_idea" "remove_idea" scr)
    where
        pp_si :: TextAtom -> PPT g m IndentedMessages
        pp_si ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                add_loc <- handleIdeaWithEffects True what
                remove_loc <- handleIdea False atom
                case (add_loc, remove_loc) of
                    (Just (addcategory, addideaIcon, addideaKey, addidea_loc, Just addeffectbox),
                     Just (category, ideaIcon, ideaKey, idea_loc, _)) ->
                        if addidea_loc == idea_loc then do
                                idmsg <- msgToPP $ MsgModifyIdea category ideaIcon ideaKey idea_loc
                                                    addcategory addideaIcon addideaKey addidea_loc
                                return $ idmsg ++ addeffectbox
                        else do
                            idmsg <- msgToPP $ MsgReplaceIdea category ideaIcon ideaKey idea_loc
                                                addcategory addideaIcon addideaKey addidea_loc
                            return $ idmsg ++ addeffectbox
                    (Just (addcategory, addideaIcon, addideaKey, addidea_loc, Nothing),
                     Just (category, ideaIcon, ideaKey, idea_loc, _)) ->
                        if addidea_loc == idea_loc then
                            msgToPP $ MsgModifyIdea category ideaIcon ideaKey idea_loc
                                addcategory addideaIcon addideaKey addidea_loc
                        else
                            msgToPP $ MsgReplaceIdea category ideaIcon ideaKey idea_loc
                                addcategory addideaIcon addideaKey addidea_loc
                    _ -> preStatement stmt
            _ -> preStatement stmt
handleSwapIdeas stmt = preStatement stmt

-- | Write out one idea from what 'handleIdea' found of it: the message, with
-- the idea's effect box under it where it has one. 'Nothing' falls back to the
-- raw statement.
ppIdeaMsg :: (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> Text -> ScriptMessage)
    -> GenericStatement
    -> Maybe (Text, Text, Text, Text, Maybe IndentedMessages)
    -> PPT g m IndentedMessages
ppIdeaMsg _ stmt Nothing = preStatement stmt
ppIdeaMsg msg _ (Just (category, ideaIcon, ideaKey, idea_loc, meffectbox)) = do
    idmsg <- msgToPP $ msg category ideaIcon ideaKey idea_loc
    return $ idmsg ++ fromMaybe [] meffectbox

handleTimedIdeas :: forall g m. (HOI4Info g, Monad m) =>
        (Text -> Text -> Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> StatementHandler g m
handleTimedIdeas msg stmt@[pdx| %_ = @scr |]
    = pp_idda (parseTV "idea" "days" scr)
    where
        pp_idda :: TextValue -> PPT g m IndentedMessages
        pp_idda tv = case (tv_what tv, tv_value tv) of
            (Just what, Just value) ->
                ppIdeaMsg (\c i k l -> msg c i k l value) stmt =<< handleIdea True what
            _ -> preStatement stmt
handleTimedIdeas _ stmt = preStatement stmt

handleIdeas :: forall g m. (HOI4Info g, Monad m) =>
    Bool ->
    (Text -> Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
handleIdeas addIdea msg stmt@[pdx| $lhs = %idea |] = case idea of
    CompoundRhs ideas -> if length ideas == 1 then
                ppIdeaMsg msg stmt =<< handleIdea addIdea (mconcat $ map getbareidea ideas)
            else do
                ideashandle <- mapM (handleIdea addIdea . getbareidea) ideas
                let ideashandled = catMaybes ideashandle
                    ideasmsgd :: [(Text, Text, Text, Text, Maybe IndentedMessages)] -> PPT g m [IndentedMessages]
                    ideasmsgd ihs = mapM (\ih ->
                            let (category, ideaIcon, ideaKey, idea_loc, effectbox) = ih in
                            withCurrentIndent $ \i -> case effectbox of
                                    Just boxNS -> return ((i, msg category ideaIcon ideaKey idea_loc):boxNS)
                                    _-> return [(i, msg category ideaIcon ideaKey idea_loc)]
                                ) ihs
                ideasmsgdd <- ideasmsgd ideashandled
                return $ mconcat ideasmsgdd
    GenericRhs txt [] -> ppIdeaMsg msg stmt =<< handleIdea addIdea txt
    _ -> preStatement stmt
handleIdeas _ _ stmt = preStatement stmt

getbareidea :: GenericStatement -> Text
getbareidea (StatementBare (GenericLhs e [])) = e
getbareidea _ = "<!-- Check Script -->"

handleIdea :: (HOI4Info g, Monad m) =>
        Bool -> Text ->
           PPT g m (Maybe (Text, Text, Text, Text, Maybe IndentedMessages))
handleIdea = handleIdea' False

-- | As 'handleIdea', for an idea named for what it grants rather than to say
-- which one it is. A national spirit says what it grants wherever it turns up,
-- while a designer or a company is usually only named -- but where one is
-- swapped for an improved version, what the new one grants is the whole of what
-- the swap does, so it is written out whatever kind of idea it is.
handleIdeaWithEffects :: (HOI4Info g, Monad m) =>
        Bool -> Text ->
           PPT g m (Maybe (Text, Text, Text, Text, Maybe IndentedMessages))
handleIdeaWithEffects = handleIdea' True

handleIdea' :: (HOI4Info g, Monad m) =>
        Bool -> Bool -> Text ->
           PPT g m (Maybe (Text, Text, Text, Text, Maybe IndentedMessages))
handleIdea' always addIdea ide = do
    ides <- getIdeas
    charto <- getCharToken
    let midea = HM.lookup ide ides
    case midea of
        Just iidea -> do
            let ideaKey = id_id iidea
                ideaname = id_name iidea
            ideaIcon <- getIdeaIcon iidea
            idea_loc <- getGameL10n ideaname
            category <- if id_category iidea == "country" then getGameL10n "FE_COUNTRY_SPIRIT" else getGameL10n $ id_category iidea
            effectbox <- modmessage iidea idea_loc ideaKey ideaIcon
            effectboxNS <- if addIdea && (always || id_category iidea == "country")
                              then return $ Just effectbox else return Nothing
            -- The wiki has an icon of its own for each of the country's laws,
            -- keyed by the law's name, and shows a law by that rather than by
            -- the picture the game draws it with. Every other idea has no such
            -- icon and is shown by its picture.
            let shown | id_law iidea = iconText idea_loc
                      | T.null ideaIcon = ""
                      | otherwise = "[[File:" <> ideaIcon <> ".png|28px]]"
            return $ Just (category, shown, ideaKey, idea_loc, effectboxNS)
        Nothing -> case HM.lookup ide charto of
            Nothing -> return Nothing
            Just cchat -> do
                let namekey = adv_cha_id cchat
                mloc <- getGameL10nIfPresent $ adv_cha_name cchat
                name_loc <- case mloc of
                    Just nloc -> return nloc
                    _ -> getGameL10n $ adv_idea_token cchat
                slot <- getGameL10n (adv_advisor_slot cchat)
                return $ Just (slot, "", namekey, name_loc, Nothing)


-- | The picture the game shows an idea with, falling back on the generic one.
getIdeaIcon :: (HOI4Info g, Monad m) => HOI4Idea -> PPT g m Text
getIdeaIcon iidea = do
    micon <- getGameInterfaceIfPresent ("GFX_idea_" <> id_id iidea)
    case micon of
        Nothing -> getGameInterfaceNamed (id_picture iidea)
        Just idicon -> return idicon

-- | What an idea named by a @show_ideas_tooltip@ grants. An idea in the idea
-- files gets the same effect box as one that is handed out for good; an advisor
-- has no such box, since their entry holds little beyond their name and the
-- trait that comes with the post.
showIdea :: (HOI4Info g, Monad m) => StatementHandler g m
showIdea = showIdeaWith id

-- | As 'showIdea', for an idea listed under a heading announcing its slot. An
-- effect box is a block in its own right, so it stays at the heading's level,
-- while everything shown as a list item belongs one level under it.
showIdeaUnderHeading :: (HOI4Info g, Monad m) => StatementHandler g m
showIdeaUnderHeading = showIdeaWith indentUp

showIdeaWith :: (HOI4Info g, Monad m) =>
    (PPT g m IndentedMessages -> PPT g m IndentedMessages) -> StatementHandler g m
showIdeaWith nest stmt@[pdx| %_ = $idea |] = do
    ides <- getIdeas
    charto <- getCharToken
    case HM.lookup idea ides of
        Just iidea -> do
            idea_loc <- getGameL10n (id_name iidea)
            ideaIcon <- getIdeaIcon iidea
            modmessage iidea idea_loc (id_id iidea) ideaIcon
        Nothing -> case HM.lookup idea charto of
            Just ccharto -> nest $ showAdvisor idea ccharto
            -- Script also points this at things that are not ideas at all, a
            -- military industrial organization most often. There is nothing to
            -- list for those, but they are localized, so they can at least be
            -- named rather than left as script.
            Nothing -> nest $ mioTooltip MsgShowMio stmt
showIdeaWith _ stmt = preStatement stmt

-- | Name an advisor, with the trait their post comes with, and list everything
-- the post grants under them. Which of the modifiers the trait rather than the
-- advisor's own entry supplies is of no interest on the wiki, so they are shown
-- as one list.
showAdvisor :: (HOI4Info g, Monad m) => Text -> HOI4Advisor -> PPT g m IndentedMessages
showAdvisor token adv = do
    let traits = fromMaybe [] (adv_traits adv)
    mloc <- getGameL10nIfPresent (adv_cha_name adv)
    name_loc <- maybe (getGameL10n (adv_idea_token adv)) return mloc
    -- Some trait names are laid out over two lines ("Armor\n(Expert)"), which
    -- would end the list item they are written into.
    traitlocs <- traverse (fmap Doc.oneLine . getGameL10n) traits
    basemsg <- msgToPP $ MsgShowAdvisor name_loc token (T.intercalate ", " traitlocs)
    bonusmsg <- indentUp $ do
        traitmsg <- concatMapM getLeaderTraits traits
        modmsg <- maybe (return []) handleModifier (adv_modifier adv)
        resmsg <- maybe (return []) handleResearchBonus (adv_research_bonus adv)
        return $ traitmsg ++ modmsg ++ resmsg
    return $ basemsg ++ bonusmsg

modmessage :: forall g m. (HOI4Info g, Monad m) => HOI4Idea -> Text -> Text -> Text -> PPT g m IndentedMessages
modmessage iidea idea_loc ideaKey ideaIcon = do
        curind <- getCurrentIndent
        curindent <- case curind of
            Just curindt -> return curindt
            _ -> return 1
        withCurrentIndentCustom 1 $ \_ -> do
            ideaDesc <- case id_desc_loc iidea of
                Just desc -> return $ Doc.nl2br desc
                _ -> return ""
            -- A company's or an advisor's trait names the kind of thing it is
            -- ("Railway Company"), which the box's own title already conveys;
            -- only a national spirit's traits are worth a heading of their own.
            let namedTraits = id_category iidea == "country"
            traitmsg <- case id_traits iidea of
                Just arr -> do
                    let traitbare = mapMaybe getbaretraits arr
                    concatMapM (\t-> if not namedTraits then getLeaderTraits t else do
                        traitloc <- Doc.oneLine <$> getGameL10n t
                        namemsg <- plainMsg' ("'''" <> traitloc <> "'''")
                        traitmsg <- indentUp $ getLeaderTraits t
                        return $ namemsg : traitmsg) traitbare
                _-> return []
            modifier <- maybe (return []) ppOne (id_modifier iidea)
            targeted_modifier <-
                maybe (return []) (concatMapM handleTargetedModifier) (id_targeted_modifier iidea)
            research_bonus <- maybe (return []) ppOne (id_research_bonus iidea)
            equipment_bonus <- maybe (return []) ppOne (id_equipment_bonus iidea)
            let boxend = [(0, MsgEffectBoxEnd curindent)]
            withCurrentIndentCustom curindent $ \_ -> do
                let ideamods = traitmsg ++modifier ++ targeted_modifier ++ research_bonus ++ equipment_bonus ++ boxend
                return $ (0, MsgEffectBox idea_loc ideaKey ideaIcon ideaDesc) : ideamods


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
        else trace ("unknown modifier type: " ++ show specmod ++ " IN: " ++ show stmt) $ preStatement stmt
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
        addLine adm stmt = trace ("Unknown in add_dynamic_modifier: " ++ show stmt) adm
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
addDynamicModifier stmt = trace ("Not handled in addDynamicModifier: " ++ show stmt) $ preStatement stmt

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

-------------------------------------------------------
-- Runs of statements that only make sense together  --
-------------------------------------------------------

-- | How a run of variable writes changes the dynamic modifier behind them.
data DynModOp = DynModSet | DynModAdd | DynModSub deriving (Eq)

-- | An effect script, with the runs of statements that only say something
-- together pulled out of it.
data ScriptChunk
    = PlainStmt GenericStatement
    | DynModChunk HOI4DynamicModifier Bool [(Text, Double)]
      -- ^ The modifier, whether its values are set rather than added to, and
      --   the modifier keys affected with their new values.
    | IdeaSlotChunk GenericStatement [GenericStatement]
      -- ^ The tooltip announcing that a slot's ideas change, and the
      --   @show_ideas_tooltip@ statements naming the ideas it announces.
    | StateChunk Text IndentedMessages
      -- ^ The states a run of scopes names, written out as one, and what the
      --   block every one of them holds comes to.

-- | Split a script into the chunks that are shown as a whole.
chunkScript :: (HOI4Info g, Monad m) => GenericScript -> PPT g m [ScriptChunk]
chunkScript scr = chunkStates =<< chunkIdeaSlots <$> chunkDynModVars scr

-- | Group each run of consecutive state scopes that come to the same thing, so
-- that what befalls all of them is said once with the states named together,
-- the way the game says it.
--
-- What is compared is what the blocks come to, not how script writes them:
-- script often says a thing of several states in wording that differs in some
-- part the wiki does not show, and two states the wiki says the very same thing
-- of are worth naming together however differently they were written. Each
-- block is written out the once, here, and the run keeps what it came to.
--
-- A state scope standing on its own is left as the plain statement it was.
chunkStates :: (HOI4Info g, Monad m) => [ScriptChunk] -> PPT g m [ScriptChunk]
chunkStates chunks = do
    quiet <- traverse saysNothing chunks
    concat <$> traverse chunkRun (groupBy alongside (zip chunks quiet))
    where
        -- What says nothing stands between two states without parting them:
        -- script spaces its blocks out with tooltips whose text is empty, and
        -- a run the game shows as one should not be broken up by a blank line
        -- that the wiki does not draw in the first place.
        alongside (one, _) (two, quiettwo) =
            isJust (stateScope one) && (quiettwo || isJust (stateScope two))
        chunkRun run = case mapMaybe (stateScope . fst) run of
            states@(_:_:_) -> traverse together . groupBy sameSaid =<< traverse said states
            _ -> return (map fst run)
        said (n, block) = do
            block_pp <- scope HOI4ScopeState (ppMany block)
            saidas <- imsg2doc block_pp
            return (n, Doc.doc2text saidas, block_pp)
        sameSaid (_, one, _) (_, two, _) = one == two
        together shared = do
            heading <- case [n | (n, _, _) <- shared] of
                [n] -> getStateLoc n
                ns -> return (Doc.doc2text (template "states" (map (T.pack . show) ns)))
            return (StateChunk heading (said_pp (head shared)))
        said_pp (_, _, block_pp) = block_pp
        stateScope (PlainStmt (Statement (IntLhs n) OpEq (CompoundRhs block))) = Just (n, block)
        stateScope _ = Nothing

-- | Whether a chunk comes to nothing a reader sees. Only the tooltips are asked,
-- since a tooltip with no text is how script spaces its blocks out and is the
-- one thing written to be seen and yet show nothing.
saysNothing :: (HOI4Info g, Monad m) => ScriptChunk -> PPT g m Bool
saysNothing (PlainStmt stmt@[pdx| custom_effect_tooltip = %_ |]) = null <$> ppOne stmt
saysNothing (PlainStmt stmt@[pdx| tooltip = %_ |]) = null <$> ppOne stmt
saysNothing _ = return False

-- | Group each tooltip saying that ideas become available in (or leave) an
-- advisor or company slot with the @show_ideas_tooltip@ statements it heads, so
-- that the ideas can be listed under it. A tooltip that heads nothing is left
-- where it was, to be shown as the ordinary tooltip it is.
chunkIdeaSlots :: [ScriptChunk] -> [ScriptChunk]
chunkIdeaSlots = reverse . foldl' addChunk []
    where
        addChunk chunks chunk = case chunk of
            PlainStmt stmt
                | isSlotTooltip stmt -> IdeaSlotChunk stmt [] : chunks
                | isShownIdea stmt
                , IdeaSlotChunk tt ideas : rest <- chunks ->
                    IdeaSlotChunk tt (ideas ++ [stmt]) : rest
            _ -> chunk : chunks
        -- The slot a run of ideas belongs to is named by the tooltip's key, and
        -- the game keeps to @available_@ and @remove_@ for those.
        isSlotTooltip [pdx| custom_effect_tooltip = $key |] =
            any (`T.isPrefixOf` key) ["available_", "remove_"]
        isSlotTooltip _ = False
        isShownIdea [pdx| show_ideas_tooltip = $_ |] = True
        isShownIdea _ = False

-- | Present the tooltip announcing a change to an advisor or company slot as a
-- heading for the ideas it names, and those ideas under it. The trailing
-- newline that spaces the tooltip out in game is noise on the wiki, and so is
-- the @Custom effect tooltip:@ that an ordinary tooltip is labelled with: what
-- follows the heading says plainly enough that it is one.
ppIdeaSlotChunk :: forall g m. (HOI4Info g, Monad m) =>
    GenericStatement -> [GenericStatement] -> PPT g m IndentedMessages
ppIdeaSlotChunk [pdx| %_ = $key |] ideas@(_:_) = do
    loc <- T.strip <$> getGameL10n key
    -- Without a heading there is nothing for the ideas to be listed under.
    if T.null loc then concatMapM showIdea ideas else do
        headmsg <- plainMsg loc
        ideamsg <- concatMapM showIdeaUnderHeading ideas
        return $ headmsg ++ ideamsg
ppIdeaSlotChunk tt ideas = concatMapM ppOne (tt : ideas)

-- | Split a script into chunks, grouping each run of consecutive statements
-- that write to the variables of one and the same dynamic modifier.
chunkDynModVars :: (HOI4Info g, Monad m) => GenericScript -> PPT g m [ScriptChunk]
chunkDynModVars scr
    | not (any (isJust . dynModVarOp) scr) = return (map PlainStmt scr)
    | otherwise = do
        varTable <- dynModVarTable <$> getDynamicModifiers
        return $ reverse $ foldl' (addChunk varTable) [] scr
    where
        addChunk table chunks stmt = case resolve table =<< dynModVarOp stmt of
            Nothing -> PlainStmt stmt : chunks
            Just (dmod, key, isSet, val) -> case chunks of
                DynModChunk dmod' isSet' mods : rest
                    | dmodName dmod' == dmodName dmod && isSet' == isSet ->
                        DynModChunk dmod' isSet' (mods ++ [(key, val)]) : rest
                -- The tooltip right before such a run is the game's way of
                -- announcing which modifier is about to change; the effect box
                -- says as much, so drop it rather than say it twice.
                PlainStmt prev : rest | isCustomTooltip prev ->
                    DynModChunk dmod isSet [(key, val)] : rest
                _ -> DynModChunk dmod isSet [(key, val)] : chunks
        resolve table (op, var, val) = do
            (dmod, key) <- HM.lookup var table
            return (dmod, key, op == DynModSet, if op == DynModSub then negate val else val)
        isCustomTooltip [pdx| custom_effect_tooltip = %_ |] = True
        isCustomTooltip _ = False

-- | Map each variable that a dynamic modifier reads a value from to that
-- modifier and the modifier key it supplies.
dynModVarTable :: HashMap Text HOI4DynamicModifier -> HashMap Text (HOI4DynamicModifier, Text)
dynModVarTable = HM.fromList . concatMap entries . HM.elems
    where
        entries dmod = mapMaybe (entry dmod) (dmodEffects dmod)
        entry dmod [pdx| $key = $var |] = Just (var, (dmod, key))
        entry _ _ = Nothing

-- | Recognise an effect that writes a plain number to a single variable, i.e.
-- @add_to_variable = { some_var = 0.025 }@ or the @var@/@value@ spelling of it.
-- Anything more involved is left to the ordinary variable handler.
dynModVarOp :: GenericStatement -> Maybe (DynModOp, Text, Double)
dynModVarOp [pdx| $lhs = @scr |] = case theop of
        Nothing -> Nothing
        Just op -> case foldl' addLine (Nothing, Nothing, False) scr of
            (Just var, Just val, False) -> Just (op, var, val)
            _ -> Nothing
    where
        theop = case lhs of
            "set_variable" -> Just DynModSet
            "add_to_variable" -> Just DynModAdd
            "subtract_from_variable" -> Just DynModSub
            _ -> Nothing
        -- The third component flags anything unexpected in the statement.
        addLine acc@(mvar, mval, bad) stmt = case stmt of
            [pdx| var = ?v |]
                | isNothing mvar -> (Just v, mval, bad)
            [pdx| value = !n |]
                | isNothing mval -> (mvar, Just n, bad)
            [pdx| tooltip = %_ |] -> acc
            [pdx| $v = !n |]
                | isNothing mvar && isNothing mval -> (Just v, Just n, bad)
            _ -> (mvar, mval, True)
dynModVarOp _ = Nothing

-- | Present a run of writes to a dynamic modifier's variables as an effect box
-- listing what the modifier grants after the change.
ppDynModChunk :: forall g m. (HOI4Info g, Monad m) =>
    HOI4DynamicModifier -> Bool -> [(Text, Double)] -> PPT g m IndentedMessages
ppDynModChunk dmod isSet mods = do
    headmsg <- msgToPP $ if isSet then MsgSetDynamicModifier else MsgModifyDynamicModifier
    box <- ppDynModBox dmod (map modStmt mods)
    return (headmsg ++ box)
    where
        modStmt (key, val) = Statement (GenericLhs key []) OpEq (FloatRhs val)


hasDynamicModifier :: (HOI4Info g, Monad m) => StatementHandler g m
hasDynamicModifier stmt@[pdx| %_ = @dyn |] = if length dyn == 2
    then textAtom "scope" "modifier" MsgHasDynamicModFlag flagMaybeText stmt
    else case dyn of
        [stmtd@[pdx| %_ = $txt |]] ->  withLocAtom MsgHasDynamicMod stmtd
        _-> preStatement stmt
hasDynamicModifier stmt = preStatement stmt

--------------------------------------------
-- Handler for add_power_balance_modifier --
--------------------------------------------

addPowerBalanceModifier :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addPowerBalanceModifier stmt@[pdx| %_ = @scr |] =
    pp_ta (parseTA "id" "modifier" scr)
    where
        pp_ta :: TextAtom -> PPT g m IndentedMessages
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just idpob, Just modi) -> do
                mmod <- HM.lookup modi <$> getModifiers
                midpob_loc <- getGameL10nIfPresent idpob
                let idpob_loc = fromMaybe (typewriterText idpob) midpob_loc
                case mmod of
                    Just mod -> withCurrentIndent $ \i -> do
                        effect <- fold <$> indentUp (traverse (modifierMSG False "") (modEffects mod))
                        let name = modLocName mod
                            locName = maybe (typewriterText modi) (Doc.doc2text . iquotes) name
                        return ((i, MsgAddPowerBalanceModifier idpob_loc idpob locName modi) : effect)
                    _ -> trace ("add_power_balance_modifier: Modifier " ++ T.unpack modi ++ " not found") $ preStatement stmt
            _-> preStatement stmt
addPowerBalanceModifier stmt = trace ("Not handled in addPowerBalanceModifier: " ++ show stmt) $ preStatement stmt


-----------------------------------------
-- Handler for the relation modifiers  --
-----------------------------------------

-- | Handler for @add_relation_modifier@ and the two statements that go with it.
-- An opinion modifier is only a number moving two countries towards or away from
-- each other; a relation modifier is one of the static modifiers, and what it
-- grants holds for as long as the relation between the two does, so that is
-- written out under it.
relationModifier :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> Bool -> StatementHandler g m
relationModifier msg witheffects stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (Just ewhom, Just modid) -> do
            mwhomflag <- eflag (Just HOI4Country) ewhom
            let whomflag = fromMaybe "<!-- check script -->" mwhomflag
            mmod <- HM.lookup modid <$> getModifiers
            case mmod of
                Just mod -> withCurrentIndent $ \i -> do
                    -- Taking the modifier away, or asking whether it is there,
                    -- says nothing new about what it grants.
                    effect <- if witheffects
                        then fold <$> indentUp (traverse (modifierMSG False "") (modEffects mod))
                        else return []
                    let locName = maybe (typewriterText modid) (Doc.doc2text . iquotes) (modLocName mod)
                    return ((i, msg locName whomflag) : effect)
                Nothing -> trace ("relation modifier not found: " ++ T.unpack modid) $ preStatement stmt
        _ -> trace ("relation modifier: target or modifier missing: " ++ show stmt) $ preStatement stmt
    where
        addLine (whom, modid) [pdx| target = $tag |] = (Just (Left tag), modid)
        addLine (whom, modid) [pdx| target = $vartag:$var |] = (Just (Right (vartag, var)), modid)
        addLine (whom, modid) [pdx| modifier = ?label |] = (whom, Just label)
        addLine acc _ = acc
relationModifier _ _ stmt = preStatement stmt


----------------
-- characters --
----------------

addFieldMarshalRole :: (Monad m, HOI4Info g) => (Text -> ScriptMessage) -> StatementHandler g m
addFieldMarshalRole msg stmt@[pdx| %_ = @scr |] = do
        let (name, _) = extractStmt (matchLhsText "character") scr
        nameloc <- case name of
            Just [pdx| character = ?id |] -> getCharacterName id
            _ -> case extractStmt (matchLhsText "name") scr of
                (Just [pdx| name = ?id |],_) -> getCharacterName id
                _-> return ""
        msgToPP $ msg nameloc
addFieldMarshalRole _ stmt = preStatement stmt

setCharacterName :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setCharacterName stmt@[pdx| %_ = ?txt |] = withLocAtom MsgSetCharacterName stmt
setCharacterName stmt@[pdx| %_ = @scr |] = case scr of
    [[pdx| $who = $name |]] -> do
        whochar <- getCharacterName who
        nameloc <- getGameL10n name
        msgToPP $ MsgSetCharacterNameType whochar nameloc
    _ -> preStatement stmt
setCharacterName stmt = preStatement stmt

removeAdvisorRole :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
removeAdvisorRole stmt@[pdx| %_ = @scr |] =
    if length scr == 2
    then textAtom "character" "slot" MsgRemoveAdvisorRole getGameL10nIfPresent stmt
    else do
        let (mslot,_) = extractStmt (matchLhsText "slot") scr
        slot <- case mslot of
            Just [pdx| %_ = $slottype |] -> getGameL10n slottype
            _-> return "<!-- Check Script -->"
        msgToPP $ MsgRemoveAdvisorRole "" "" slot
removeAdvisorRole stmt = preStatement stmt

withCharacter :: (HOI4Info g, Monad m) => (Text -> ScriptMessage) -> StatementHandler g m
withCharacter = withLookupAtom getCharacterName

addAdvisorRole :: (Monad m, HOI4Info g) => StatementHandler g m
addAdvisorRole stmt@[pdx| %_ = @scr |] = do
        let (name, rest) = extractStmt (matchLhsText "character") scr
            (advisor, rest') = extractStmt (matchLhsText "advisor") rest
            (activate, _) = extractStmt (matchLhsText "activate") rest'
        activate <- maybe (return False) (\case
            [pdx| %_ = yes |] -> return True
            _-> return False) activate
        nameloc <- case name of
            Just [pdx| character = $id |] -> getCharacterName id
            _ -> return ""
        case advisor of
            Just advisorj -> do
                (slotloc, traitmsg) <- parseAdvisor advisorj
                basemsg <- msgToPP $ MsgAddAdvisorRole nameloc slotloc
                (if activate
                then do
                    hiremsg <- msgToPP MsgAndIsHired
                    return $ basemsg ++ traitmsg ++ hiremsg
                else return $ basemsg ++ traitmsg)
            _-> preStatement stmt
addAdvisorRole stmt = preStatement stmt

parseAdvisor :: (Monad m, HOI4Info g) =>
    GenericStatement -> PPT g m (Text, [IndentedMessage])
parseAdvisor stmt@[pdx| %_ = @scr |] = do
    let (slot, rest) = extractStmt (matchLhsText "slot") scr
        (traits, modrest) = extractStmt (matchLhsText "traits") rest
        (modifier, bonusrest) = extractStmt (matchLhsText "modifier") modrest
        (resbonus, _) = extractStmt (matchLhsText "research_bonus") bonusrest
    modmsg <- maybe (return []) (indentUp . handleModifier) modifier
    resmsg <- maybe (return []) (indentUp . handleResearchBonus) resbonus
    traitmsg <- case traits of
        Just [pdx| %_ = @arr |] -> do
            let traitbare = mapMaybe getbaretraits arr
            concatMapM (indentUp . getLeaderTraits) traitbare
        _-> return []
    slotloc <- maybe (return "") (\case
        [pdx| %_ = $slottype|] -> getGameL10n slottype
        _->return "<!-- Check Script -->") slot

    return (slotloc, traitmsg ++ modmsg ++ resmsg)
parseAdvisor stmt = return ("<!-- Check Script -->", [])

-- | The party a character is written to lead or belong to, as the wiki's icon
-- for it. Script writes the sub-ideology a leader is filed under -- despotism,
-- leninism -- and the party is named for the ideology that belongs to.
partyIcon :: (Monad m, HOI4Info g) => Text -> PPT g m Text
partyIcon subideo = do
    subideos <- getIdeology
    maybe (return "<!-- Check Script -->") partyIconOf (HM.lookup subideo subideos)

-- | The party the character the script has scoped to is written to lead, or
-- nothing where the script is not about a character or their entry names no
-- party.
scopeParty :: (Monad m, HOI4Info g) => PPT g m (Maybe Text)
scopeParty = do
    chas <- getCharacters
    inscope <- getCurrentCharacter
    let msub = cha_leader_ideology =<< (flip HM.lookup chas =<< inscope)
    traverse partyIcon msub

addLeaderRole :: (Monad m, HOI4Info g) => StatementHandler g m
addLeaderRole stmt@[pdx| %_ = @scr |] = do
        let (name, rest) = extractStmt (matchLhsText "character") scr
            (leader, rest') = extractStmt (matchLhsText "country_leader") rest
            (promote, _) = extractStmt (matchLhsText "promote_leader") rest'
        promoted <- maybe (return False) (\case
            [pdx| %_ = yes |] -> return True
            _-> return False) promote
        nameloc <- case name of
            Just [pdx| character = $id |] -> getCharacterName id
            _ -> return ""
        case leader of
            Just leaderj -> do
                (ideoloc, traitmsg) <- parseLeader leaderj
                basemsg <- if promoted
                    then msgToPP $ MsgAddCountryLeaderRolePromoted nameloc ideoloc
                    else msgToPP $ MsgAddCountryLeaderRole nameloc ideoloc
                return $ basemsg ++ traitmsg
            _-> preStatement stmt
addLeaderRole stmt = preStatement stmt

parseLeader :: (Monad m, HOI4Info g) =>
    GenericStatement -> PPT g m (Text, [IndentedMessage])
parseLeader stmt@[pdx| %_ = @scr |] = do
    let (ideo, rest) = extractStmt (matchLhsText "ideology") scr
        (traits, _) = extractStmt (matchLhsText "traits") rest
    traitmsg <- case traits of
        Just [pdx| %_ = @arr |] -> do
            let traitbare = mapMaybe getbaretraits arr
            concatMapM ppHt traitbare
        _-> return []
    ideoloc <- maybe (return "") (\case
        [pdx| %_ = $ideotype|] -> partyIcon ideotype
        _->return "<!-- Check Script -->") ideo
    return (ideoloc, traitmsg)
parseLeader stmt = return ("<!-- Check Script -->", [])


createLeader :: (Monad m, HOI4Info g) => StatementHandler g m
createLeader stmt@[pdx| %_ = @scr |] = do
        let (name, _) = extractStmt (matchLhsText "name") scr
        nameloc <- case name of
            Just [pdx| %_ = ?id |] -> getCharacterName id
            _ -> return ""
        (ideoloc, traitmsg) <- parseLeader stmt
        basemsg <- msgToPP $ MsgAddCountryLeaderRole nameloc ideoloc
        return $ basemsg ++ traitmsg
createLeader stmt = preStatement stmt

promoteCharacter :: (Monad m, HOI4Info g) => StatementHandler g m
promoteCharacter stmt@[pdx| %_ = @scr |] =
    ppPC (parseTA "character" "ideology" scr)
    where
        ppPC ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> promomessage what atom stmt
            (_, Just atom) -> promomessage "" atom stmt
            _ -> preStatement stmt
promoteCharacter stmt@[pdx| %_ = $txt |]
    -- The character is whoever the script has scoped to, who the scope names
    -- already, so the line says only what becomes of them.
    | txt == "yes" = do
        mparty <- scopeParty
        msgToPP $ maybe (MsgPromoteCharacter "") (MsgAddCountryLeaderRolePromoted "") mparty
    | otherwise = do
        chas <- getCharacters
        subideos <- getIdeology
        case HM.lookup txt subideos of
            Just ideo -> promomessage "" txt stmt
            _-> case HM.lookup txt chas of
                Just ccha -> promomessage txt "" stmt
                _-> preStatement stmt
promoteCharacter stmt = preStatement stmt

promomessage :: (Monad m, HOI4Info g) => Text
    -> Text-> StatementHandler g m
promomessage what atom stmt = do
    chas <- getCharacters
    let mcha = HM.lookup what chas
    -- The party the character comes to lead: the one the statement names, or
    -- failing that the one their own entry writes them for.
    party <- case (atom, cha_leader_ideology =<< mcha) of
        (a, _) | not (T.null a) -> partyIcon a
        (_, Just own) -> partyIcon own
        _ -> return ""
    case mcha of
        Just ccha -> do
            let nameloc = cha_loc_name ccha
            traitmsg <- case cha_leader_traits ccha of
                Just trts -> do
                    concatMapM ppHt trts
                _-> return []
            basemsg <- if T.null party
                then msgToPP $ MsgPromoteCharacter nameloc
                else msgToPP $ MsgAddCountryLeaderRolePromoted nameloc party
            return $ basemsg ++ traitmsg
        _-> if not (T.null what)
            then preStatement stmt
            else msgToPP $ MsgAddCountryLeaderRolePromoted "" party

ppHt :: (Monad m, HOI4Info g) => Text -> PPT g m IndentedMessages
ppHt trait = do
    traitloc <- Doc.oneLine <$> getGameL10n trait
    namemsg <- indentUp $ plainMsg' ("'''" <> traitloc <> "'''")
    traitmsg' <- indentUp $ indentUp $ getLeaderTraits trait
    return $ namemsg : traitmsg'

getbaretraits :: GenericStatement -> Maybe Text
getbaretraits (StatementBare (GenericLhs trait [])) = Just trait
getbaretraits stmt = Nothing





----------------------------------------
-- Handlers for the doctrine mastery  --
----------------------------------------

-- | The name a part of the doctrine tree is written under, or 'Nothing' when no
-- key matches. Script names a folder, a track, a subdoctrine or a grand doctrine
-- by its id, and the game localizes each of them under a key built from that id
-- in a settled way. Two of those ways have exceptions to them often enough to be
-- worth following: the grand doctrines reworked for the doctrine tree keep a
-- @new_@ on the id that the key does not, and the air subdoctrines say
-- @air_subdoctrine_@ in the id where the key just says which one it is. The
-- naval subdoctrines take the name of their track on the front of the key
-- instead of the id, so those are tried as well.
--
-- Nothing here reads the doctrine files: one localization key is cheaper to keep
-- up with than another set of game files, and a part whose key is simply
-- misnamed comes out under its id, which says plainly enough what to fix.
doctrineLocLookup :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m (Maybe Text)
doctrineLocLookup kind theid = go (candidates kind (T.toLower theid))
    where
        go [] = return Nothing
        go (key:rest) = do
            mloc <- getGameL10nIfPresent key
            case mloc of
                Just loc | not (T.null loc) -> Just <$> getGameL10n key
                _ -> go rest
        candidates "folder" i = [i <> "_doctrine_folder"]
        candidates "track" i = ["DOCTRINE_TRACK_" <> T.toUpper i]
        candidates "grand_doctrine" i =
            ["GRAND_DOCTRINE_" <> T.toUpper (fromMaybe i (T.stripPrefix "new_" i))
            ,"GRAND_DOCTRINE_" <> T.toUpper i]
        candidates _ i =
            let bare = fromMaybe i (T.stripPrefix "air_subdoctrine_" i)
                stem = fromMaybe bare (T.stripSuffix "_no_lar" bare)
            in ["SUBDOCTRINE_" <> T.toUpper stem]
               ++ [ "SUBDOCTRINE_" <> t <> T.toUpper stem
                  | t <- ["SCREEN_", "SUBMARINE_", "CARRIER_"] ]

-- | A wiki link to a heading on a page. The heading a link jumps to is written
-- with underscores where the name has spaces; a percent escape is not followed
-- here.
sectionLink :: Text -> Text -> Text -> Text
sectionLink page heading name = mconcat
    [ "[[", page, "#", T.replace " " "_" heading, "|", name, "]]" ]

-- | A link to the part of the doctrine tree script has named, on the wiki page
-- for the folder it belongs to. A folder is the page itself; anything under one
-- is a heading on it.
doctrineLink :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m Text
doctrineLink "folder" theid = return ("[[" <> doctrinePage theid <> "]]")
doctrineLink kind theid = do
    mname <- doctrineLocLookup kind theid
    case (mname, HM.lookup (T.toLower theid) doctrineFolders) of
        (Just name, Just folder) -> return $ sectionLink (doctrinePage folder) name name
        -- A part we know the page of but not the name, or the name but not the
        -- page, has nothing to make a heading out of, so it is left as the id
        -- script called it by. That is also what says which key to go and fix.
        (Just name, Nothing) -> return name
        _ -> return (typewriterText theid)




-- | The part of the doctrine tree a @..._mastery_gain_factor@ modifier is about,
-- together with the sentence the game words that modifier with. The modifier is
-- named after the part with nothing to say which kind of part it is, beyond a
-- @_track@ that the tracks carry, so the other kinds are tried in turn.
masteryGainModifier :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe (Text, Text))
masteryGainModifier lmod = case T.stripSuffix "_mastery_gain_factor" lmod of
    Nothing -> return Nothing
    Just stem -> case T.stripSuffix "_track" stem of
        Just track -> named "mod_track_mastery_gain_factor" "track" track
        Nothing -> go
            [ ("mod_folder_mastery_gain_factor", "folder")
            , ("mod_grand_doctrine_mastery_gain_factor", "grand_doctrine")
            , ("mod_subdoctrine_mastery_gain_factor", "sub_doctrine") ]
            stem
    where
        go [] _ = return Nothing
        go ((wrapper, kind):rest) stem = do
            found <- named wrapper kind stem
            maybe (go rest stem) (return . Just) found
        named wrapper kind theid = fmap ((,) wrapper) <$> doctrineLocLookup kind theid

-- | Handler for @add_mastery@ and @add_daily_mastery@, which hand a country
-- mastery towards part of its doctrine tree, in one go or so much a day for a
-- while.
-- | The shape the mastery effects share: an amount under some label, maybe a
-- number of days, and conditions saying which part of the tree is meant. The
-- message is picked from the amount and days; 'Nothing' falls back to the raw
-- statement.
masteryStmt :: forall g m. (HOI4Info g, Monad m) =>
    Text -- ^ label the amount is written under
    -> String -- ^ effect name, for trace messages
    -> (Maybe Double -> Maybe Double -> Maybe (Text -> ScriptMessage))
    -> StatementHandler g m
masteryStmt amtlabel what mkmsg stmt@[pdx| %_ = @scr |] =
    case mkmsg amount days of
        Just msg -> masteryTargets msg targets
        _ -> preStatement stmt
    where
        (amount, days, targets) = foldl' addLine (Nothing, Nothing, []) scr
        addLine (amt, d, t) [pdx| $lbl = !n |]
            | lbl == amtlabel = (Just n, d, t)
            | lbl == "days" = (amt, Just n, t)
        -- The name is what a later effect takes the mastery away by, and says
        -- nothing to a reader.
        addLine acc [pdx| name = %_ |] = acc
        addLine (amt, d, ts) [pdx| $kind = $theid |]
            | kind `elem` masteryConditions = (amt, d, ts ++ [(kind, theid)])
        addLine acc stmt = trace ("unknown section in " ++ what ++ ": " ++ show stmt) acc
masteryStmt _ _ _ stmt = preStatement stmt

addMastery :: forall g m. (HOI4Info g, Monad m) => Bool -> StatementHandler g m
addMastery daily = masteryStmt "amount" "add_mastery" $ \mamt mdays -> case mamt of
    Just amt | not daily -> Just (MsgAddMastery amt)
             | Just d <- mdays -> Just (MsgAddDailyMastery amt d)
    _ -> Nothing

-- | Handler for @add_mastery_bonus@, which raises how fast mastery comes in for
-- part of the doctrine tree, for a while.
addMasteryBonus :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addMasteryBonus = masteryStmt "bonus" "add_mastery_bonus" $ \mb md -> case (mb, md) of
    (Just b, Just d) -> Just (MsgAddMasteryBonus b d)
    _ -> Nothing

-- | The ways script narrows down which part of the doctrine tree a mastery
-- effect is aimed at, in the order the game sets them out in.
masteryConditions :: [Text]
masteryConditions = ["folder", "grand_doctrine", "sub_doctrine", "track"]

-- | How the part of the doctrine tree a mastery effect is aimed at is written
-- out. Script may narrow it down in more than one way at once -- a track of a
-- given type, under a given grand doctrine -- and the game sets those out one
-- to a line below the effect, since none of them alone says where the mastery
-- goes. A single one is said in the same breath as the effect, and with none at
-- all the mastery goes to the whole tree.
masteryTargets :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> [(Text, Text)] -> PPT g m IndentedMessages
masteryTargets msg [] = msgToPP (msg "every doctrine")
masteryTargets msg [cond] = msgToPP . msg =<< masteryTarget cond
masteryTargets msg conds = do
    header <- msgToPP (msg "all tracks that:")
    said <- indentUp (traverse conditionLine (sortOn order conds))
    return (header ++ said)
    where
        order (kind, _) = fromMaybe (length masteryConditions) (elemIndex kind masteryConditions)
        conditionLine (kind, theid) = plainMsg' . conditionText kind =<< doctrineLink kind theid
        conditionText "folder" link = "Are in the " <> link <> " folder"
        conditionText "grand_doctrine" link = "Have the " <> link <> " grand doctrine"
        conditionText "sub_doctrine" link = "Have the " <> link <> " subdoctrine"
        conditionText "track" link = "Are of type " <> link
        conditionText _ link = link

-- | How one way of narrowing down where the mastery goes is written out on its
-- own. A track holds a run of subdoctrines rather than being one thing, so it is
-- spoken of in the plural.
masteryTarget :: (HOI4Info g, Monad m) => (Text, Text) -> PPT g m Text
masteryTarget ("track", theid) = do
    link <- doctrineLink "track" theid
    return ("all " <> link <> " tracks")
masteryTarget (kind, theid) = doctrineLink kind theid

-------------------------------
-- Handlers for advisor posts --
-------------------------------



-- | Handler for @activate_advisor@ and @deactivate_advisor@, which put someone
-- into one of the country's advisor posts and take them out of it again.
advisorPost :: (HOI4Info g, Monad m) => (Text -> ScriptMessage) -> StatementHandler g m
advisorPost msg [pdx| %_ = $token |] = msgToPP . msg =<< advisorName token
advisorPost _ stmt = preStatement stmt

-- | Handler for @set_can_be_fired_in_advisor_role@, which decides whether the
-- player may dismiss someone from a post they hold. Script may leave the
-- character out, meaning whoever the surrounding scope is about, and may leave
-- the slot out, meaning every post they hold.
setCanBeFiredInAdvisorRole :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setCanBeFiredInAdvisorRole stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing) scr of
        (mchar, mslot, Just value) -> do
            who <- maybe (return "") advisorName mchar
            slot <- maybe (return "") getGameL10n mslot
            msgToPP $ MsgSetCanBeFiredInAdvisorRole who slot value
        _ -> preStatement stmt
    where
        addLine (mchar, mslot, value) [pdx| character = $char |] = (Just char, mslot, value)
        addLine (mchar, mslot, value) [pdx| slot = $slot |] = (mchar, Just slot, value)
        addLine (mchar, mslot, value) [pdx| value = yes |] = (mchar, mslot, Just True)
        addLine (mchar, mslot, value) [pdx| value = no |] = (mchar, mslot, Just False)
        addLine acc stmt = trace ("unknown section in set_can_be_fired_in_advisor_role: " ++ show stmt) acc
setCanBeFiredInAdvisorRole stmt = preStatement stmt

----------------------------------------
-- Handler for add_relation_rule_override --
----------------------------------------

-- | Handler for @add_relation_rule_override@, which lifts or imposes one of the
-- rules that would otherwise settle what two countries may do with each other.
--
-- Script may hang the override on a trigger rather than on one named country, and
-- where it does it writes out a line of its own saying when the rule applies;
-- that says it better than anything assembled from the rule names would.
addRelationRuleOverride :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addRelationRuleOverride stmt@[pdx| %_ = @scr |] =
    case (mdesc, rules) of
        (Just desc, _) -> tooltipText MsgRelationRuleOverrideDesc =<< locKeyText HM.empty desc
        (_, rules@(_:_)) -> do
            mwhomflag <- maybe (return Nothing) (eflag (Just HOI4Country)) mtarget
            let whomflag = fromMaybe "<!-- check script -->" mwhomflag
            fold <$> traverse (msgToPP . ($ whomflag) . uncurry rule) rules
        _ -> preStatement stmt
    where
        (mtarget, mdesc, rules) = foldl' addLine (Nothing, Nothing, []) scr
        addLine (target, desc, rs) [pdx| target = $tag |] = (Just (Left tag), desc, rs)
        addLine (target, desc, rs) [pdx| target = $vartag:$var |] = (Just (Right (vartag, var)), desc, rs)
        addLine (target, desc, rs) [pdx| usage_desc = ?key |] = (target, Just key, rs)
        -- The trigger deciding when the override holds is a named block of
        -- script, and the line under @usage_desc@ is the game's own reading of it.
        addLine acc [pdx| trigger = %_ |] = acc
        addLine (target, desc, rs) [pdx| $what = %rhs |]
            | GenericRhs "yes" [] <- rhs = (target, desc, rs ++ [(what, True)])
            | GenericRhs "no" [] <- rhs = (target, desc, rs ++ [(what, False)])
        addLine acc stmt = trace ("unknown section in add_relation_rule_override: " ++ show stmt) acc

        rule "can_send_volunteers" yn = MsgRelationRuleVolunteers yn
        rule "can_access_market" yn = MsgRelationRuleMarket yn
        rule what yn = MsgRelationRuleOther what yn
addRelationRuleOverride stmt = preStatement stmt

-- operatives

data CreateOperative = CreateOperative
        {   co_bypass_recruitment :: Bool
        ,   co_name :: Text
        ,   co_traits :: Maybe [Text]
        ,   co_nationalities :: Maybe [Text]
        ,   co_available_to_spy_master :: Bool
        }

newCO :: CreateOperative
newCO = CreateOperative False "" Nothing Nothing False

createOperativeLeader :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createOperativeLeader stmt@[pdx| %_ = @scr |]
    = ppCO (foldl' addLine newCO scr)
    where
        addLine :: CreateOperative -> GenericStatement -> CreateOperative
        addLine co [pdx| bypass_recruitment = %rhs |]
            | GenericRhs "yes" [] <- rhs = co { co_bypass_recruitment = True }
            | GenericRhs "no" [] <- rhs = co { co_bypass_recruitment = False }
        addLine co [pdx| name = ?txt |] = co {co_name = txt}
        addLine co [pdx| traits = @arr |] =
            let traits = mapMaybe getbaretraits arr
            in co {co_traits = Just traits}
        addLine co [pdx| nationalities = @arr |] =
            let nats = mapMaybe getbaretraits arr
            in co {co_nationalities = Just nats}
        addLine co [pdx| available_to_spy_master = %rhs |]
            | GenericRhs "yes" [] <- rhs = co { co_available_to_spy_master = True }
            | otherwise = co
        addLine co stmt = co

        ppCO co = do
            natmsg <- case co_nationalities co of
                    Just nats -> do
                        flagged <- mapM (flagText (Just HOI4Country)) nats
                        return $ T.intercalate ", " flagged
                    _ -> return ""
            basemsg <- msgToPP $ MsgCreateOperativeLeader (co_name co) natmsg (co_bypass_recruitment co) (co_available_to_spy_master co)
            traitsmsg <- case co_traits co of
                Just traits -> concatMapM (\t -> do
                    namemsg <- indentUp $ plainMsg' ("'''" <> t <> "'''")
                    traitmsg <- indentUp $ indentUp $ getUnitTraits t
                    return $ namemsg : traitmsg
                    ) traits
                _ -> return []
            return $ basemsg ++ traitsmsg
createOperativeLeader stmt = preStatement stmt

------------
-- traits --
------------
data HandleTrait = HandleTrait
    { ht_trait :: Text
    , ht_character :: Maybe Text
    , ht_ideology :: Maybe Text
    }

newHT :: HandleTrait
newHT = HandleTrait undefined Nothing Nothing

handleTrait :: forall g m. (HOI4Info g, Monad m) => Bool -> StatementHandler g m
handleTrait addremove stmt@[pdx| %_ = @scr |] =
    pp_ht addremove (foldl' addLine newHT scr)
    where
        addLine ht [pdx| trait = $txt |] = ht { ht_trait = txt }
        addLine ht [pdx| character = $txt |] = ht { ht_character = Just txt }
        addLine ht [pdx| ideology = $txt |] = ht { ht_ideology = Just txt }
        addLine ht [pdx| slot = %_ |] = ht
        addLine ht stmt = trace ("Unknown in handleTrait: " ++ show stmt) ht
        pp_ht addremove ht = do
            traitloc <- Doc.oneLine <$> getGameL10n (ht_trait ht)
            traitmsg <- indentUp $ getLeaderTraits (ht_trait ht)
            baseMsg <- case (ht_character ht, ht_ideology ht) of
                (Just char, Just ideo) -> do
                    charloc <- getCharacterName char
                    ideoloc <- getGameL10n ideo
                    msgToPP $ MsgTraitCharIdeo charloc addremove ideoloc traitloc
                (Just char, _) -> do
                    charloc <- getCharacterName char
                    msgToPP $ MsgTraitChar charloc addremove traitloc
                (_, Just ideo) -> do
                    ideoloc <- getGameL10n ideo
                    msgToPP $ MsgTraitIdeo addremove ideoloc traitloc
                _ -> msgToPP $ MsgTrait addremove traitloc
            return $ baseMsg ++ traitmsg
handleTrait _ stmt = preStatement stmt

-- | Handler for adding or removing a trait: the trait's name, with what it
-- grants written out under it by the given lookup.
addRemoveTrait :: (Monad m, HOI4Info g) =>
    (Text -> PPT g m IndentedMessages) -> (Text -> ScriptMessage) -> StatementHandler g m
addRemoveTrait getTraits msg stmt@[pdx| %_ = $trait |] = do
    traitloc <- Doc.oneLine <$> getGameL10n trait
    traitmsg <- indentUp $ getTraits trait
    baseMsg <- msgToPP (msg traitloc)
    return $ baseMsg ++ traitmsg
-- The block form names the trait under @trait@, and may say which of the
-- country's leaders it belongs to under @ideology@. The trait is the part that
-- carries anything to read.
addRemoveTrait getTraits msg stmt@[pdx| %_ = @scr |] =
    case [traitstmt | traitstmt@[pdx| trait = %_ |] <- scr] of
        (traitstmt : _) -> addRemoveTrait getTraits msg traitstmt
        [] -> preStatement stmt
addRemoveTrait _ _ stmt = preStatement stmt

addRemoveLeaderTrait :: (Monad m, HOI4Info g) => (Text -> ScriptMessage) -> StatementHandler g m
addRemoveLeaderTrait = addRemoveTrait getLeaderTraits

addRemoveUnitTrait :: (Monad m, HOI4Info g) => (Text -> ScriptMessage) -> StatementHandler g m
addRemoveUnitTrait = addRemoveTrait getUnitTraits

data AddTimedTrait = AddTimedTrait
    { adt_trait :: Text
    , adt_days :: Maybe Double
    , adt_daysvar :: Maybe Text
    }

newADT :: AddTimedTrait
newADT = AddTimedTrait undefined Nothing Nothing
addTimedTrait ::  (Monad m, HOI4Info g) => GenericStatement -> PPT g m IndentedMessages
addTimedTrait stmt@[pdx| %_ = @scr |] =
    ppADT (foldl' addLine newADT scr)

    where
        addLine adt [pdx| trait = $txt |] = adt { adt_trait = txt }
        addLine adt [pdx| days = !num |] = adt { adt_days = Just num }
        addLine adt [pdx| days = $txt |] = adt { adt_daysvar = Just txt }
        addLine adt stmt = trace ("Unknown in addTimedTrait: " ++ show stmt) adt
        ppADT adt = do
            traitloc <- getGameL10n (adt_trait adt)
            traitmsg <- indentUp $ getUnitTraits (adt_trait adt)
            baseMsg <- case (adt_days adt, adt_daysvar adt) of
                (Just days,_)-> msgToPP $ MsgAddTimedUnitLeaderTrait traitloc days
                (_, Just daysvar)->msgToPP $ MsgAddTimedUnitLeaderTraitVar traitloc daysvar
                _-> msgToPP $ MsgAddTimedUnitLeaderTraitVar traitloc "<!-- Check Script -->"
            return $ baseMsg ++ traitmsg
addTimedTrait stmt = preStatement stmt


data SwapTrait = SwapTrait
    { st_add :: Text
    , st_remove :: Text
    }

newST :: SwapTrait
newST = SwapTrait undefined undefined
swapLeaderTrait ::  (Monad m, HOI4Info g) => GenericStatement -> PPT g m IndentedMessages
swapLeaderTrait stmt@[pdx| %_ = @scr |] =
    ppST (foldl' addLine newST scr)

    where
        addLine st [pdx| add = $txt |] = st { st_add = txt }
        addLine st [pdx| remove = $txt |] = st { st_remove = txt }
        addLine st [pdx| ideology = %_ |] = st -- restricts the swap to a leader of this ideology
        addLine st stmt = trace ("Unknown in swapTrait: " ++ show stmt) st
        ppST st = do
            traitaddloc <- getGameL10n (st_add st)
            traitremoveloc <- getGameL10n (st_remove st)
            let same = traitaddloc == traitremoveloc
            namemsg <- indentUp $ plainMsg' ("'''" <> traitaddloc <> "'''")
            traitmsg' <- indentUp $ indentUp $ getLeaderTraits (st_add st)
            let traitmsg = namemsg : traitmsg'
            baseMsg <- if same
                then msgToPP MsgModifyCountryLeaderTrait
                else msgToPP $ MsgReplaceCountryLeaderTrait traitremoveloc
            return $ baseMsg ++ traitmsg
swapLeaderTrait stmt = preStatement stmt

getLeaderTraits :: (Monad m, HOI4Info g) => Text -> PPT g m IndentedMessages
getLeaderTraits trait = do
    traits <- getCountryLeaderTraits
    case HM.lookup trait traits of
        Just clt-> do
            mod <- maybe (return []) (\ml -> fmap fold $ traverse (modifierMSG False "") =<< sortmod ml) (clt_modifier clt)
            equipmod <- maybe (return []) handleEquipmentBonus (clt_equipment_bonus clt)
            tarmod <- maybe (return []) (concatMapM handleTargetedModifier) (clt_targeted_modifier clt)
            hidmod <- maybe (return []) handleModifier (clt_hidden_modifier clt)
            return ( mod ++ hidmod ++ tarmod ++ equipmod )
        Nothing -> getUnitTraits trait
    where
        sortmod scr = sortmods scr =<< getModKeys

getUnitTraits :: (Monad m, HOI4Info g) => Text-> PPT g m IndentedMessages
getUnitTraits trait = do
    traits <- getUnitLeaderTraits
    case HM.lookup trait traits of
        Just ult-> do
            attack <- maybe (return []) (msgToPP . MsgAddSkill "Attack") (ult_attack_skill ult)
            defense <- maybe (return []) (msgToPP . MsgAddSkill "Defense") (ult_defense_skill ult)
            planning <- maybe (return []) (msgToPP . MsgAddSkill "Planning") (ult_planning_skill ult)
            logistics <- maybe (return []) (msgToPP . MsgAddSkill "Logistics") (ult_logistics_skill ult)
            maneuvering <- maybe (return []) (msgToPP . MsgAddSkill "Maneuvering") (ult_maneuvering_skill ult)
            coordination <- maybe (return []) (msgToPP . MsgAddSkill "Coordination") (ult_coordination_skill ult)
            let skillmsg = attack ++ defense ++ planning ++ logistics ++ maneuvering ++ coordination
                mod = getscript (ult_modifier ult)
                nsmod = getscript (ult_non_shared_modifier ult)
                ccmod = getscript (ult_corps_commander_modifier ult)
                fmmod = getscript (ult_field_marshal_modifier ult)
            trtxp <- maybe (return []) handleModifier (ult_trait_xp_factor ult)
            mods <- do
                let mods' = mod ++ nsmod ++ ccmod ++ fmmod
                keys <- getModKeys
                sm <- sortmods mods' keys
                fold <$> traverse (modifierMSG False "") sm
            sumod <- maybe (return []) handleEquipmentBonus (ult_sub_unit_modifiers ult)

            return (trtxp ++ mods ++ sumod ++ skillmsg)
        Nothing -> return []
    where
        getscript stmt = case stmt of
            Just [pdx| %_ = @scr|] -> scr
            _ -> []

--------------
-- tooltips --
--------------

-- | Handler for @custom_effect_tooltip@. A tooltip is usually a localization key
-- on its own, but it can also be a block naming the key together with values for
-- the arguments its text is written with.
--
-- Tooltips are worth the trouble of reading: script hides the machinery of an
-- effect in a @hidden_effect@ and puts a tooltip next to it saying in one
-- sentence what the machinery comes to, so the text a tooltip names is the
-- game's own summary of an effect we would otherwise have to spell out.
-- | Handler for @event_option_tooltip@, which stands next to the effect that
-- will offer an event and says what picking one of its options comes to. What
-- the game draws is that option's effects, not the words written on its button,
-- so the effects are what we write out.
--
-- The option is named by its localization key, which by convention is the id of
-- the event it belongs to with the option's letter on the end, so the event to
-- look in is everything up to the last dot.
eventOptionTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
eventOptionTooltip stmt@[pdx| %_ = ?key |] = do
    events <- getEvents
    case HM.lookup (T.dropEnd 1 (T.dropWhileEnd (/= '.') key)) events of
        Just evt | Just opt <- find isNamedKey (fromMaybe [] (hoi4evt_options evt)) ->
            setIsInEffect True (ppMany (fromMaybe [] (hoi4opt_effects opt)))
        _ -> preStatement stmt
    where isNamedKey opt = hoi4opt_name opt == Just key
eventOptionTooltip stmt = preStatement stmt

tooltipWith :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
tooltipWith msg stmt@[pdx| %_ = @scr |] =
    case extractStmt (matchLhsText "localization_key") scr of
        (Just [pdx| %_ = ?key |], rest) -> do
            args <- HM.fromList . catMaybes <$> traverse locArg rest
            tooltipText msg =<< locKeyText args key
        _ -> preStatement stmt
tooltipWith msg [pdx| %_ = ?key |] = tooltipText msg =<< locKeyText HM.empty key
tooltipWith _ stmt = preStatement stmt

-- | Handler for @custom_trigger_tooltip@, which shows its own sentence in
-- place of the triggers inside it. That sentence is all the game ever shows
-- of the block, so it stands alone where the block stood, with the hidden
-- triggers dropped: a lone line can read on from whatever the block was
-- written under, where a heading and a list cannot. A block with no sentence
-- to show falls back to listing its triggers under a heading, which is more
-- than the game says, but the wiki has nothing else to say there.
customTriggerTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
customTriggerTooltip stmt@[pdx| %_ = @scr |] = do
    shown <- concatMapM (tooltipWith MsgUnprocessed) [tt | tt@[pdx| tooltip = %_ |] <- scr]
    if null shown
        then compoundMessage MsgCustomTriggerTooltip stmt
        else return shown
customTriggerTooltip stmt = preStatement stmt

-- | Handler for @custom_override_tooltip@, which runs the effects inside it but
-- shows only its own sentence in place of theirs. What the game hides here is
-- exactly what the wiki is for, so the sentence heads the block and the effects
-- are written out under it.
customOverrideTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
customOverrideTooltip stmt@[pdx| %_ = @scr |] = do
    let (mtooltip, rest) = extractStmt (matchLhsText "tooltip") scr
    ttmsg <- maybe (return []) (tooltipWith MsgTooltip) mtooltip
    script_pp'd <- ppMany rest
    return $ ttmsg ++ script_pp'd
customOverrideTooltip stmt = preStatement stmt

-- | Emit a tooltip's text, or nothing at all if it has none: scripts use
-- tooltips whose text is only whitespace (@generic_skip_one_line_tt@ and
-- friends) to space the tooltip out in game, and on the wiki they are noise. A
-- tooltip written over several lines keeps its breaks, written the way the wiki
-- writes a break inside a list item.
tooltipText :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> Text -> PPT g m IndentedMessages
tooltipText msg loc
    | T.null stripped = return []
    | otherwise = msgToPP (msg (Doc.nl2br stripped))
    where stripped = T.strip loc

-- | The text a tooltip's localization key comes to. A key is usually just a key,
-- but it can instead ask one of the game's formatters for something the game
-- works out as it draws the tooltip, written as the formatter's name, a @|@, and
-- the token to apply it to.
locKeyText :: (HOI4Info g, Monad m) => HashMap Text LocArg -> Text -> PPT g m Text
locKeyText args key = case T.stripPrefix "|" rest of
    Just token -> fromMaybe (typewriterText key) <$> locFormatter fmt token
    Nothing -> fillConstants =<< getGameL10nArgs args key
    where (fmt, rest) = T.breakOn "|" key



-- | What one of the game's localization formatters comes to when applied to a
-- token, or 'Nothing' for one we cannot work out, which is left as written for a
-- human to fill in. The eight the game documents are of two kinds: those that
-- name or describe one thing, which come to a phrase, and those that say what
-- something grants, which come to a line apiece.
--
-- Several are documented as needing the country the tooltip is drawn for, but
-- only @country_culture@ reads it for anything: to prefer a country's own wording
-- over the generic one. A wiki page has no country, so the generic wording is
-- what we use, which is the wording the game itself uses for every country that
-- has nothing of its own to say.
locFormatter :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m (Maybe Text)
locFormatter fmt token = case fmt of
    "character_name" -> Just <$> getCharacterName token
    "idea_name" -> do
        ides <- getIdeas
        traverse (getGameL10n . id_name) (HM.lookup token ides)
    "idea_desc" -> do
        ides <- getIdeas
        return (id_desc_loc =<< HM.lookup token ides)
    -- An advisor is an idea, and the post's own entry names the character rather
    -- than describing them, so the description wanted is the idea's.
    "advisor_desc" -> do
        charto <- getCharToken
        descOf $ case HM.lookup token charto of
            Just adv -> adv_idea_token adv
            Nothing -> token
    "country_leader_desc" -> descOf token
    "country_culture" -> getGameL10nIfPresent token
    "building_state_modifier" -> do
        blds <- getBuildings
        case bld_state_modifiers =<< HM.lookup token blds of
            Just mods -> Just <$> (messageLines =<< handleModifier mods)
            Nothing -> return Nothing
    "tech_effect" -> do
        techs <- concat . HM.elems <$> getTechnologies
        case find ((token ==) . tech_id) techs of
            Just tech -> Just <$> (messageLines =<< techEffect tech)
            Nothing -> return Nothing
    _ -> return Nothing
    where descOf key = getGameL10nIfPresent (key <> "_desc")

-- | Write out messages as the lines of a tooltip's text. A tooltip goes into a
-- single list item, so what would otherwise have been the list's own nesting has
-- to be spelled out. The game asks for a particular indent with an @INDENT@
-- argument, which wiki markup has no use for.
messageLines :: (HOI4Info g, Monad m) => IndentedMessages -> PPT g m Text
messageLines msgs = T.intercalate "\n" <$> traverse line msgs
    where
        base = if null msgs then 0 else minimum (map fst msgs)
        line (i, msg) = (T.replicate (2 * max 0 (i - base)) "&nbsp;" <>) . T.strip
                            <$> messageText msg

-- | What finishing a technology comes to: whatever it makes available, the
-- modifiers it gives the country, and the stats it improves for the units it
-- applies to.
techEffect :: (HOI4Info g, Monad m) => HOI4Technology -> PPT g m IndentedMessages
techEffect tech = do
    units <- getUnit
    unittags <- getUnitTag
    let isStatBlock stmt = case stmt of
            [pdx| $what = @_ |] -> what `elem` units || what `elem` unittags
            _ -> False
        available = concatMap (fromMaybe [])
            [tech_equipment tech, tech_modules tech, tech_units tech]
    availmsg <- concatMapM (\what -> msgToPP . MsgTechEnables =<< getGameL10n what) available
    buildmsg <- concatMapM building (fromMaybe [] (tech_buildings tech))
    globalmsg <- fold <$> traverse (modifierMSG False "") (fromMaybe [] (tech_globalmod tech))
    statmsg <- concatMapM statBlock (filter isStatBlock (fromMaybe [] (tech_sortrest tech)))
    return $ availmsg ++ buildmsg ++ globalmsg ++ statmsg
    where
        -- A technology raises the level a building may be built to rather than
        -- simply making it available, so the level is worth saying.
        building scr = do
            let (mbuilding, rest) = extractStmt (matchLhsText "building") scr
                (mlevel, _) = extractStmt (matchLhsText "level") rest
            case mbuilding of
                Just [pdx| %_ = $what |] -> do
                    whatloc <- getGameL10n what
                    case mlevel of
                        Just [pdx| %_ = !level |] -> msgToPP (MsgTechEnablesBuilding whatloc level)
                        _ -> msgToPP (MsgTechEnables whatloc)
                _ -> return []
        -- The stats a technology improves are written under the unit or the class
        -- of units they apply to, which the game picks out in yellow.
        statBlock [pdx| $what = @scr |] = do
            whatloc <- Doc.oneLine <$> getGameL10n what
            headmsg <- plainMsg' ("'''" <> whatloc <> "'''")
            modmsg <- fold <$> indentUp (traverse (modifierMSG False "") scr)
            return (headmsg : modmsg)
        statBlock stmt = preStatement stmt

-- | The value a tooltip gives for one of the arguments its text is written with,
-- ready to be written into that text.
locArg :: (HOI4Info g, Monad m) => GenericStatement -> PPT g m (Maybe (Text, LocArg))
-- An argument can be a tooltip in its own right, written the same way.
locArg [pdx| $name = @scr |] = case extractStmt (matchLhsText "localization_key") scr of
    (Just [pdx| %_ = ?key |], rest) -> do
        inner <- HM.fromList . catMaybes <$> traverse locArg rest
        Just . (,) name . LocText <$> locKeyText inner key
    _ -> return Nothing
locArg [pdx| $name = !num |] = return (Just (name, LocNum num))
locArg [pdx| $name = ?val |] = Just . (,) name . LocText <$> locArgValue val
locArg _ = return Nothing

-- | Read one tooltip argument's value. Most of them name localization of their
-- own, some ask a formatter for text the game works out for itself, and the rest
-- are the game reading something out of its own state, where the script says how
-- to find it but not what it will say. Those last are left as written for a human
-- to fill in, except for a state's name, which we can look up.
locArgValue :: (HOI4Info g, Monad m) => Text -> PPT g m Text
locArgValue val
    -- A promoted scope, e.g. "[70.GetName]".
    | Just inner <- T.stripPrefix "[" =<< T.stripSuffix "]" unquoted
        = case T.breakOn "." inner of
            (sid, _) | not (T.null sid), T.all isDigit sid
                -> getStateLoc (read (T.unpack sid))
            _ -> return (typewriterText unquoted)
    | otherwise = locKeyText HM.empty unquoted
    where unquoted = T.dropAround (== '"') val

-- | Handler for @show_unit_leaders_tooltip@, which names a commander in a
-- tooltip. Script uses it to show what a @hidden_effect@ next to it just did, so
-- the commander's name is all there is to say.
showUnitLeader :: (HOI4Info g, Monad m) => StatementHandler g m
showUnitLeader [pdx| %_ = ?token |] = do
    nameloc <- getCharacterName token
    role <- getCharacterRole token
    msgToPP (MsgShowUnitLeader (Doc.oneLine nameloc) token role)
showUnitLeader stmt = preStatement stmt

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

-- | Handler for @character_list_tooltip@, which names in a tooltip every
-- character the conditions inside it pick out. Nothing but those conditions is
-- ever written in the block, so they go on the line rather than under it.
characterListTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
characterListTooltip stmt@[pdx| %_ = @scr |] = do
    let (mlimit, rest) = extractStmt (matchLhsText "limit") scr
        (mamount, _) = extractStmt (matchLhsText "random_select_amount") rest
        amount = case mamount of
            Just [pdx| %_ = !num |] -> T.pack (show (round (num :: Double) :: Int))
            _ -> ""
    mclause <- maybe (return Nothing) limitClause mlimit
    case mclause of
        Just clause -> msgToPP (MsgCharacterListTooltip amount clause)
        Nothing -> do
            basemsg <- msgToPP (MsgCharacterListTooltip amount "")
            script_pp'd <- indentUp (ppMany scr)
            return $ basemsg ++ script_pp'd
characterListTooltip stmt = preStatement stmt

------------------------------------------
-- Handlers for the intelligence agency --
------------------------------------------

-- | Handler for @create_intelligence_agency@, whose block gives the agency a
-- name and picks a logo for it. The logo is a graphics id and says nothing to a
-- reader, so only the name is written out. A name is sometimes spelled out in
-- the block and sometimes given as a key to look up.
createIntelligenceAgency :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createIntelligenceAgency [pdx| %_ = @scr |] = case foldl' addLine Nothing scr of
    Nothing -> msgToPP MsgCreateIntelligenceAgencyPlain
    Just name -> do
        nameloc <- getGameL10nIfPresent name
        msgToPP (MsgCreateIntelligenceAgency (fromMaybe name nameloc))
    where
        addLine :: Maybe Text -> GenericStatement -> Maybe Text
        addLine _ [pdx| name = ?name |] = Just name
        addLine acc _ = acc
-- Written bare wherever the agency takes the country's own name.
createIntelligenceAgency [pdx| %_ = yes |] = msgToPP MsgCreateIntelligenceAgencyPlain
createIntelligenceAgency stmt = preStatement stmt

-- | Handler for @upgrade_intelligence_agency@, which puts one upgrade of the
-- agency into effect.
upgradeIntelligenceAgency :: (HOI4Info g, Monad m) => StatementHandler g m
upgradeIntelligenceAgency [pdx| %_ = $upgrade |] =
    msgToPP . MsgUpgradeIntelligenceAgency =<< agencyUpgradeLink upgrade
upgradeIntelligenceAgency stmt = preStatement stmt

-- | Handler for @has_done_agency_upgrade@, which asks whether an upgrade is
-- already in effect.
hasDoneAgencyUpgrade :: (HOI4Info g, Monad m) => StatementHandler g m
hasDoneAgencyUpgrade [pdx| %_ = $upgrade |] =
    msgToPP . MsgHasDoneAgencyUpgrade =<< agencyUpgradeLink upgrade
hasDoneAgencyUpgrade stmt = preStatement stmt

-- | A link to an agency upgrade, on the wiki page the agency is written about
-- on, where the branch of upgrades it belongs to is a heading. Script names an
-- upgrade without saying which branch holds it, and the branches live in a file
-- that says little else we want, so which branch each upgrade sits in is listed
-- here instead.
agencyUpgradeLink :: (HOI4Info g, Monad m) => Text -> PPT g m Text
agencyUpgradeLink theid = do
    mname <- getGameL10nIfPresent theid
    case (mname, HM.lookup (T.toLower theid) agencyUpgradeBranches) of
        (Just name, Just branch) -> do
            branchloc <- getGameL10n branch
            return $ sectionLink "Intelligence agency" (branchloc <> " Branch") name
        -- An upgrade we know the name of but not the branch has no heading to
        -- jump to; one we do not even have a name for is left as the id script
        -- called it by, which is also what says the key needs fixing.
        (Just name, Nothing) -> return name
        _ -> return (typewriterText theid)



-------------------------------------
-- Handler for national focus costs --
-------------------------------------

-- | Handler for @reduce_focus_completion_cost@, which takes days off however many
-- focuses are named in it. The focuses are written as links to where each stands
-- in its tree, so that a reader can go and see which ones were sped up.
reduceFocusCompletionCost :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
reduceFocusCompletionCost stmt@[pdx| %_ = @scr |] = case cost of
    Nothing -> preStatement stmt
    Just days -> do
        basemsg <- msgToPP (MsgReduceFocusCompletionCost days)
        links <- indentUp (concatMapM focusLink focuses)
        return (basemsg ++ links)
    where
        (cost, focuses) = foldl' addLine (Nothing, []) scr
        addLine (c, fs) [pdx| cost = !n |] = (Just n, fs)
        addLine (c, fs) [pdx| focus = @arr |] = (c, fs ++ mapMaybe getbaretraits arr)
        addLine (c, fs) [pdx| focus = $theid |] = (c, fs ++ [theid])
        addLine acc stmt = trace ("unknown section in reduce_focus_completion_cost: " ++ show stmt) acc
reduceFocusCompletionCost stmt = preStatement stmt

-- | One focus written the way the wiki writes it: a template call naming the page
-- it is written up on and the focus itself. A focus on no page of the wiki falls
-- back to its icon and name written out, which is how focuses are named
-- everywhere else.
focusLink :: (HOI4Info g, Monad m) => Text -> PPT g m IndentedMessages
focusLink theid = do
    focuses <- getNationalFocus
    case HM.lookup theid focuses of
        Just nf | Just page <- focusPage focuses nf -> msgToPP (MsgFocusLink page theid)
        Just nf -> msgToPP (MsgFocusNamed (nf_icon nf) theid (nf_name_loc nf))
        Nothing -> msgToPP (MsgUnprocessed (typewriterText theid))

-- | The wiki page a focus is written up on. Which file script keeps a focus in
-- says which page it belongs to, bar three files the wiki writes up over two
-- pages each: Spain's two sides are told apart by the tag their ids carry, and
-- Germany's and the Soviet Union's halves each run in one stretch, so the focus
-- the second half opens with says where the break falls.
focusPage :: HashMap Text HOI4NationalFocus -> HOI4NationalFocus -> Maybe Text
focusPage focuses nf = byTag <|> bySplit <|> HM.lookup file focusPages
    where
        file = T.toLower (T.takeWhileEnd (\c -> c /= '/' && c /= (chr 92)) (T.pack (nf_path nf)))
        byTag = listToMaybe
            [ page | (tag, page) <- focusTagPages, tag `T.isPrefixOf` nf_id nf ]
        bySplit = do
            (before, marker, from) <- HM.lookup file focusPageSplits
            split <- HM.lookup marker focuses
            return (if nf_ordinal nf >= nf_ordinal split then from else before)








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

-- | Handler for @has_doctrine@, which asks whether a grand doctrine or one of
-- the subdoctrines under it is the one the country is running. The statement
-- does not say which kind of part it names, so the grand doctrines are tried
-- first and anything else is taken for a subdoctrine.
hasDoctrine :: (HOI4Info g, Monad m) => StatementHandler g m
hasDoctrine [pdx| %_ = $theid |] = do
    mgrand <- doctrineLocLookup "grand_doctrine" theid
    link <- doctrineLink (maybe "subdoctrine" (const "grand_doctrine") mgrand) theid
    msgToPP (MsgHasDoctrine link)
hasDoctrine stmt = preStatement stmt

--------------------------------------------
-- Handler for remove_country_leader_role --
--------------------------------------------
-- | Handler for @remove_country_leader_role@, which stops a character leading
-- the country for one ideology. In a character scope the character is the one
-- in scope and is not named again.
removeCountryLeaderRole :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
removeCountryLeaderRole stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (mchar, Just ideo) -> do
            ideoloc <- partyIcon ideo
            charloc <- traverse getCharacterName mchar
            msgToPP (MsgRemoveCountryLeaderRole (fromMaybe "" charloc) ideoloc)
        _ -> preStatement stmt
    where
        addLine (c, i) [pdx| character = $ch |] = (Just ch, i)
        addLine (c, i) [pdx| character = ?ch |] = (Just ch, i)
        addLine (c, i) [pdx| ideology = $ideo |] = (c, Just ideo)
        addLine acc stmt = trace ("unknown section in remove_country_leader_role: " ++ show stmt) acc
removeCountryLeaderRole stmt = preStatement stmt

-- | Handler for @can_be_country_leader@, which asks whether a character is fit
-- to lead the country. The character is the one in scope, or named outright.
canBeCountryLeader :: (HOI4Info g, Monad m) => StatementHandler g m
canBeCountryLeader stmt@[pdx| %_ = yes |] = withBool MsgCanBeCountryLeader stmt
canBeCountryLeader stmt@[pdx| %_ = no |] = withBool MsgCanBeCountryLeader stmt
canBeCountryLeader stmt = withCharacter MsgCanBeCountryLeaderChar stmt

