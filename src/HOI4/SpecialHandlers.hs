
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
    ,   modifiersTable
    ,   addDynamicModifier
    ,   removeDynamicModifier
    ,   hasDynamicModifier
    ,   ScriptChunk (..)
    ,   chunkScript
    ,   ppDynModChunk
    ,   ppIdeaSlotChunk
    ,   addPowerBalanceModifier
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
    ,   customEffectTooltip
    ,   customOverrideTooltip
    ,   tooltip
    ,   tooltipWith
    ,   showUnitLeader
    ,   showMio
    ,   unlockMio
    ,   unlockMioTrait
    ,   unlockMioPolicy
    ,   characterListTooltip
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
--import Data.Set (Set)


import Data.Char (isDigit)
import Data.List (foldl', sortOn, elemIndex, find)
import Data.Maybe

import Control.Monad.State (gets)
import Data.Foldable (fold)

import Abstract -- everything
import qualified Doc -- everything
import HOI4.Messages -- everything
import MessageTools (iquotes
                    , plainNum
                    , formatDays)
import QQ -- everything
-- everything
import SettingsTypes ( PPT, IsGameData (..), GameData (..), IsGameState (..), GameState (..)
                     , indentUp, getCurrentIndent, withCurrentIndent, withCurrentIndentCustom
                     , getGameL10n, getGameL10nIfPresent, getGameL10nArgs
                     , LocArg (..)
                     , concatMapM
                     , getGameInterface, getGameInterfaceIfPresent)
import {-# SOURCE #-} HOI4.Common (ppMany, ppOne, extractStmt, matchLhsText)
import HOI4.Types -- everything
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
                add_loc <- handleIdea True what
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

handleTimedIdeas :: forall g m. (HOI4Info g, Monad m) =>
        (Text -> Text -> Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> StatementHandler g m
handleTimedIdeas msg stmt@[pdx| %_ = @scr |]
    = pp_idda (parseTV "idea" "days" scr)
    where
        pp_idda :: TextValue -> PPT g m IndentedMessages
        pp_idda tv = case (tv_what tv, tv_value tv) of
            (Just what, Just value) -> do
                ideashandled <- handleIdea True what
                case ideashandled of
                    Just (category, ideaIcon, ideaKey, idea_loc, Just effectbox) -> do
                        idmsg <- msgToPP $ msg category ideaIcon ideaKey idea_loc value
                        return $ idmsg ++ effectbox
                    Just (category, ideaIcon, ideaKey, idea_loc, Nothing) -> msgToPP $ msg category ideaIcon ideaKey idea_loc value
                    Nothing -> preStatement stmt
            _ -> preStatement stmt
handleTimedIdeas _ stmt = preStatement stmt

handleIdeas :: forall g m. (HOI4Info g, Monad m) =>
    Bool ->
    (Text -> Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
handleIdeas addIdea msg stmt@[pdx| $lhs = %idea |] = case idea of
    CompoundRhs ideas -> if length ideas == 1 then do
                ideashandled <- handleIdea addIdea (mconcat $ map getbareidea ideas)
                case ideashandled of
                    Just (category, ideaIcon, ideaKey, idea_loc, Just effectbox) -> do
                        idmsg <- msgToPP $ msg category ideaIcon ideaKey idea_loc
                        return $ idmsg ++ effectbox
                    Just (category, ideaIcon, ideaKey, idea_loc, Nothing) -> msgToPP $ msg category ideaIcon ideaKey idea_loc
                    Nothing -> preStatement stmt
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
    GenericRhs txt [] -> do
        ideashandled <- handleIdea addIdea txt
        case ideashandled of
            Just (category, ideaIcon, ideaKey, idea_loc, Just effectbox) -> do
                idmsg <- msgToPP $ msg category ideaIcon ideaKey idea_loc
                return $ idmsg ++ effectbox
            Just (category, ideaIcon, ideaKey, idea_loc, Nothing) -> msgToPP $ msg category ideaIcon ideaKey idea_loc
            Nothing -> preStatement stmt
    _ -> preStatement stmt
handleIdeas _ _ stmt = preStatement stmt

getbareidea :: GenericStatement -> Text
getbareidea (StatementBare (GenericLhs e [])) = e
getbareidea _ = "<!-- Check Script -->"

handleIdea :: (HOI4Info g, Monad m) =>
        Bool -> Text ->
           PPT g m (Maybe (Text, Text, Text, Text, Maybe IndentedMessages))
handleIdea addIdea ide = do
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
            effectboxNS <- if id_category iidea == "country" && addIdea then return $ Just effectbox else return Nothing
            return $ Just (category, ideaIcon, ideaKey, idea_loc, effectboxNS)
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
        Nothing -> getGameInterface "idea_unknown" (id_picture iidea)
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

handleModifier :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleModifier [pdx| %_ = @scr |] = do
    keys <- getModKeys
    sm <- sortmods scr keys
    fold <$> traverse (modifierMSG False "") sm
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

modifierMSG :: forall g m. (HOI4Info g, Monad m) =>
        Bool -> Text -> StatementHandler g m
modifierMSG _ targ stmt@[pdx| $specmod = @scr|]
    | specmod == "hidden_modifier" = do
        keys <- getModKeys
        sm <- sortmods scr keys
        fold <$> traverse (modifierMSG True targ) sm
    | otherwise = do
        terrain <- getTerrain
        if specmod `elem` terrain || specmod `elem` ["fort", "river", "night"]
        then do
            ter <- getGameL10n specmod
            termsg <- plainMsg' ("{{color|Yellow|" <> ter <> "}}:")
            modmsg <- fold <$> indentUp (traverse (modifierMSG False targ) scr)
            return $ termsg : modmsg
        else trace ("unknown modifier type: " ++ show specmod ++ " IN: " ++ show stmt) $ preStatement stmt
modifierMSG hidden targ stmt@[pdx| $mod = !num |] = let lmod = T.toLower mod in case HM.lookup lmod modifiersTable of
    Just (key, msg, dec) -> do
        loc <- getGameL10n key
        let bonus = num :: Double
            loc' = locprep hidden targ loc
        numericLocPrec loc' dec msg stmt
    Nothing
        | "cat_" `T.isPrefixOf` lmod -> do
            mloc <- getGameL10nIfPresent lmod
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierPcNegReduced stmt
                Nothing -> preStatement stmt
        | ("production_speed_" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("state_production_speed_" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("experience_gain_" `T.isPrefixOf` lmod && "_combat_factor" `T.isSuffixOf` lmod) ||
            ("trait_" `T.isPrefixOf` lmod && "_xp_gain_factor" `T.isSuffixOf` lmod) ||
            ("repair_speed" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("state_repair_speed" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("sp_tag_" `T.isPrefixOf` lmod && "_speed_factor" `T.isSuffixOf` lmod) -> do --precision 2
            mloc <- getGameL10nIfPresent ("modifier_" <> lmod)
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierPcPosReduced stmt
                Nothing -> preStatement stmt
        | "unit_" `T.isPrefixOf` lmod && "_design_cost_factor" `T.isSuffixOf` lmod -> do
            mloc <- getGameL10nIfPresent ("modifier_" <> lmod)
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierPcNegReduced stmt
                Nothing -> preStatement stmt
        | "modifier_army_sub_" `T.isPrefixOf` lmod ||
            (("operation_" `T.isPrefixOf` lmod && "_outcome" `T.isSuffixOf` lmod ) ||
            "sp_tag_" `T.isPrefixOf` lmod ||
            "_preferred_weight_factor"` T.isSuffixOf` lmod ||
            ("specialization_" `T.isPrefixOf` lmod && "_speed_factor" `T.isSuffixOf` lmod)) -> do
            mloc <- getGameL10nIfPresent lmod
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierPcPosReduced stmt
                Nothing -> preStatement stmt
        | "operation_" `T.isPrefixOf` lmod && ("_risk" `T.isSuffixOf` lmod || "_cost" `T.isSuffixOf` lmod ) ||
            "_design_cost_factor" `T.isSuffixOf` lmod -> do
            mloc <- getGameL10nIfPresent lmod
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierPcNegReduced stmt
                Nothing -> preStatement stmt
        | ("state_resource_" `T.isPrefixOf` lmod && not ("state_resource_cost_" `T.isPrefixOf` lmod)) || --precision 0
            ("country_resource_" `T.isPrefixOf` lmod && not ("country_resource_cost_" `T.isPrefixOf` lmod)) || --precision 0
            "temporary_state_resource_" `T.isPrefixOf` lmod -> do --precision 0
            mloc <- getGameL10nIfPresent lmod
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierColourPos stmt
                Nothing -> preStatement stmt
        |  "_intel_decryption_bonus" `T.isSuffixOf` lmod -> do --precision 0
            mloc <- getGameL10nIfPresent ("modifier_" <> lmod)
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierColourPos stmt
                Nothing -> preStatement stmt
        | "country_resource_cost_" `T.isPrefixOf` lmod -> do --precision 0
            mloc <- getGameL10nIfPresent lmod
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierColourNeg stmt
                Nothing -> preStatement stmt
        | "production_cost_max_" `T.isPrefixOf` lmod -> do --precision 0
            mloc <- getGameL10nIfPresent ("modifier_" <> lmod)
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    numericLocPrec loc' (familyPrecision lmod) MsgModifierYellow stmt
                Nothing -> preStatement stmt
        | otherwise -> do
            moddef <- getModifierDefinitions
            case HM.lookup mod moddef of
                Just (_, scrmsg, dec) -> do
                    mloc <- getGameL10nIfPresent mod
                    case mloc of
                        Just loc ->
                            let loc' = locprep hidden targ loc in
                            numericLocPrec loc' dec scrmsg stmt
                        Nothing -> preStatement stmt
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
        | "cat_" `T.isPrefixOf` lmod -> do
            mloc <- getGameL10nIfPresent lmod
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    msgToPP $ MsgModifierVar loc' var
                Nothing -> preStatement stmt
        | ("production_speed_" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("state_production_speed_" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("unit_" `T.isPrefixOf` lmod && "_design_cost_factor" `T.isSuffixOf` lmod) ||
            ("experience_gain_" `T.isPrefixOf` lmod && "_combat_factor" `T.isSuffixOf` lmod) ||
            ("trait_" `T.isPrefixOf` lmod && "_xp_gain_factor" `T.isSuffixOf` lmod) ||
            ("repair_speed" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) ||
            ("state_repair_speed" `T.isPrefixOf` lmod && "_factor" `T.isSuffixOf` lmod) -> do
            mloc <- getGameL10nIfPresent ("modifier_" <> lmod)
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    msgToPP $ MsgModifierVar loc' var
                Nothing -> preStatement stmt
        | "modifier_army_sub_" `T.isPrefixOf` lmod ||
            ("operation_" `T.isPrefixOf` lmod && "_outcome" `T.isSuffixOf` lmod) ||
            ("country_resource_" `T.isPrefixOf` lmod && not ("country_resource_cost_" `T.isPrefixOf` lmod)) ||
            "_design_cost_factor" `T.isSuffixOf` lmod ||
            "state_resource_" `T.isPrefixOf` lmod ||
            "country_resource_cost_" `T.isPrefixOf` lmod ||
            "temporary_state_resource_" `T.isPrefixOf` lmod -> do
            mloc <- getGameL10nIfPresent lmod
            case mloc of
                Just loc ->
                    let loc' = locprep hidden targ loc in
                    msgToPP $ MsgModifierVar loc' var
                Nothing -> preStatement stmt
        | otherwise -> do
            moddef <- getModifierDefinitions
            case HM.lookup mod moddef of
                Just _ -> do
                    mloc <- getGameL10nIfPresent mod
                    case mloc of
                        Just loc ->
                            let loc' = locprep hidden targ loc in
                            msgToPP $ MsgModifierVar loc' var
                        Nothing -> preStatement stmt
                Nothing -> preStatement stmt
modifierMSG _ _ stmt = preStatement stmt

numericLocPost :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    Text
        -> (Text -> Double -> Maybe Text -> ScriptMessage)
        -> StatementHandler g m
numericLocPost what msg [pdx| %_ = !amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msg whatloc amt Nothing
numericLocPost _ _  stmt = plainMsg $ preStatementText' stmt

-- | The decimal places the documentation gives for a modifier family: the
-- modifiers named after a building, a unit, a resource or the like, which are
-- documented by their shape rather than one at a time, and which 'modifierMSG'
-- likewise recognises by their shape rather than from 'modifiersTable'.
familyPrecision :: Text -> Maybe Int
familyPrecision lmod = Just $
    if any (`T.isPrefixOf` lmod)
            [ "experience_gain_"          -- experience_gain_<Unit>_combat_factor
            , "sp_tag_", "specialization_" -- <SpecialProject>_speed_factor
            , "operation_"               -- <Operation>_cost, _outcome and _risk
            , "state_resource_", "country_resource_", "temporary_state_resource_"
            , "production_cost_max_"     -- production_cost_max_<NavalEquipment>
            , "cat_"                     -- <IdeaCategory>_category_type_cost_factor
            , "modifier_army_sub_" ]
        || "_intel_decryption_bonus" `T.isSuffixOf` lmod
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
        -- The country a targeted modifier applies to reads better after what the
        -- modifier does than in front of it: a colon is written straight after
        -- this, and a flag at the front puts the whole width of a name between
        -- the reader and it.
        named | T.null targ = loc'
              | otherwise = T.strip loc' <> " (" <> targ <> ")"

handleResearchBonus :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleResearchBonus [pdx| %_ = @scr |] = fold <$> traverse handleResearchBonus' scr
    where
        handleResearchBonus' stmt@[pdx| $tech = !num |] = let bonus = num :: Double in numericLocPrec (T.toLower tech <> "_research") Nothing MsgModifierPcPosReduced stmt
        handleResearchBonus' scr = preStatement scr
handleResearchBonus stmt = preStatement stmt

handleHiddenModifier :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleHiddenModifier [pdx| %_ = @scr |] = do
    keys <- getModKeys
    sm <- sortmods scr keys
    fold <$> traverse (modifierMSG True "") sm
handleHiddenModifier stmt = preStatement stmt

handleTargetedModifier :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleTargetedModifier stmt@[pdx| %_ = @scr |] = do
    let (tag, rest) = extractStmt (matchLhsText "tag") scr
    tagmsg <- case tag of
        Just [pdx| tag = $country |] -> flagText (Just HOI4Country) country
        _ -> return "CHECK SCRIPT"
    keys <- getModKeys
    sm <- sortmods rest keys
    fold <$> traverse (modifierTagMSG tagmsg) sm
        where
            modifierTagMSG = modifierMSG False
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
                modmsg <- do
                    keys <- getModKeys
                    sm <- sortmods rest keys
                    fold <$> indentUp (traverse modifierEquipMSG' sm)
                return $ techmsg : modmsg
            modifierEquipMSG stmt = preStatement stmt

            modifierEquipMSG' = modifierMSG False ""
handleEquipmentBonus stmt = preStatement stmt


-- | Handlers for numeric statements with icons
modifiersTable :: HashMap Text ModifierDisplay
modifiersTable = HM.fromList
        [
        --general modifiers
         ("monthly_population"              , ("MODIFIER_GLOBAL_MONTHLY_POPULATION", MsgModifierPcPosReduced, Just 1))
        ,("nuclear_production_factor"       , ("MODIFIER_NUCLEAR_PRODUCTION_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("research_sharing_per_country_bonus" , ("MODIFIER_RESEARCH_SHARING_PER_COUNTRY_BONUS", MsgModifierPcPosReduced, Just 2))
        ,("research_sharing_per_country_bonus_factor" , ("MODIFIER_RESEARCH_SHARING_PER_COUNTRY_BONUS_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("research_speed_factor"           , ("MODIFIER_RESEARCH_SPEED_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("local_resources_factor"          , ("MODIFIER_LOCAL_RESOURCES_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("surrender_limit"                 , ("MODIFIER_SURRENDER_LIMIT", MsgModifierPcPosReduced, Just 2))
        ,("max_surrender_limit_offset"      , ("MODIFIER_MAX_SURRENDER_LIMIT_OFFSET", MsgModifierPcPosReduced, Just 2)) --precision 2

            -- Politics modifiers
        ,("min_export"                      , ("MODIFIER_MIN_EXPORT_FACTOR", MsgModifierPcReducedSign, Just 0)) -- yellow
        ,("trade_opinion_factor"            , ("MODIFIER_TRADE_OPINION_FACTOR", MsgModifierPcReducedSign, Just 2))
        ,("economy_cost_factor"             , ("economy_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("disabled_ideas"                  , ("MODIFIER_DISABLE_IDEA_TAKING", modNoYes, Just 0))
        ,("mobilization_laws_cost_factor"   , ("mobilization_laws_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("political_advisor_cost_factor"   , ("political_advisor_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("trade_laws_cost_factor"          , ("trade_laws_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("tank_manufacturer_cost_factor"   , ("tank_manufacturer_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("naval_manufacturer_cost_factor"  , ("naval_manufacturer_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("aircraft_manufacturer_cost_factor" , ("aircraft_manufacturer_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("materiel_manufacturer_cost_factor" , ("materiel_manufacturer_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("industrial_concern_cost_factor"  , ("industrial_concern_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("theorist_cost_factor"            , ("theorist_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("army_chief_cost_factor"          , ("army_chief_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("navy_chief_cost_factor"          , ("navy_chief_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("air_chief_cost_factor"           , ("air_chief_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("high_command_cost_factor"        , ("high_command_cost_factor", MsgModifierPcNegReduced, Nothing))
        ,("air_advisor_cost_factor"         , ("MODIFIER_AIR_ADVISOR_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("army_advisor_cost_factor"        , ("MODIFIER_ARMY_ADVISOR_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("navy_advisor_cost_factor"        , ("MODIFIER_NAVY_ADVISOR_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("offensive_war_stability_factor"  , ("MODIFIER_STABILITY_OFFENSIVE_WAR_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("defensive_war_stability_factor"  , ("MODIFIER_STABILITY_DEFENSIVE_WAR_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("unit_leader_as_advisor_cp_cost_factor" , ("MODIFIER_UNIT_LEADER_AS_ADVISOR_CP_COST_FACTOR", MsgModifierPcNegReduced, Just 1)) --precision 1
        ,("improve_relations_maintain_cost_factor" , ("MODIFIER_IMPROVE_RELATIONS_MAINTAIN_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("party_popularity_stability_factor" , ("MODIFIER_STABILITY_POPULARITY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("political_power_cost"            , ("MODIFIER_POLITICAL_POWER_COST", MsgModifierColourNeg, Just 2))
        ,("political_power_gain"            , ("MODIFIER_POLITICAL_POWER_GAIN", MsgModifierColourPos, Just 2)) --precision 2
        ,("political_power_factor"          , ("MODIFIER_POLITICAL_POWER_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("stability_factor"                , ("MODIFIER_STABILITY_FACTOR", MsgModifierPcPosReduced, Just 2)) --precision 2
        ,("stability_weekly"                , ("MODIFIER_STABILITY_WEEKLY", MsgModifierPcPosReduced, Just 2))
        ,("stability_weekly_factor"         , ("MODIFIER_STABILITY_WEEKLY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("war_stability_factor"            , ("MODIFIER_STABILITY_WAR_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("war_support_factor"              , ("MODIFIER_WAR_SUPPORT_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("war_support_weekly"              , ("MODIFIER_WAR_SUPPORT_WEEKLY", MsgModifierPcPosReduced, Just 2))
        ,("war_support_weekly_factor"       , ("MODIFIER_WAR_SUPPORT_WEEKLY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("weekly_casualties_war_support"   , ("MODIFIER_WEEKLY_CASUALTIES_WAR_SUPPORT", MsgModifierPcPosReduced, Just 2))
        ,("weekly_convoys_war_support"      , ("MODIFIER_WEEKLY_CONVOYS_WAR_SUPPORT", MsgModifierPcPosReduced, Just 2))
        ,("weekly_bombing_war_support"      , ("MODIFIER_WEEKLY_BOMBING_WAR_SUPPORT", MsgModifierPcPosReduced, Just 2))
        ,("drift_defence_factor"            , ("MODIFIER_DRIFT_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("power_balance_daily"             , ("MODIFIER_POWER_BALANCE_DAILY", MsgModifierBop, Just 2))
        ,("power_balance_weekly"            , ("MODIFIER_POWER_BALANCE_WEEKLY", MsgModifierBop, Just 2))
        ,("communism_drift"                 , ("communism_drift", MsgModifierColourPos, Nothing)) --precision 2
        ,("democratic_drift"                , ("democratic_drift", MsgModifierColourPos, Nothing)) --precision 2
        ,("fascism_drift"                   , ("fascism_drift", MsgModifierColourPos, Nothing)) --precision 2
        ,("neutrality_drift"                , ("neutrality_drift", MsgModifierColourPos, Nothing)) --precision 2
        ,("communism_acceptance"            , ("communism_acceptance", MsgModifierColourPos, Nothing))
        ,("democratic_acceptance"           , ("democratic_acceptance", MsgModifierColourPos, Nothing))
        ,("fascism_acceptance"              , ("fascism_acceptance", MsgModifierColourPos, Nothing))
        ,("neutrality_acceptance"           , ("neutrality_acceptance", MsgModifierColourPos, Nothing))

            -- Diplomacy
        ,("civil_war_involvement_tension"   , ("MODIFIER_CIVIL_WAR_INVOLVEMENT_TENSION", MsgModifierPcNegReduced, Just 1)) -- precision 1
        ,("enemy_declare_war_tension"       , ("MODIFIER_ENEMY_DECLARE_WAR_TENSION", MsgModifierPcPosReduced, Just 1))
        ,("enemy_justify_war_goal_time"     , ("MODIFIER_ENEMY_JUSTIFY_WAR_GOAL_TIME", MsgModifierPcPosReduced, Just 1))
        ,("faction_influence_war_score_factor" , ("MODIFIER_FACTION_INFLUENCE_WAR_SCORE", MsgModifierPcReducedSignMin, Just 2)) -- yellow
        ,("faction_trade_opinion_factor"    , ("MODIFIER_FACTION_TRADE_OPINION_FACTOR", MsgModifierPcReducedSign, Just 2)) --precision 2 yellow
        ,("generate_wargoal_tension"        , ("MODIFIER_GENERATE_WARGOAL_TENSION_LIMIT", MsgModifierPcReducedSign, Just 1)) -- yellow
        ,("guarantee_cost"                  , ("MODIFIER_GUARANTEE_COST", MsgModifierPcNegReduced, Just 0))
        ,("guarantee_tension"               , ("MODIFIER_GUARANTEE_TENSION_LIMIT", MsgModifierPcNegReduced, Just 1))
        ,("join_faction_tension"            , ("MODIFIER_JOIN_FACTION_TENSION_LIMIT", MsgModifierPcNegReduced, Just 1))
        ,("justify_war_goal_time"           , ("MODIFIER_JUSTIFY_WAR_GOAL_TIME", MsgModifierPcNegReduced, Just 1))
        ,("justify_war_goal_when_in_major_war_time" , ("MODIFIER_JUSTIFY_WAR_GOAL_WHEN_IN_MAJOR_WAR_TIME", MsgModifierPcNegReduced, Just 1))
        ,("lend_lease_tension"              , ("MODIFIER_LEND_LEASE_TENSION_LIMIT", MsgModifierPcNegReduced, Just 1))
        ,("opinion_gain_monthly"            , ("MODIFIER_OPINION_GAIN_MONTHLY", MsgModifierColourPos, Just 1))
        ,("opinion_gain_monthly_factor"     , ("MODIFIER_OPINION_GAIN_MONTHLY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("opinion_gain_monthly_same_ideology" , ("MODIFIER_OPINION_GAIN_MONTHLY_SAME_IDEOLOGY", MsgModifierColourPos, Just 1))
        ,("opinion_gain_monthly_same_ideology_factor" , ("MODIFIER_OPINION_GAIN_MONTHLY_SAME_IDEOLOGY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("request_lease_tension"           , ("MODIFIER_REQUEST_LEASE_TENSION_LIMIT", MsgModifierPcNegReduced, Just 1))
        ,("annex_cost_factor"               , ("MODIFIER_ANNEX_COST_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("puppet_cost_factor"              , ("MODIFIER_PUPPET_COST_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("send_volunteer_divisions_required" , ("MODIFIER_SEND_VOLUNTEER_DIVISIONS_REQUIRED", MsgModifierPcNegReduced, Just 1))
        ,("send_volunteer_factor"           , ("MODIFIER_SEND_VOLUNTEER_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("send_volunteer_size"             , ("MODIFIER_SEND_VOLUNTEER_SIZE", MsgModifierColourPos, Just 0))
        ,("send_volunteers_tension"         , ("MODIFIER_SEND_VOLUNTEERS_TENSION_LIMIT", MsgModifierPcNegReduced, Just 1))
        ,("air_volunteer_cap"               , ("MODIFIER_AIR_VOLUNTEER_CAP", MsgModifierColourPos, Just 0))
        ,("embargo_threshold_factor"        , ("MODIFIER_EMBARGO_THRESHOLD_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("embargo_cost_factor"             , ("MODIFIER_EMBARGO_COST_FACTOR", MsgModifierPcNegReduced, Just 1))

        ,("resource_trade_cost_bonus_per_factory", ("MODIFIER_RESOURCE_TRADE_COST_BONUS_PER_FACTORY", MsgModifierColourPos, Just 0))

            -- autonomy
        ,("autonomy_gain"                   , ("MODIFIER_AUTONOMY_GAIN", MsgModifierColourPos, Just 1))
        ,("autonomy_gain_global_factor"     , ("MODIFIER_AUTONOMY_GAIN_GLOBAL_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("subjects_autonomy_gain"          , ("MODIFIER_AUTONOMY_SUBJECT_GAIN", MsgModifierColourPos, Just 2))
        ,("autonomy_gain_ll_to_overlord"    , ("MODIFIER_AUTONOMY_GAIN_LL_TO_OVERLORD", MsgModifierColourPos, Just 2))
        ,("autonomy_gain_trade_factor"      , ("MODIFIER_AUTONOMY_GAIN_TRADE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("autonomy_manpower_share"         , ("MODIFIER_AUTONOMY_MANPOWER_SHARE", MsgModifierPcReducedSign, Just 2))
        ,("can_master_build_for_us"         , ("MODIFIER_CAN_MASTER_BUILD_FOR_US", modNoYes, Just 0))
        ,("cic_to_overlord_factor"          , ("MODIFIER_CIC_TO_OVERLORD_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("mic_to_overlord_factor"          , ("MODIFIER_MIC_TO_OVERLORD_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("extra_trade_to_overlord_factor"  , ("MODIFIER_TRADE_TO_OVERLORD_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("master_ideology_drift"           , ("MODIFIER_MASTER_IDEOLOGY_DRIFT", MsgModifierColourPos, Just 2))
        ,("overlord_trade_cost_factor"      , ("MODIFIER_TRADE_COST_FACTOR", MsgModifierPcNegReduced, Just 2))

            -- Governments in exile
        ,("dockyard_donations"              , ("MODIFIER_DOCKYARD_DONATIONS", MsgModifierColourPos, Just 0))
        ,("industrial_factory_donations"    , ("MODIFIER_INDUSTRIAL_FACTORY_DONATIONS", MsgModifierColourPos, Just 0))
        ,("military_factory_donations"      , ("MODIFIER_MILITARY_FACTORY_DONATIONS", MsgModifierColourPos, Just 0))
        ,("exile_manpower_factor"           , ("MODIFIER_EXILED_MAPOWER_GAIN_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("exiled_government_weekly_manpower" , ("MODIFIER_EXILED_GOVERNMENT_WEEKLY_MANPOWER", MsgModifierColourPos, Just 0))
        ,("legitimacy_daily"                , ("MODIFIER_LEGITIMACY_DAILY", MsgModifierColourPos, Just 2))
        ,("legitimacy_gain_factor"          , ("MODIFIER_LEGITIMACY_FACTOR", MsgModifierPcPosReduced, Just 0))

            -- Equipment
        ,("equipment_capture"               , ("MODIFIER_EQUIPMENT_CAPTURE", MsgModifierPcPosReduced, Just 1))
        ,("equipment_capture_factor"        , ("MODIFIER_EQUIPMENT_CAPTURE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("equipment_conversion_speed"      , ("EQUIPMENT_CONVERSION_SPEED_MODIFIERS", MsgModifierPcPosReduced, Just 0))
        ,("equipment_upgrade_xp_cost"       , ("MODIFIER_EQUIPMENT_UPGRADE_XP_COST", MsgModifierPcNegReduced, Just 0))
        ,("license_purchase_cost"           , ("MODIFIER_LICENSE_PURCHASE_COST", MsgModifierPcNegReduced, Just 0))
        ,("license_tech_difference_speed"   , ("MODIFIER_LICENSE_TECH_DIFFERENCE_SPEED", MsgModifierPcPosReduced, Just 0))
        ,("license_production_speed"        , ("MODIFIER_LICENSE_PRODUCTION_SPEED", MsgModifierPcPosReduced, Just 0))
        ,("license_armor_purchase_cost"     , ("MODIFIER_LICENSE_ARMOR_PURCHASE_COST", MsgModifierPcNegReduced, Just 0))
        ,("license_air_purchase_cost"       , ("MODIFIER_LICENSE_AIR_PURCHASE_COST", MsgModifierPcNegReduced, Just 0))
        ,("license_naval_purchase_cost"     , ("MODIFIER_LICENSE_NAVAL_PURCHASE_COST", MsgModifierPcNegReduced, Just 0))
        ,("production_factory_efficiency_gain_factor" , ("MODIFIER_PRODUCTION_FACTORY_EFFICIENCY_GAIN_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("production_factory_max_efficiency_factor" , ("MODIFIER_PRODUCTION_FACTORY_MAX_EFFICIENCY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("production_factory_start_efficiency_factor" , ("MODIFIER_PRODUCTION_FACTORY_START_EFFICIENCY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("production_lack_of_resource_penalty_factor" , ("MODIFIER_PRODUCTION_LACK_OF_RESOURCE_PENALTY_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("refit_speed"                     , ("MODIFIER_INDUSTRIAL_REFIT_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))

            -- Military outside of combat
        ,("command_power_gain"              , ("MODIFIER_COMMAND_POWER_GAIN", MsgModifierColourPos, Just 2))
        ,("command_power_gain_mult"         , ("MODIFIER_COMMAND_POWER_GAIN_MULT", MsgModifierPcPosReduced, Just 0))
        ,("conscription"                    , ("MODIFIER_CONSCRIPTION_FACTOR", MsgModifierPcReducedSignMin, Just 2)) --yellow
        ,("conscription_factor"             , ("MODIFIER_CONSCRIPTION_TOTAL_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("dig_in_speed_factor"             , ("MODIFIER_DIG_IN_SPEED_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("experience_gain_air"             , ("MODIFIER_XP_GAIN_AIR", MsgModifierColourPos, Just 2))
        ,("experience_gain_air_factor"      , ("MODIFIER_XP_GAIN_AIR_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("experience_gain_army"            , ("MODIFIER_XP_GAIN_ARMY", MsgModifierColourPos, Just 2))
        ,("experience_gain_army_factor"     , ("MODIFIER_XP_GAIN_ARMY_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("experience_gain_navy"            , ("MODIFIER_XP_GAIN_NAVY", MsgModifierColourPos, Just 2))
        ,("experience_gain_navy_factor"     , ("MODIFIER_XP_GAIN_NAVY_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("land_equipment_upgrade_xp_cost"  , ("MODIFIER_LAND_EQUIPMENT_UPGRADE_XP_COST", MsgModifierPcNegReduced, Just 0)) --precision 0
        ,("land_reinforce_rate"             , ("MODIFIER_LAND_REINFORCE_RATE", MsgModifierPcPosReduced, Just 1))
        ,("training_time_factor"            , ("MODIFIER_TRAINING_TIME_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("minimum_training_level"          , ("MODIFIER_MINIMUM_TRAINING_LEVEL", MsgModifierPcNegReduced, Just 0))
        ,("max_training"                    , ("MODIFIER_MAX_TRAINING_XP_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("air_doctrine_cost_factor"        , ("MODIFIER_AIR_DOCTRINE_COST_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("land_doctrine_cost_factor"       , ("MODIFIER_LAND_DOCTRINE_COST_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("naval_doctrine_cost_factor"      , ("MODIFIER_NAVAL_DOCTRINE_COST_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("max_command_power"               , ("MODIFIER_MAX_COMMAND_POWER", MsgModifierColourPos, Just 0))
        ,("max_command_power_mult"          , ("MODIFIER_MAX_COMMAND_POWER_MULT", MsgModifierPcPosReduced, Just 0))
        ,("training_time_army_factor"       , ("MODIFIER_TRAINING_TIME_ARMY_FACTOR", MsgModifierPcReducedSign, Just 1)) --yellow
        ,("weekly_manpower"                 , ("MODIFIER_WEEKLY_MANPOWER", MsgModifierColourPos, Just 0))
        ,("refit_ic_cost"                   , ("MODIFIER_INDUSTRIAL_REFIT_IC_COST_FACTOR", MsgModifierPcNegReduced, Just 0)) --precision 0
        ,("air_equipment_upgrade_xp_cost"   , ("MODIFIER_AIR_EQUIPMENT_UPGRADE_XP_COST", MsgModifierPcNegReduced, Just 0)) --precision 0
        ,("special_forces_training_time_factor", ("MODIFIER_SPECIAL_FORCES_TRAINING_TIME_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("command_abilities_cost_factor"   , ("MODIFIER_COMMAND_ABILITIES_COST_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("special_forces_cap_flat"         ,("MODIFIER_SPECIAL_FORCES_CAP_FLAT", MsgModifierColourPos, Just 0))

            -- Fuel and supplies
        ,("base_fuel_gain"                  , ("MODIFIER_BASE_FUEL_GAIN_ADD", MsgModifierColourPos, Just 0))
        ,("base_fuel_gain_factor"           , ("MODIFIER_BASE_FUEL_GAIN_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("fuel_cost"                       , ("MODIFIER_FUEL_COST", MsgModifierColourNeg, Just 0))
        ,("fuel_gain"                       , ("MODIFIER_FUEL_GAIN_ADD", MsgModifierColourPos, Just 2))
        ,("fuel_gain_factor"                , ("MODIFIER_MAX_FUEL_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("fuel_gain_factor_from_states"    , ("MODIFIER_FUEL_GAIN_FACTOR_FROM_STATES", MsgModifierPcPosReduced, Just 2))
        ,("max_fuel"                        , ("MODIFIER_MAX_FUEL_ADD", MsgModifierColourPos, Just 2))
        ,("max_fuel_factor"                 , ("MODIFIER_MAX_FUEL_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("army_fuel_consumption_factor"    , ("MODIFIER_ARMY_FUEL_CONSUMPTION_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("air_fuel_consumption_factor"     , ("MODIFIER_AIR_FUEL_CONSUMPTION_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("navy_fuel_consumption_factor"    , ("MODIFIER_NAVY_FUEL_CONSUMPTION_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("supply_factor"                   , ("MODIFIER_SUPPLY_FACTOR", MsgModifierPcPosReduced, Just 0)) --precision 0
        ,("supply_combat_penalties_on_core_factor" , ("supply_combat_penalties_on_core_factor", MsgModifierPcNegReduced, Just 1))
        ,("supply_consumption_factor"       , ("MODIFIER_SUPPLY_CONSUMPTION_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("no_supply_grace"                 , ("MODIFIER_NO_SUPPLY_GRACE", MsgModifierColourPos, Just 1))
        ,("out_of_supply_factor"            , ("MODIFIER_OUT_OF_SUPPLY_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("attrition"                       , ("MODIFIER_ATTRITION", MsgModifierPcNegReduced, Just 1))
        ,("naval_attrition"                 , ("MODIFIER_NAVAL_ATTRITION_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("heat_attrition"                  , ("MODIFIER_HEAT_ATTRITION", MsgModifierPcNegReduced, Just 1))
        ,("heat_attrition_factor"           , ("MODIFIER_HEAT_ATTRITION_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("winter_attrition_factor"         , ("MODIFIER_WINTER_ATTRITION_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("truck_attrition_factor"          , ("MODIFIER_TRUCK_ATTRITION_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("extra_marine_supply_grace"       , ("MODIFIER_MARINE_EXTRA_SUPPLY_GRACE", MsgModifierColourPos, Just 1))
        ,("extra_paratrooper_supply_grace"  , ("MODIFIER_PARATROOPER_EXTRA_SUPPLY_GRACE", MsgModifierColourPos, Just 1))
        ,("special_forces_no_supply_grace"  , ("MODIFIER_SPECIAL_FORCES_NO_SUPPLY_GRACE", MsgModifierColourPos, Just 1))
        ,("special_forces_out_of_supply_factor" , ("MODIFIER_SPECIAL_FORCES_OUT_OF_SUPPLY_FACTOR", MsgModifierPcNegReduced, Just 2))

            -- buildings
        ,("civilian_factory_use"            , ("MODIFIER_CIVILIAN_FACTORY_USE", MsgModifierColourNeg, Just 0))
        ,("industry_free_repair_factor"     , ("MODIFIER_INDUSTRY_FREE_REPAIR_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("consumer_goods_factor"           , ("MODIFIER_CONSUMER_GOODS_FACTOR", MsgModifierPcReducedSignMin, Just 1))
        ,("conversion_cost_civ_to_mil_factor" , ("MODIFIER_CONVERSION_COST_CIV_TO_MIL_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("conversion_cost_mil_to_civ_factor" , ("MODIFIER_CONVERSION_COST_MIL_TO_CIV_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("global_building_slots"           , ("MODIFIER_GLOBAL_BUILDING_SLOTS", MsgModifierPcPosReduced, Just 0))
        ,("global_building_slots_factor"    , ("MODIFIER_GLOBAL_BUILDING_SLOTS_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("industrial_capacity_dockyard"    , ("MODIFIER_INDUSTRIAL_CAPACITY_DOCKYARD_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("industrial_capacity_factory"     , ("MODIFIER_INDUSTRIAL_CAPACITY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("industry_air_damage_factor"      , ("MODIFIER_INDUSTRY_AIR_DAMAGE_FACTOR", MsgModifierPcNegReduced, Just 2)) --precision 2
        ,("industry_repair_factor"          , ("MODIFIER_INDUSTRY_REPAIR_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("line_change_production_efficiency_factor" , ("MODIFIER_LINE_CHANGE_PRODUCTION_EFFICIENCY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("production_oil_factor"           , ("MODIFIER_PRODUCTION_OIL_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("production_speed_buildings_factor" , ("MODIFIER_PRODUCTION_SPEED_BUILDINGS_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("supply_node_range"               , ("MODIFIER_SUPPLY_NODE_RANGE", MsgModifierPcPosReduced, Just 0))
        ,("static_anti_air_damage_factor"   , ("MODIFIER_STATIC_ANTI_AIR_DAMAGE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("static_anti_air_hit_chance_factor" , ("MODIFIER_STATIC_ANTI_AIR_HIT_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("coastal_bunker_effectiveness_factor" , ("MODIFIER_COASTAL_BUNKER_EFFECTIVENESS_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("land_bunker_effectiveness_factor" , ("MODIFIER_LAND_BUNKER_EFFECTIVENESS_FACTOR", MsgModifierPcPosReduced, Just 0))

            -- resistance and compliance
        ,("compliance_growth_on_our_occupied_states" , ("MODIFIER_COMPLIANCE_GROWTH_ON_OUR_OCCUPIED_STATES", MsgModifierPcNegReduced, Just 0))
        ,("no_compliance_gain"              , ("MODIFIER_NO_COMPLIANCE_GAIN", modNoYes, Just 0))
        ,("occupation_cost"                 , ("MODIFIER_OCCUPATION_COST", MsgModifierColourNeg, Nothing))
        ,("required_garrison_factor"        , ("MODIFIER_REQUIRED_GARRISON_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("resistance_activity"             , ("MODIFIER_RESISTANCE_ACTIVITY_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("resistance_damage_to_garrison_on_our_occupied_states" , ("MODIFIER_RESISTANCE_DAMAGE_TO_GARRISONS_ON_OUR_OCCUPIED_STATES", MsgModifierPcPosReduced, Just 2))
        ,("resistance_decay_on_our_occupied_states" , ("MODIFIER_RESISTANCE_DECAY_ON_OUR_OCCUPIED_STATES", MsgModifierPcNegReduced, Just 0))
        ,("resistance_growth_on_our_occupied_states" , ("MODIFIER_RESISTANCE_GROWTH_ON_OUR_OCCUPIED_STATES", MsgModifierPcPosReduced, Just 0))
        ,("resistance_target_on_our_occupied_states" , ("MODIFIER_RESISTANCE_TARGET_ON_OUR_OCCUPIED_STATES", MsgModifierPcPosReduced, Just 0))

            -- Intelligence
        ,("agency_upgrade_time"             , ("MODIFIER_AGENCY_UPGRADE_TIME", MsgModifierPcNegReduced, Just 1))
        ,("decryption"                      , ("MODIFIER_DECRYPTION", MsgModifierColourPos, Just 2))
        ,("decryption_factor"               , ("MODIFIER_DECRYPTION_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("encryption"                      , ("MODIFIER_ENCRYPTION", MsgModifierColourPos, Just 2))
        ,("encryption_factor"               , ("MODIFIER_ENCRYPTION_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("civilian_intel_factor"           , ("MODIFIER_CIVILIAN_INTEL_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("army_intel_factor"               , ("MODIFIER_ARMY_INTEL_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("navy_intel_factor"               , ("MODIFIER_NAVY_INTEL_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("airforce_intel_factor"           , ("MODIFIER_AIRFORCE_INTEL_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("civilian_intel_to_others"        , ("MODIFIER_CIVILIAN_INTEL_TO_OTHERS", MsgModifierPcNeg, Just 1))
        ,("army_intel_to_others"            , ("MODIFIER_ARMY_INTEL_TO_OTHERS", MsgModifierPcNeg, Just 1))
        ,("navy_intel_to_others"            , ("MODIFIER_NAVY_INTEL_TO_OTHERS", MsgModifierPcNeg, Just 1))
        ,("airforce_intel_to_others"        , ("MODIFIER_AIRFORCE_INTEL_TO_OTHERS", MsgModifierPcNeg, Just 1))
        ,("intel_network_gain"              , ("MODIFIER_INTEL_NETWORK_GAIN", MsgModifierColourPos, Just 1))
        ,("intel_network_gain_factor"       , ("MODIFIER_INTEL_NETWORK_GAIN_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("subversive_activites_upkeep"     , ("MODIFIER_SUBVERSIVE_ACTIVITES_UPKEEP", MsgModifierPcNegReduced, Just 0))
        ,("target_sabotage_risk"            , ("target_sabotage_risk", MsgModifierPcNegReduced, Nothing))
        ,("target_sabotage_cost"            , ("target_sabotage_cost", MsgModifierPcNegReduced, Nothing))
        ,("diplomatic_pressure_mission_factor" , ("MODIFIER_DIPLOMATIC_PRESSURE_MISSION_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("control_trade_mission_factor"    , ("MODIFIER_CONTROL_TRADE_MISSION_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("boost_ideology_mission_factor"   , ("MODIFIER_BOOST_IDEOLOGY_MISSION_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("boost_resistance_factor"         , ("MODIFIER_BOOST_RESISTANCE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("propaganda_mission_factor"       , ("MODIFIER_PROPAGANDA_MISSION_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("target_sabotage_factor"          , ("MODIFIER_TARGET_SABOTAGE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("crypto_strength"                 , ("MODIFIER_CRYPTO_STRENGTH", MsgModifierColourPos, Just 0))
        ,("decryption_power"                , ("MODIFIER_DECRYPTION_POWER", MsgModifierColourPos, Just 0))
        ,("decryption_power_factor"         , ("MODIFIER_DECRYPTION_POWER_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("intel_from_combat_factor"        , ("MODIFIER_INTEL_FROM_COMBAT_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("intel_from_operatives_factor"    , ("MODIFIER_INTEL_FROM_OPERATIVES_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("civilian_intel_to_others"        , ("MODIFIER_CIVILIAN_INTEL_TO_OTHERS", MsgModifierPcNeg, Just 1))
        ,("foreign_subversive_activites"    , ("MODIFIER_FOREIGN_SUBVERSIVE_ACTIVITIES", MsgModifierPcNegReduced, Just 0))
        ,("intelligence_agency_defense"     , ("MODIFIER_INTELLIGENCE_AGENCY_DEFENSE", MsgModifierColourPos, Just 2))
        ,("root_out_resistance_effectiveness_factor", ("MODIFIER_ROOT_OUT_RESISTANCE_EFFECTIVENESS_FACTOR", MsgModifierPcPosReduced, Just 0))

            -- Operatives
        ,("own_operative_detection_chance_factor" , ("MODIFIER_OWN_OPERATIVE_DETECTION_CHANCE_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("enemy_operative_capture_chance_factor" , ("MODIFIER_ENEMY_OPERATIVE_CAPTURE_CHANCE_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("enemy_operative_detection_chance" , ("MODIFIER_ENEMY_OPERATIVE_DETECTION_CHANCE", MsgModifierPcPos, Just 2))
        ,("enemy_operative_detection_chance_factor" , ("MODIFIER_ENEMY_OPERATIVE_DETECTION_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("enemy_operative_intel_extraction_rate" , ("MODIFIER_ENEMY_OPERATIVE_INTEL_EXTRACTION_RATE", MsgModifierPcNegReduced, Just 0))
        ,("new_operative_slot_bonus"        , ("MODIFIER_NEW_OPERATIVE_SLOT_BONUS", MsgModifierColourPos, Just 0))
        ,("operative_slot"                  , ("MODIFIER_OPERATIVE_SLOT", MsgModifierColourPos, Just 0))

            -- AI
        ,("ai_badass_factor"                , ("MODIFIER_AI_BADASS_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_call_ally_desire_factor"      , ("MODIFIER_AI_GET_ALLY_DESIRE_FACTOR", MsgModifierSign, Just 0))
        ,("ai_desired_divisions_factor"     , ("MODIFIER_AI_DESIRED_DIVISIONS_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_focus_aggressive_factor"      , ("MODIFIER_AI_FOCUS_AGGRESSIVE_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_focus_defense_factor"         , ("MODIFIER_AI_FOCUS_DEFENSE_FACTOR", MsgModifierPcReducedSign, Just 1)) --precision 1
        ,("ai_focus_aviation_factor"        , ("MODIFIER_AI_FOCUS_AVIATION_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_focus_military_advancements_factor" , ("MODIFIER_AI_FOCUS_MILITARY_ADVANCEMENTS_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_focus_military_equipment_factor" , ("MODIFIER_AI_FOCUS_MILITARY_EQUIPMENT_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_focus_naval_air_factor"       , ("MODIFIER_AI_FOCUS_NAVAL_AIR_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_focus_naval_factor"           , ("MODIFIER_AI_FOCUS_NAVAL_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_focus_war_production_factor"  , ("MODIFIER_AI_FOCUS_WAR_PRODUCTION_FACTOR", MsgModifierPcReducedSign, Just 1))
        ,("ai_focus_peaceful_factor"        , ("MODIFIER_AI_FOCUS_PEACEFUL_FACTOR", MsgModifierPcReducedSign, Just 1)) --precision 1
        ,("ai_get_ally_desire_factor"       , ("MODIFIER_AI_GET_ALLY_DESIRE_FACTOR", MsgModifierSign, Just 0))
        ,("ai_join_ally_desire_factor"      , ("MODIFIER_AI_JOIN_ALLY_DESIRE_FACTOR", MsgModifierSign, Just 0))
        ,("ai_license_acceptance"           , ("MODIFIER_AI_LICENSE_ACCEPTANCE", MsgModifierSign, Just 0))

            -- MIOs
        ,("military_industrial_organization_funds_gain" , ("MODIFIER_MIO_FUNDS_GAIN", MsgModifierPcPosReduced, Just 0))

            -- Unit Leaders
        ,("female_random_army_leader_chance", ("MODIFIER_FEMALE_ARMY_LEADER_CHANCE", MsgModifierPcReducedSign, Just 0))
        ,("army_leader_cost_factor"         , ("MODIFIER_ARMY_LEADER_COST_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("army_leader_start_level"         , ("MODIFIER_ARMY_LEADER_START_LEVEL", MsgModifierColourPos, Just 0))
        ,("army_leader_start_attack_level"  , ("MODIFIER_ARMY_LEADER_START_ATTACK_LEVEL", MsgModifierColourPos, Just 0))
        ,("army_leader_start_defense_level" , ("MODIFIER_ARMY_LEADER_START_DEFENSE_LEVEL", MsgModifierColourPos, Just 0))
        ,("army_leader_start_logistics_level" , ("MODIFIER_ARMY_LEADER_START_LOGISTICS_LEVEL", MsgModifierColourPos, Just 0))
        ,("army_leader_start_planning_level" , ("MODIFIER_ARMY_LEADER_START_PLANNING_LEVEL", MsgModifierColourPos, Just 0))
        ,("military_leader_cost_factor"     , ("MODIFIER_MILITARY_LEADER_COST_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("navy_leader_start_attack_level"  , ("MODIFIER_NAVY_LEADER_START_ATTACK_LEVEL", MsgModifierColourPos, Just 0)) --precision 0
        ,("grant_medal_cost_factor"         , ("MODIFIER_GRANT_MEDAL_COST_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("female_divisional_commander_chance", ("MODIFIER_FEMALE_DIVISIONAL_COMMANDER_CHANCE", MsgModifierPcReducedSign, Just 0))

            -- General Combat
        ,("offence"                         , ("MODIFIER_OFFENCE", MsgModifierPcPosReduced, Just 2))
        ,("defence"                         , ("MODIFIER_DEFENCE", MsgModifierPcPosReduced, Just 2))

            -- Land Combat
        ,("acclimatization_cold_climate_gain_factor", ("MODIFIER_ACCLIMATIZATION_COLD_CLIMATE_GAIN_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("acclimatization_hot_climate_gain_factor", ("MODIFIER_ACCLIMATIZATION_HOT_CLIMATE_GAIN_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("air_superiority_bonus_in_combat" , ("MODIFIER_AIR_SUPERIORITY_BONUS_IN_COMBAT", MsgModifierPcPosReduced, Just 1))
        ,("army_attack_factor"              , ("MODIFIERS_ARMY_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_core_attack_factor"         , ("MODIFIERS_ARMY_CORE_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_attack_against_major_factor", ("MODIFIERS_ARMY_ATTACK_AGAINST_MAJOR_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_attack_against_minor_factor", ("MODIFIERS_ARMY_ATTACK_AGAINST_MINOR_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_attack_speed_factor"        , ("MODIFIER_ARMY_ATTACK_SPEED_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("army_breakthrough_against_major_factor", ("MODIFIERS_ARMY_BREAKTHROUGH_AGAINST_MAJOR_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_breakthrough_against_minor_factor", ("MODIFIERS_ARMY_BREAKTHROUGH_AGAINST_MINOR_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_defence_factor"             , ("MODIFIERS_ARMY_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_core_defence_factor"        , ("MODIFIERS_ARMY_CORE_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_strength_factor"            , ("MODIFIER_ARMY_STRENGTH", MsgModifierPcPosReduced, Just 2)) --precision 2
        ,("army_infantry_attack_factor"     , ("MODIFIER_ARMY_INFANTRY_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_infantry_defence_factor"    , ("MODIFIER_ARMY_INFANTRY_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_armor_attack_factor"        , ("MODIFIER_ARMY_ARMOR_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_armor_defence_factor"       , ("MODIFIER_ARMY_ARMOR_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_artillery_attack_factor"    , ("MODIFIER_ARMY_ARTILLERY_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_artillery_defence_factor"   , ("MODIFIER_ARMY_ARTILLERY_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("special_forces_attack_factor"    , ("MODIFIER_SPECIAL_FORCES_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("special_forces_defence_factor"   , ("MODIFIER_SPECIAL_FORCES_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("motorized_attack_factor"         , ("MODIFIER_MOTORIZED_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("motorized_defence_factor"        , ("MODIFIER_MOTORIZED_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("mechanized_attack_factor"        , ("MODIFIER_MECHANIZED_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("mechanized_defence_factor"       , ("MODIFIER_MECHANIZED_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("cavalry_attack_factor"           , ("MODIFIER_CAVALRY_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("cavalry_defence_factor"          , ("MODIFIER_CAVALRY_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_speed_factor"               , ("MODIFIER_ARMY_SPEED_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("army_armor_speed_factor"         , ("MODIFIER_ARMY_ARMOR_SPEED_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_morale_factor"              , ("MODIFIER_ARMY_MORALE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_org"                        , ("MODIFIER_ARMY_ORG", MsgModifierColourPos, Just 1))
        ,("army_org_factor"                 , ("MODIFIER_ARMY_ORG_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_org_regain"                 , ("MODIFIER_ARMY_ORG_REGAIN", MsgModifierPcPosReduced, Just 2))
        ,("breakthrough_factor"             , ("MODIFIER_BREAKTHROUGH", MsgModifierPcPosReduced, Just 2))
        ,("cas_damage_reduction"            , ("MODIFIER_CAS_DAMAGE_REDUCTION", MsgModifierPcPosReduced, Just 1))
        ,("combat_width_factor"             , ("MODIFIER_COMBAT_WIDTH_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("coordination_bonus"              , ("MODIFIER_COORDINATION_BONUS", MsgModifierPcPosReduced, Just 1))
        ,("dig_in_speed"                    , ("MODIFIER_DIG_IN_SPEED", MsgModifierColourPos, Just 0))
        ,("dig_in_speed_factor"             , ("MODIFIER_DIG_IN_SPEED_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("experience_gain_army_unit_factor" , ("MODIFIER_XP_GAIN_ARMY_UNIT_FACTOR", MsgModifierPcPosReduced, Just 1)) --precision 1
        ,("experience_loss_factor"          , ("MODIFIER_EXPERIENCE_LOSS_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("initiative_factor"               , ("MODIFIER_INITIATIVE_FACTOR", MsgModifierPcPosReduced, Just 1)) --precision 1
        ,("land_night_attack"               , ("MODIFIER_LAND_NIGHT_ATTACK", MsgModifierPcPosReduced, Just 1))
        ,("max_dig_in"                      , ("MODIFIER_MAX_DIG_IN", MsgModifierColourPos, Just 1))
        ,("max_dig_in_factor"               , ("MODIFIER_MAX_DIG_IN_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("max_planning"                    , ("MODIFIER_MAX_PLANNING", MsgModifierPcPosReduced, Just 1))
        ,("max_planning_factor"             , ("MODIFIER_MAX_PLANNING_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("pocket_penalty"                  , ("MODIFIER_POCKET_PENALTY", MsgModifierPcNegReduced, Just 1))
        ,("recon_factor"                    , ("MODIFIER_RECON_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("recon_factor_while_entrenched"   , ("MODIFIER_RECON_FACTOR_WHILE_ENTRENCHED", MsgModifierPcPosReduced, Just 1))
        ,("special_forces_cap"              , ("MODIFIER_SPECIAL_FORCES_CAP", MsgModifierPcPosReduced, Just 1))
        ,("special_forces_min"              , ("MODIFIER_SPECIAL_FORCES_MIN", MsgModifierColourPos, Just 0))
        ,("terrain_penalty_reduction"       , ("MODIFIER_TERRAIN_PENALTY_REDUCTION", MsgModifierPcPosReduced, Just 1))
        ,("org_loss_at_low_org_factor"      , ("MODIFIER_ORG_LOSS_AT_LOW_ORG_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("org_loss_when_moving"            , ("MODIFIER_ORG_LOSS_WHEN_MOVING", MsgModifierPcNegReduced, Just 1))
        ,("planning_speed"                  , ("MODIFIER_PLANNING_SPEED", MsgModifierPcPosReduced, Just 1))

            -- naval invasions
        ,("naval_invasion_prep_speed"       , ("MODIFIER_NAVAL_INVASION_PREPARATION_SPEED", MsgModifierPcPosReduced, Just 1)) --precision 1
        ,("naval_invasion_capacity"         , ("MODIFIER_NAVAL_INVASION_CAPACITY", MsgModifierColourPos, Just 0)) --precision 0
        ,("amphibious_invasion"             , ("MODIFIER_AMPHIBIOUS_INVASION", MsgModifierPcPosReduced, Just 1))
        ,("amphibious_invasion_defence"     , ("MODIFIER_NAVAL_INVASION_DEFENSE", MsgModifierPcPosReduced, Just 0))
        ,("invasion_preparation"            , ("MODIFIER_NAVAL_INVASION_PREPARATION", MsgModifierPcNegReduced, Just 1))

            -- Naval combat
        ,("convoy_escort_efficiency"        , ("MODIFIER_MISSION_CONVOY_ESCORT_EFFICIENCY", MsgModifierPcPosReduced, Just 1))
        ,("convoy_raiding_efficiency_factor" , ("MODIFIER_CONVOY_RAIDING_EFFICIENCY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("convoy_retreat_speed"            , ("MODIFIER_CONVOY_RETREAT_SPEED", MsgModifierPcPosReduced, Just 0))
        ,("critical_receive_chance"         , ("MODIFIER_NAVAL_CRITICAL_RECEIVE_CHANCE_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("experience_gain_navy_unit_factor" , ("MODIFIER_XP_GAIN_NAVY_UNIT_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("mines_planting_by_fleets_factor" , ("MODIFIER_MINES_PLANTING_BY_FLEETS_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("mines_sweeping_by_fleets_factor" , ("MODIFIER_MINES_SWEEPING_BY_FLEETS_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("navy_anti_air_attack_factor"     , ("MODIFIER_NAVY_ANTI_AIR_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_coordination"              , ("MODIFIER_NAVAL_COORDINATION", MsgModifierPcPosReduced, Just 0))
        ,("naval_critical_effect_factor"    , ("MODIFIER_NAVAL_CRITICAL_EFFECT_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("naval_critical_score_chance_factor" , ("MODIFIER_NAVAL_CRITICAL_SCORE_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_damage_factor"             , ("MODIFIER_NAVAL_DAMAGE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_defense_factor"            , ("MODIFIER_NAVAL_DEFENSE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_detection"                 , ("MODIFIER_NAVAL_DETECTION", MsgModifierPcPosReduced, Just 0))
        ,("naval_enemy_fleet_size_ratio_penalty_factor" , ("MODIFIER_NAVAL_ENEMY_FLEET_SIZE_RATIO_PENALTY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_enemy_retreat_chance"      , ("MODIFIER_NAVAL_ENEMY_RETREAT_CHANCE", MsgModifierPcNegReduced, Just 2))
        ,("naval_has_potf_in_combat_attack" , ("MODIFIER_NAVAL_HAS_POTF_IN_COMBAT_ATTACK", MsgModifierPcPosReduced, Just 2))
        ,("naval_has_potf_in_combat_defense" , ("MODIFIER_NAVAL_HAS_POTF_IN_COMBAT_DEFENSE", MsgModifierPcPosReduced, Just 2))
        ,("naval_hit_chance"                , ("MODIFIER_NAVAL_HIT_CHANCE", MsgModifierPcPosReduced, Just 0))
        ,("naval_mines_effect_reduction"    , ("MODIFIER_NAVAL_MINES_EFFECT_REDUCTION", MsgModifierPcPosReduced, Just 0))
        ,("naval_morale_factor"             , ("MODIFIER_NAVAL_MORALE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("naval_night_attack"             , ("MODIFIER_NAVAL_MORALE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("naval_retreat_chance"            , ("MODIFIER_NAVAL_RETREAT_CHANCE", MsgModifierPcPosReduced, Just 0))
        ,("naval_retreat_speed"             , ("MODIFIER_NAVAL_RETREAT_SPEED", MsgModifierPcPosReduced, Just 1))
        ,("navy_org"                        , ("MODIFIER_NAVY_ORG", MsgModifierColourPos, Just 1))
        ,("navy_org_factor"                 , ("MODIFIER_NAVY_ORG_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("navy_max_range_factor"           , ("MODIFIER_NAVY_MAX_RANGE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_torpedo_cooldown_factor"   , ("MODIFIER_NAVAL_TORPEDO_COOLDOWN_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("naval_torpedo_hit_chance_factor" , ("MODIFIER_NAVAL_TORPEDO_HIT_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_torpedo_reveal_chance_factor" , ("MODIFIER_NAVAL_TORPEDO_REVEAL_CHANCE_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("naval_torpedo_screen_penetration_factor" , ("MODIFIER_NAVAL_TORPEDO_SCREEN_PENETRATION_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_capital_ship_attack_factor" , ("MODIFIER_NAVY_CAPITAL_SHIP_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_capital_ship_defence_factor" , ("MODIFIER_NAVY_CAPITAL_SHIP_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_screen_attack_factor"       , ("MODIFIER_NAVY_SCREEN_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_screen_defence_factor"      , ("MODIFIER_NAVY_SCREEN_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_speed_factor"              , ("MODIFIER_NAVAL_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("navy_visibility"                 , ("MODIFIER_NAVAL_VISIBILITY_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("navy_submarine_attack_factor"    , ("MODIFIER_NAVY_SUBMARINE_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_submarine_defence_factor"   , ("MODIFIER_NAVY_SUBMARINE_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_submarine_detection_factor" , ("MODIFIERS_SUBMARINE_DETECTION_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("positioning"                     , ("MODIFIER_POSITIONING", MsgModifierPcPosReduced, Just 1))
        ,("repair_speed_factor"             , ("MODIFIER_REPAIR_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("screening_efficiency"            , ("MODIFIER_SCREENING_EFFICIENCY", MsgModifierPcPosReduced, Just 1))
        ,("screening_without_screens"       , ("MODIFIER_SCREENING_WITHOUT_SCREENS", MsgModifierPcPosReduced, Just 1))
        ,("ships_at_battle_start"           , ("MODIFIER_SHIPS_AT_BATTLE_START_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("spotting_chance"                 , ("MODIFIER_SPOTTING_CHANCE", MsgModifierPcPosReduced, Just 0))
        ,("strike_force_movement_org_loss"  , ("MODIFIER_STRIKE_FORCE_MOVING_ORG", MsgModifierPcNegReduced, Just 2))--precision 2
        ,("sub_retreat_speed"               , ("MODIFIER_SUB_RETREAT_SPEED", MsgModifierPcPosReduced, Just 0)) --precision 0

            -- carriers and their planes
        ,("navy_carrier_air_agility_factor" , ("MODIFIER_NAVAL_CARRIER_AIR_AGILITY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_carrier_air_attack_factor"  , ("MODIFIER_NAVAL_CARRIER_AIR_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_carrier_air_targetting_factor" , ("MODIFIER_NAVAL_CARRIER_AIR_TARGETTING_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("air_carrier_night_penalty_reduction_factor" , ("MODIFIER_AIR_CARRIER_NIGHT_PENALTY_REDUCTION_FACTOR", MsgModifierPcPosReduced, Just 2)) --precision 2
        ,("sortie_efficiency"               , ("MODIFIER_STAT_CARRIER_SORTIE_EFFICIENCY", MsgModifierPcPosReduced, Just 0))
        ,("fighter_sortie_efficiency"       , ("MODIFIER_CARRIER_FIGHTER_SORTIE_EFFICIENCY_FACTOR", MsgModifierPcPosReduced, Just 0))

            -- Air combat
        ,("air_accidents_factor"            , ("MODIFIER_AIR_ACCIDENTS_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("air_ace_generation_chance_factor" , ("MODIFIER_AIR_ACE_GENERATION_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("air_agility_factor"              , ("MODIFIER_AIR_AGILITY_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_attack_factor"               , ("MODIFIER_AIR_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_defence_factor"              , ("MODIFIER_AIR_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_close_air_support_org_damage_factor" , ("MODIFIER_AIR_CAS_ORG_DAMAGE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("rocket_attack_factor"            , ("MODIFIER_ROCKET_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))

        ,("air_close_air_support_agility_factor" , ("MODIFIER_CAS_AGILITY_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_close_air_support_attack_factor" , ("MODIFIER_CAS_ATTACK_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_close_air_support_defence_factor" , ("MODIFIER_CAS_DEFENCE_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_air_superiority_agility_factor", ("MODIFIER_AIR_SUPERIORITY_AGILITY_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_air_superiority_attack_factor", ("MODIFIER_AIR_SUPERIORITY_ATTACK_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_air_superiority_defence_factor", ("MODIFIER_AIR_SUPERIORITY_DEFENCE_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_interception_agility_factor"  , ("MODIFIER_INTERCEPTION_AGILITY_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_interception_attack_factor"  , ("MODIFIER_INTERCEPTION_ATTACK_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_interception_defence_factor" , ("MODIFIER_INTERCEPTION_DEFENCE_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_strategic_bomber_agility_factor" , ("MODIFIER_STRATEGIC_BOMBER_AGILITY_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_strategic_bomber_attack_factor" , ("MODIFIER_STRATEGIC_BOMBER_ATTACK_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_strategic_bomber_defence_factor" , ("MODIFIER_STRATEGIC_BOMBER_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_strike_agility_factor"     , ("MODIFIER_NAVAL_STRIKE_AGILITY_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("naval_strike_attack_factor"      , ("MODIFIER_NAVAL_STRIKE_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_paradrop_attack_factor"      , ("MODIFIER_PARADROP_ATTACK_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_paradrop_agility_factor"     , ("MODIFIER_AIR_SUPERIORITY_AGILITY_FACTOR", MsgModifierPcPosReduced, Nothing))
        ,("air_paradrop_defence_factor"     , ("MODIFIER_PARADROP_DEFENCE_FACTOR", MsgModifierPcPosReduced, Nothing))

        ,("naval_strike_targetting_factor"  , ("MODIFIER_NAVAL_STRIKE_TARGETTING_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_bombing_targetting"          , ("MODIFIER_AIR_BOMBING_TARGETTING", MsgModifierPcPosReduced, Just 1))
        ,("air_cas_efficiency"              , ("MODIFIER_AIR_CAS_EFFICIENCY", MsgModifierPcPosReduced, Just 0))
        ,("air_cas_present_factor"          , ("MODIFIER_AIR_CAS_PRESENT_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("air_intercept_efficiency"        , ("MODIFIER_AIR_INTERCEPT_EFFICIENCY", MsgModifierPcPosReduced, Just 0))
        ,("air_maximum_speed_factor"        , ("MODIFIER_AIR_MAX_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_mission_efficiency"          , ("MODIFIER_AIR_MISSION_EFFICIENCY", MsgModifierPcPosReduced, Just 1))
        ,("air_mission_xp_gain_factor"      , ("MODIFIER_AIR_MISSION_XP_FACTOR", MsgModifierPcPosReduced, Just 0)) --precision 0
        ,("air_nav_efficiency"              , ("MODIFIER_AIR_NAV_EFFICIENCY", MsgModifierPcPosReduced, Just 0)) --precison 0
        ,("air_night_penalty"               , ("MODIFIER_AIR_NIGHT_PENALTY", MsgModifierPcNegReduced, Just 2))
        ,("air_range_factor"                , ("MODIFIER_AIR_RANGE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_strategic_bomber_bombing_factor" , ("MODIFIER_STRATEGIC_BOMBER_BOMBING_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("air_strategic_bomber_night_penalty" , ("MODIFIER_AIR_STRAT_BOMBER_NIGHT_PENALTY", MsgModifierPcNegReduced, Just 2)) --precision 2
        ,("air_superiority_efficiency"      , ("MODIFIER_AIR_SUPERIORITY_EFFICIENCY", MsgModifierPcPosReduced, Just 0)) --precision 0
        ,("air_training_xp_gain_factor"     , ("MODIFIER_AIR_TRAINING_XP_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_weather_penalty"             , ("MODIFIER_AIR_WEATHER_PENALTY", MsgModifierPcNegReduced, Just 2))
        ,("air_wing_xp_loss_when_killed_factor" , ("MODIFIER_AIR_WING_XP_LOSS_WHEN_KILLED_FACTOR", MsgModifierPcNegReduced, Just 0)) --precision 0
        ,("army_bonus_air_superiority_factor" , ("MODIFIER_ARMY_BONUS_AIR_SUPERIORITY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("enemy_army_bonus_air_superiority_factor" , ("MODIFIER_ENEMY_ARMY_BONUS_AIR_SUPERIORITY_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("ground_attack_factor"            , ("MODIFIER_GROUND_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1)) --precision 1
        ,("mines_planting_by_air_factor"    , ("MODIFIER_MINES_PLANTING_BY_AIR_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("strategic_bomb_visibility"       , ("MODIFIER_STRAT_BOMBING_VISIBILITY", MsgModifierPcNegReduced, Just 0)) --precison 0

            -- targeted
        ,("extra_trade_to_target_factor"    , ("MODIFIER_TRADE_TO_TARGET_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("trade_cost_for_target_factor"    , ("MODIFIER_TRADE_COST_TO_TARGET_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("generate_wargoal_tension_against" , ("MODIFIER_GENERATE_WARGOAL_TENSION_LIMIT_AGAINST_COUNTRY", MsgModifierPcReducedSign, Just 1))
        ,("attack_bonus_against"            , ("MODIFIER_ATTACK_BONUS_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 1))
        ,("attack_bonus_against_cores"      , ("MODIFIER_ATTACK_BONUS_AGAINST_A_COUNTRY_ON_ITS_CORES", MsgModifierPcPosReduced, Just 1))
        ,("cic_to_target_factor"            , ("MODIFIER_CIC_TO_TARGET_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("mic_to_target_factor"            , ("MODIFIER_MIC_TO_TARGET_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("targeted_legitimacy_daily"       , ("MODIFIER_TARGETED_LEGITIMACY_DAILY", MsgModifierColourPos, Just 2))
        ,("breakthrough_bonus_against"      , ("MODIFIER_BREAKTHROUGH_BONUS_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 1))
        ,("defense_bonus_against"           , ("MODIFIER_DEFENSE_BONUS_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 1))

        -- State Scope
        ,("army_speed_factor_for_controller" , ("MODIFIER_ARMY_SPEED_FACTOR_FOR_CONTROLLER", MsgModifierPcPosReduced, Just 2))
        ,("attrition_for_controller"        , ("MODIFIER_ATTRITION_FOR_CONTROLLER", MsgModifierPcNegReduced, Just 1)) --precision 1
        ,("compliance_gain"                 , ("MODIFIER_COMPLIANCE_GAIN_ADD", MsgModifierPcPos, Just 3))
        ,("compliance_growth"               , ("MODIFIER_COMPLIANCE_GROWTH", MsgModifierPcPosReduced, Just 0))
        ,("disable_strategic_redeployment"  , ("MODIFIER_STRATEGIC_REDEPLOYMENT_DISABLED", modNoYes, Just 0))
        ,("enemy_intel_network_gain_factor_over_occupied_tag" , ("MODIFIER_ENEMY_INTEL_NETWORK_GAIN_FACTOR_OVER_OCCUPIED_TAG", MsgModifierPcNegReduced, Just 0))
        ,("local_building_slots"            , ("MODIFIER_LOCAL_BUILDING_SLOTS", MsgModifierPcPos, Just 0))
        ,("local_building_slots_factor"     , ("MODIFIER_LOCAL_BUILDING_SLOTS_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("local_factories"                 , ("MODIFIER_LOCAL_FACTORIES", MsgModifierPcPosReduced, Just 0))
        ,("local_factory_sabotage"         , ("MODIFIER_LOCAL_FACTORY_SABOTAGE", MsgModifierPcNegReduced, Just 0)) --precision 0
        ,("local_intel_to_enemies"          , ("MODIFIER_LOCAL_INTEL_TO_ENEMIES", MsgModifierPcNegReduced, Just 0))
        ,("local_manpower"                  , ("MODIFIER_LOCAL_MANPOWER", MsgModifierPcPosReduced, Just 0))
        ,("local_non_core_manpower"         , ("MODIFIER_LOCAL_NON_CORE_MANPOWER", MsgModifierPcPosReduced, Just 2))
        ,("local_org_regain"                , ("MODIFIER_LOCAL_ORG_REGAIN", MsgModifierPcPosReduced, Just 2))
        ,("local_resources"                 , ("MODIFIER_LOCAL_RESOURCES", MsgModifierPcPosReduced, Just 0))
        ,("local_supplies"                  , ("MODIFIER_LOCAL_SUPPLIES", MsgModifierPcPosReduced, Just 0))
        ,("local_supplies_for_controller"   , ("MODIFIER_LOCAL_SUPPLIES_FOR_CONTROLLER", MsgModifierPcPosReduced, Just 0)) --precision 0
        ,("local_supply_impact_factor"      , ("MODIFIER_LOCAL_SUPPLY_IMPACT", MsgModifierPcNegReduced, Just 0)) --precision 0
        ,("local_non_core_supply_impact_factor" , ("MODIFIER_LOCAL_NON_CORE_SUPPLY_IMPACT", MsgModifierPcNegReduced, Just 0)) --precision 0
        ,("mobilization_speed"              , ("MODIFIER_MOBILIZATION_SPEED", MsgModifierPcPosReduced, Just 2))
        ,("non_core_manpower"               , ("MODIFIER_GLOBAL_NON_CORE_MANPOWER", MsgModifierPcPosReduced, Just 2))
        ,("non_core_manpower"               , ("MODIFIER_GLOBAL_NON_CORE_MANPOWER", MsgModifierPcPosReduced, Just 2))
        ,("recruitable_population_factor"   , ("MODIFIER_RECRUITABLE_POPULATION_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("resistance_damage_to_garrison"   , ("MODIFIER_RESISTANCE_DAMAGE_TO_GARRISONS", MsgModifierPcNegReduced, Just 2))
        ,("resistance_decay"                , ("MODIFIER_RESISTANCE_DECAY", MsgModifierPcPosReduced, Just 0))
        ,("resistance_garrison_penetration_chance" , ("MODIFIER_RESISTANCE_GARRISON_PENETRATION_CHANCE", MsgModifierPcNegReduced, Just 2))
        ,("resistance_growth"               , ("MODIFIER_RESISTANCE_GROWTH", MsgModifierPcNegReduced, Just 0))
        ,("resistance_target"               , ("MODIFIER_RESISTANCE_TARGET", MsgModifierPcNegReduced, Just 0))
        ,("starting_compliance"             , ("MODIFIER_COMPLIANCE_STARTING_VALUE", MsgModifierPcPosReduced, Just 0))
        ,("state_resources_factor"          , ("MODIFIER_STATE_RESOURCES_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("state_production_speed_buildings_factor" , ("MODIFIER_STATE_PRODUCTION_SPEED_BUILDINGS_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("enemy_operative_detection_chance_factor_over_occupied_tag" , ("MODIFIER_ENEMY_OPERATIVE_DETECTION_CHANCE_FACTOR_OVER_OCCUPIED_TAG", MsgModifierPcPosReduced, Just 0)) --precision 0

        -- Unit Leader Scope
        ,("cannot_use_abilities"            , ("MODIFIER_CANNOT_USE_ABILITIES", modNoYes, Just 0))
        ,("dont_lose_dig_in_on_attack"      , ("MODIFIER_DONT_LOSE_DIGIN_ON_ATTACK_MOVE", modYesNo, Just 0))
        ,("exiled_divisions_attack_factor"  , ("MODIFIER_EXILED_DIVISIONS_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("exiled_divisions_defense_factor" , ("MODIFIER_EXILED_DIVISIONS_DEFENSE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("own_exiled_divisions_attack_factor" , ("MODIFIER_OWN_EXILED_DIVISIONS_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("own_exiled_divisions_defense_factor" , ("MODIFIER_OWN_EXILED_DIVISIONS_DEFENSE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("experience_gain_factor"          , ("MODIFIER_XP_GAIN_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("fortification_collateral_chance" , ("MODIFIER_FORTIFICATION_COLLATERAL_CHANCE", MsgModifierPcPosReduced, Just 1))
        ,("max_commander_army_size"         , ("MODIFIER_ARMY_LEADER_MAX_ARMY_SIZE", MsgModifierColourPos, Just 0))
        ,("max_army_group_size"             , ("MODIFIER_ARMY_LEADER_MAX_ARMY_GROUP_SIZE", MsgModifierColourPos, Just 0))
        ,("paradrop_organization_factor"    , ("MODIFIER_PARADROP_ORGANIZATION_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("paratrooper_aa_defense"          , ("MODIFIER_PARATROOPER_DEFENSE", MsgModifierPcPosReduced, Just 1))
        ,("promote_cost_factor"             , ("MODIFIER_UNIT_LEADER_PROMOTE_COST_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("reassignment_duration_factor"    , ("MODIFIER_REASSIGNMENT_DURATION_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("sickness_chance"                 , ("MODIFIER_SICKNESS_CHANCE", MsgModifierPcNegReduced, Just 2))
        ,("skill_bonus_factor"              , ("MODIFIER_UNIT_LEADER_SKILL_BONUS_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("terrain_trait_xp_gain_factor"    , ("MODIFIER_TERRAIN_TRAIT_XP_GAIN_FACTOR", MsgModifierPcPosReduced, Just 2)) --precision 2
        ,("wounded_chance_factor"           , ("MODIFIER_WOUNDED_CHANCE_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("shore_bombardment_bonus"         , ("MODIFIER_SHORE_BOMBARDMENT", MsgModifierPcPosReduced, Just 1))

        -- Strategic region scope
        ,("air_accidents"                   , ("MODIFIER_AIR_ACCIDENTS", MsgModifierPcNegReduced, Just 1))
        ,("air_detection"                   , ("MODIFIER_AIR_DETECTION", MsgModifierPcPosReduced, Just 1))

        -- Special Projects
        ,("special_project_facility_supply_consumption_factor"  , (T.replace "$FACTOR$" "" "MODIFIER_SPECIAL_PROJECT_FACILITY_SUPPLY_CONSUMPTION_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("special_project_speed_factor"    , ("MODIFIER_SPECIAL_PROJECT_SPEED_FACTOR", MsgModifierPcPosReduced, Just 2))

        -- equipment/stats
        ,("build_cost_ic"           , ("STAT_COMMON_BUILD_COST_IC", MsgModifierPcNegReduced, Nothing))
        ,("reliability"             , ("STAT_COMMON_RELIABILITY", MsgModifierPcPosReduced, Nothing))
        ,("armor_value"             , ("STAT_COMMON_ARMOR", MsgModifierPcPosReduced, Nothing))
        ,("maximum_speed"           , ("STAT_COMMON_MAXIMUM_SPEED", MsgModifierPcPosReduced, Nothing))
        ,("fuel_consumption"        , ("STAT_COMMON_FUEL_CONSUMPTION", MsgModifierPcNegReduced, Nothing))
        ,("ap_attack"               , ("STAT_COMMON_PIERCING", MsgModifierPcPosReduced, Nothing))
        ,("max_strength"            , ("STAT_COMMON_MAX_STRENGTH", MsgModifierPcPosReduced, Nothing))

        ,("attack"                  , ("STAT_ADJUSTER_ATTACK", MsgModifierPcPosReduced, Nothing))
        ,("defense"                 , ("STAT_ADJUSTER_DEFENCE", MsgModifierPcPosReduced, Nothing))
        ,("movement"                , ("STAT_ADJUSTER_MOVEMENT", MsgModifierPcPosReduced, Nothing))

        ,("breakthrough"            , ("STAT_ARMY_BREAKTHROUGH", MsgModifierPcPosReduced, Nothing))
        ,("hardness"                , ("STAT_ARMY_HARDNESS", MsgModifierPcPosReduced, Nothing))
        ,("supply_consumption"      , ("STAT_ARMY_SUPPLY_CONSUMPTION", MsgModifierPcPosReduced, Nothing)) --precision 0
        ,("soft_attack"             , ("STAT_ARMY_SOFT_ATTACK", MsgModifierPcPosReduced, Nothing))
        ,("hard_attack"             , ("STAT_ARMY_HARD_ATTACK", MsgModifierPcPosReduced, Nothing))
        -- The narrower a division, the more of them fit into a battle, so this
        -- one is good news when it goes down.
        ,("combat_width"            , ("STAT_ARMY_COMBAT_WIDTH", MsgModifierPcNegReduced, Nothing))

        ,("air_agility"             , ("STAT_AIR_AGILITY", MsgModifierPcPosReduced, Nothing))
        ,("air_attack"              , ("STAT_AIR_ATTACK", MsgModifierPcPosReduced, Nothing))
        ,("air_range"               , ("STAT_AIR_RANGE", MsgModifierPcPosReduced, Nothing))
        ,("air_defence"             , ("STAT_AIR_DEFENCE", MsgModifierPcPosReduced, Nothing))
        ,("air_ground_attack"       , ("STAT_AIR_GROUND_ATTACK", MsgModifierPcPosReduced, Nothing))
        ,("air_bombing"             , ("STAT_AIR_BOMBING", MsgModifierPcPosReduced, Nothing))
        ,("naval_strike_attack"     , ("STAT_AIR_NAVAL_STRIKE_ATTACK", MsgModifierPcPosReduced, Nothing))

        ,("surface_detection"       , ("STAT_NAVY_SURFACE_DETECTION", MsgModifierPcPosReduced, Nothing))
        ,("sub_detection"           , ("STAT_NAVY_SUB_DETECTION", MsgModifierPcPosReduced, Nothing))
        ,("sub_visibility"          , ("STAT_NAVY_SUB_VISIBILITY", MsgModifierPcNegReduced, Nothing))
        ,("anti_air_attack"         , ("STAT_NAVY_ANTI_AIR_ATTACK", MsgModifierPcPosReduced, Nothing))
        ,("surface_visibility"      , ("STAT_NAVY_SURFACE_VISIBILITY", MsgModifierPcNegReduced, Nothing))
        ,("naval_speed"             , ("STAT_NAVY_MAXIMUM_SPEED", MsgModifierPcPosReduced, Nothing))
        ,("naval_range"             , ("STAT_NAVY_RANGE", MsgModifierPcPosReduced, Nothing))
        ,("lg_attack"               , ("STAT_NAVY_LG_ATTACK", MsgModifierPcPosReduced, Nothing))
        ,("hg_attack"               , ("STAT_NAVY_HG_ATTACK", MsgModifierPcPosReduced, Nothing))
        ,("carrier_size"            , ("STAT_CARRIER_SIZE", MsgModifierColourPos, Nothing))
        ,("torpedo_attack"          , ("STAT_NAVY_TORPEDO_ATTACK", MsgModifierPcPosReduced, Nothing))

        -- Modifiers the game documents that had no entry above. Where its
        -- localization writes the value itself, the kind of value and whether
        -- more of it is good news are read from the format it writes it with;
        -- the rest are judged from what the modifier is called. The decimal
        -- places are the ones the game documents for each.
        ,("ace_effectiveness_factor"                                           , ("MODIFIER_ACE_EFFECTIVENESS_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("air_ace_bonuses_factor"                                             , ("MODIFIER_ACE_BONUSES_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_escort_efficiency"                                              , ("MODIFIER_AIR_ESCORT_EFFICIENCY", MsgModifierPcPosReduced, Just 0))
        ,("air_home_defence_factor"                                            , ("MODIFIER_AIR_HOME_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_interception_detect_factor"                                     , ("MODIFIER_AIR_INTERCEPTION_DETECT_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("air_invasion_division_cap"                                          , ("MODIFIER_AIR_INVASION_DIVISION_CAP", MsgModifierColourPos, Just 0))
        ,("air_invasion_plan_cap"                                              , ("MODIFIER_AIR_INVASION_PLAN_CAP", MsgModifierColourPos, Just 0))
        ,("air_invasion_preparation"                                           , ("MODIFIER_AIR_INVASION_PREPARATION", MsgModifierColourPos, Just 1))
        ,("air_manpower_requirement_factor"                                    , ("MODIFIER_AIR_MANPOWER_REQUIREMENT_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("air_power_projection_factor"                                        , ("MODIFIER_AIR_POWER_PROJECTION_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("air_superiority_detect_factor"                                      , ("MODIFIER_AIR_SUPERIORITY_DETECT_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("air_untrained_pilots_penalty_factor"                                , ("MODIFIER_AIR_UNTRAINED_PILOTS_PENALTY_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("airforce_intel_decryption_bonus"                                    , ("MODIFIER_AIRFORCE_INTEL_DECRYPTION_BONUS", MsgModifierColourPos, Just 0))
        ,("annex_subject_cost_factor"                                          , ("MODIFIER_ANNEX_SUBJECT_COST_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("armor_factor"                                                       , ("MODIFIER_ARMOR", MsgModifierPcPosReduced, Just 2))
        ,("army_claim_attack_factor"                                           , ("MODIFIERS_ARMY_CLAIM_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_claim_defence_factor"                                          , ("MODIFIERS_ARMY_CLAIM_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_defence_against_major_factor"                                  , ("MODIFIERS_ARMY_DEFENCE_AGAINST_MAJOR_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_defence_against_minor_factor"                                  , ("MODIFIERS_ARMY_DEFENCE_AGAINST_MINOR_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_experience_from_volunteers"                                    , ("MODIFIER_ARMY_EXPERIENCE_FROM_VOLUNTEERS", MsgModifierPcPosReduced, Just 1))
        ,("army_fuel_capacity_factor"                                          , ("MODIFIER_ARMY_FUEL_CAPACITY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("army_intel_decryption_bonus"                                        , ("MODIFIER_ARMY_INTEL_DECRYPTION_BONUS", MsgModifierColourPos, Just 0))
        ,("army_morale"                                                        , ("MODIFIER_ARMY_MORALE", MsgModifierColourPos, Just 1))
        ,("army_non_core_attack_factor"                                        , ("MODIFIERS_ARMY_NON_CORE_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_non_core_defence_factor"                                       , ("MODIFIERS_ARMY_NON_CORE_DEFENCE_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("army_retreat_speed_factor"                                          , ("MODIFIER_ARMY_RETREAT_SPEED_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("assign_army_leader_cp_cost"                                         , ("MODIFIER_ASSIGN_ARMY_LEADER_CP_COST", MsgModifierColourNeg, Just 0))
        ,("assign_navy_leader_cp_cost"                                         , ("MODIFIER_ASSIGN_NAVY_LEADER_CP_COST", MsgModifierColourNeg, Just 0))
        ,("automatic_grant_medal_chance"                                       , ("automatic_grant_medal_chance", MsgModifierColourPos, Just 0))
        ,("autonomy_gain_ll_to_overlord_factor"                                , ("MODIFIER_AUTONOMY_GAIN_LL_TO_OVERLORD_FACTOR", MsgModifierPcReducedSign, Just 2))
        ,("autonomy_gain_ll_to_subject"                                        , ("MODIFIER_AUTONOMY_GAIN_LL_TO_SUBJECT", MsgModifierColourPos, Just 2))
        ,("autonomy_gain_ll_to_subject_factor"                                 , ("MODIFIER_AUTONOMY_GAIN_LL_TO_SUBJECT_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("autonomy_gain_trade"                                                , ("MODIFIER_AUTONOMY_GAIN_TRADE", MsgModifierColourPos, Just 2))
        ,("autonomy_gain_warscore"                                             , ("MODIFIER_AUTONOMY_GAIN_WARSCORE", MsgModifierPcPosReduced, Just 2))
        ,("autonomy_gain_warscore_factor"                                      , ("MODIFIER_AUTONOMY_GAIN_WARSCORE_FACTOR", MsgModifierPcReducedSign, Just 2))
        ,("autonomy_manpower_share_from_subjects"                              , ("MODIFIER_AUTONOMY_MANPOWER_SHARE_FROM_SUBJECTS", MsgModifierPcReducedSign, Just 2))
        ,("cannot_retreat_while_attacking"                                     , ("MODIFIER_CANNOT_RETREAT_WHILE_ATTACKING", MsgModifierColourPos, Just 0))
        ,("cannot_retreat_while_defending"                                     , ("MODIFIER_CANNOT_RETREAT_WHILE_DEFENDING", MsgModifierColourPos, Just 0))
        ,("carrier_capacity_penalty_reduction"                                 , ("MODIFIER_CARRIER_CAPACITY_PENALTY_REDUCTION", MsgModifierPcPosReduced, Just 1))
        ,("carrier_night_traffic"                                              , ("MODIFIER_CARRIER_NIGHT_TRAFFIC", MsgModifierPcPosReduced, Just 2))
        ,("carrier_sortie_hours_delay"                                         , ("MODIFIER_CARRIER_SORTIE_HOURS_DELAY", MsgModifierColourPos, Just 0))
        ,("carrier_traffic"                                                    , ("MODIFIER_CARRIER_TRAFFIC", MsgModifierColourPos, Just 0))
        ,("casualty_trickleback"                                               , ("MODIFIER_CASUALTY_TRICKLEBACK", MsgModifierPcPosReduced, Just 2))
        ,("cic_construction_boost"                                             , ("MODIFIER_CIC_CONSTRUCTION_BOOST", MsgModifierPcPosReduced, Just 1))
        ,("cic_construction_boost_factor"                                      , ("MODIFIER_CIC_CONSTRUCTION_BOOST_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("civilian_intel_decryption_bonus"                                    , ("MODIFIER_CIVILIAN_INTEL_DECRYPTION_BONUS", MsgModifierColourPos, Just 0))
        ,("combat_entrenchment"                                                , ("MODIFIER_COMBAT_ENTRENCHMENT", MsgModifierPcPosReduced, Just 1))
        ,("commando_trait_chance_factor"                                       , ("MODIFIER_COMMANDO_TRAIT_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("consumer_goods_expected_value"                                      , ("MODIFIER_CONSUMER_GOODS_EXPECTED_VALUE", MsgModifierPcNegReduced, Just 1))
        ,("crypto_department_enabled"                                          , ("MODIFIER_CRYPTO_DEPARTMENT_ENABLED", modYesNo, Just 0))
        ,("defense_impact_on_blueprint_stealing"                               , ("MODIFIER_DEFENSE_IMPACT_ON_BLUEPRINT_STEALING", MsgModifierColourPos, Just 1))
        ,("enemy_army_speed_factor"                                            , ("MODIFIER_ENEMY_ARMY_SPEED_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("enemy_attrition"                                                    , ("MODIFIER_ENEMY_ATTRITION", MsgModifierPcPosReduced, Just 1))
        ,("enemy_local_supplies"                                               , ("MODIFIER_ENEMY_LOCAL_SUPPLIES", MsgModifierPcNegReduced, Just 0))
        ,("enemy_operative_detection_chance_over_occupied_tag"                 , ("MODIFIER_ENEMY_OPERATIVE_DETECTION_CHANCE_OVER_OCCUPIED_TAG", MsgModifierColourPos, Just 2))
        ,("enemy_operative_forced_into_hiding_time_factor"                     , ("MODIFIER_ENEMY_OPERATIVE_FORCED_INTO_HIDING_TIME_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("enemy_operative_harmed_time_factor"                                 , ("MODIFIER_ENEMY_OPERATIVE_HARMED_TIME_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("enemy_operative_recruitment_chance"                                 , ("MODIFIER_ENEMY_OPERATIVE_RECRUITMENT_CHANCE", MsgModifierColourPos, Just 0))
        ,("enemy_spy_negative_status_factor"                                   , ("MODIFIER_ENEMY_SPY_NEGATIVE_STATUS_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("enemy_truck_attrition_factor"                                       , ("MODIFIER_ENEMY_TRUCK_ATTRITION_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("energy_gain_factor"                                                 , ("MODIFIER_ENERGY_GAIN_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("energy_scale_per_trade_factory_export_factor"                       , ("MODIFIER_ENERGY_SCALE_PER_TRADE_FACTORY_EXPORT_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("equipment_capture_factor_for_controller"                            , ("MODIFIER_EQUIPMENT_CAPTURE_FACTOR_FOR_CONTROLLER", MsgModifierColourPos, Just 1))
        ,("equipment_capture_for_controller"                                   , ("MODIFIER_EQUIPMENT_CAPTURE_FOR_CONTROLLER", MsgModifierColourPos, Just 1))
        ,("faction_influence_contribution_factor"                              , ("MODIFIER_FACTION_INFLUENCE_CONTRIBUTION", MsgModifierPcReducedSign, Just 2))
        ,("faction_subject_contribution_gain"                                  , ("MODIFIER_FACTION_SUBJECT_CONTRIBUTION_GAIN", MsgModifierColourPos, Just 2))
        ,("factory_energy_consumption"                                         , ("MODIFIER_FACTORY_ENERGY_CONSUMPTION", MsgModifierPcNegReduced, Just 2))
        ,("field_officer_promotion_penalty"                                    , ("MODIFIER_FIELD_OFFICER_PROMOTION_PENALTY", MsgModifierPcNegReduced, Just 0))
        ,("floating_harbor_duration"                                           , ("MODIFIER_FLOATING_HARBOR_DURATION", MsgModifierColourPos, Just 0))
        ,("floating_harbor_range"                                              , ("MODIFIER_FLOATING_HARBOR_RANGE", MsgModifierColourPos, Just 0))
        ,("floating_harbor_supply"                                             , ("MODIFIER_FLOATING_HARBOR_SUPPLY", MsgModifierColourPos, Just 0))
        ,("forced_surrender_limit"                                             , ("MODIFIER_FORCED_SURRENDER_LIMIT", MsgModifierColourNeg, Just 2))
        ,("fuel_gain_from_states"                                              , ("MODIFIER_FUEL_GAIN_FROM_STATES", MsgModifierColourPos, Just 2))
        ,("ground_attack"                                                      , ("MODIFIER_GROUND_ATTACK", MsgModifierColourPos, Just 1))
        ,("headquarters_experience_gain_factor"                                , ("headquarters_experience_gain_factor", MsgModifierPcPosReduced, Just 2))
        ,("industrial_capacity_dockyard_powered"                               , ("MODIFIER_INDUSTRIAL_CAPACITY_DOCKYARD_POWERED_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("industrial_capacity_factory_powered"                                , ("MODIFIER_INDUSTRIAL_CAPACITY_POWERED_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("intelligence_operation_speed"                                       , ("MODIFIER_INTELLIGENCE_OPERATION_SPEED", MsgModifierColourPos, Just 0))
        ,("license_anti_tank_eq_cost_factor"                                   , ("MODIFIER_LICENSE_ANTI_TANK_EQ_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("license_anti_tank_eq_production_speed_factor"                       , ("MODIFIER_LICENSE_ANTI_TANK_EQ_PRODUCTION_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("license_anti_tank_eq_tech_difference_speed_factor"                  , ("MODIFIER_LICENSE_ANTI_TANK_EQ_TECH_DIFFERENCE_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("license_artillery_eq_cost_factor"                                   , ("MODIFIER_LICENSE_ARTILLERY_EQ_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("license_artillery_eq_production_speed_factor"                       , ("MODIFIER_LICENSE_ARTILLERY_EQ_PRODUCTION_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("license_artillery_eq_tech_difference_speed_factor"                  , ("MODIFIER_LICENSE_ARTILLERY_EQ_TECH_DIFFERENCE_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("license_infantry_eq_cost_factor"                                    , ("MODIFIER_LICENSE_INFANTRY_EQ_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("license_infantry_eq_production_speed_factor"                        , ("MODIFIER_LICENSE_INFANTRY_EQ_PRODUCTION_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("license_infantry_eq_tech_difference_speed_factor"                   , ("MODIFIER_LICENSE_INFANTRY_EQ_TECH_DIFFERENCE_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("license_infantry_purchase_cost"                                     , ("MODIFIER_LICENSE_INFANTRY_PURCHASE_COST", MsgModifierColourNeg, Just 0))
        ,("license_light_tank_eq_cost_factor"                                  , ("MODIFIER_LICENSE_LIGHT_TANK_EQ_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("license_light_tank_eq_production_speed_factor"                      , ("MODIFIER_LICENSE_LIGHT_TANK_EQ_PRODUCTION_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("license_light_tank_eq_tech_difference_speed_factor"                 , ("MODIFIER_LICENSE_LIGHT_TANK_EQ_TECH_DIFFERENCE_SPEED_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("license_subject_master_purchase_cost"                               , ("MODIFIER_LICENSE_SUBJECT_MASTER_PURCHASE_COST", MsgModifierPcPosReduced, Just 2))
        ,("local_factory_energy_consumption"                                   , ("MODIFIER_LOCAL_FACTORY_ENERGY_CONSUMPTION", MsgModifierPcPosReduced, Just 0))
        ,("local_factory_energy_consumption_per_infrastructure"                , ("MODIFIER_LOCAL_FACTORY_ENERGY_CONSUMPTION_PER_INFRASTRUCTURE", MsgModifierColourNeg, Just 2))
        ,("local_resource_gain_efficiency_per_infrastructure"                  , ("MODIFIER_LOCAL_RESOURCE_GAIN_EFFICIENCY_PER_INFRASTRUCTURE", MsgModifierPcPosReduced, Just 2))
        ,("marines_special_forces_contribution_factor"                         , ("MODIFIER_MARINES_SPECIAL_FORCES_CONTRIBUTION_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("master_build_autonomy_factor"                                       , ("MODIFIER_MASTER_BUILD_AUTONOMY_FACTOR", modYesNo, Just 0))
        ,("max_organisation"                                                   , ("MODIFIER_MAX_ORGANISATION_FACTOR", MsgModifierColourPos, Just 0))
        ,("military_industrial_organization_research_bonus"                    , ("MODIFIER_MIO_RESEARCH_BONUS", MsgModifierPcPosReduced, Just 0))
        ,("mines_sweeping_by_air_factor"                                       , ("MODIFIER_MINES_SWEEPING_BY_AIR_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("modifier_army_sub_unit_armored_car_attack_factor"                   , ("modifier_army_sub_unit_armored_car_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_armored_car_defence_factor"                  , ("modifier_army_sub_unit_armored_car_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_armored_car_max_org_factor"                  , ("modifier_army_sub_unit_armored_car_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_armored_car_recon_attack_factor"             , ("modifier_army_sub_unit_armored_car_recon_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_armored_car_recon_defence_factor"            , ("modifier_army_sub_unit_armored_car_recon_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_armored_car_recon_max_org_factor"            , ("modifier_army_sub_unit_armored_car_recon_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_armored_car_recon_speed_factor"              , ("modifier_army_sub_unit_armored_car_recon_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_armored_car_speed_factor"                    , ("modifier_army_sub_unit_armored_car_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_blackshirt_assault_battalion_attack_factor"  , ("modifier_army_sub_unit_blackshirt_assault_battalion_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_blackshirt_assault_battalion_defence_factor" , ("modifier_army_sub_unit_blackshirt_assault_battalion_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_blackshirt_assault_battalion_max_org_factor" , ("modifier_army_sub_unit_blackshirt_assault_battalion_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_blackshirt_assault_battalion_speed_factor"   , ("modifier_army_sub_unit_blackshirt_assault_battalion_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_camelry_attack_factor"                       , ("modifier_army_sub_unit_camelry_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_camelry_defence_factor"                      , ("modifier_army_sub_unit_camelry_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_camelry_speed_factor"                        , ("modifier_army_sub_unit_camelry_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_category_rocket_artillery_attack_factor"     , ("modifier_army_sub_unit_category_rocket_artillery_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_category_special_forces_max_org_factor"      , ("modifier_army_sub_unit_category_special_forces_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_cavalry_attack_factor"                       , ("modifier_army_sub_unit_cavalry_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_cavalry_defence_factor"                      , ("modifier_army_sub_unit_cavalry_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_cavalry_speed_factor"                        , ("modifier_army_sub_unit_cavalry_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_infantry_attack_factor"                      , ("modifier_army_sub_unit_infantry_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_infantry_defence_factor"                     , ("modifier_army_sub_unit_infantry_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_infantry_speed_factor"                       , ("modifier_army_sub_unit_infantry_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_irregular_infantry_attack_factor"            , ("modifier_army_sub_unit_irregular_infantry_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_irregular_infantry_defence_factor"           , ("modifier_army_sub_unit_irregular_infantry_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_irregular_infantry_max_org_factor"           , ("modifier_army_sub_unit_irregular_infantry_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_irregular_infantry_speed_factor"             , ("modifier_army_sub_unit_irregular_infantry_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_light_tank_recon_attack_factor"              , ("modifier_army_sub_unit_light_tank_recon_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_light_tank_recon_defence_factor"             , ("modifier_army_sub_unit_light_tank_recon_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_light_tank_recon_max_org_factor"             , ("modifier_army_sub_unit_light_tank_recon_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_light_tank_recon_speed_factor"               , ("modifier_army_sub_unit_light_tank_recon_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_long_range_patrol_support_attack_factor"     , ("modifier_army_sub_unit_long_range_patrol_support_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_long_range_patrol_support_defence_factor"    , ("modifier_army_sub_unit_long_range_patrol_support_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_marine_attack_factor"                        , ("modifier_army_sub_unit_marine_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_marine_defence_factor"                       , ("modifier_army_sub_unit_marine_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_marine_max_org_factor"                       , ("modifier_army_sub_unit_marine_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_marine_speed_factor"                         , ("modifier_army_sub_unit_marine_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_military_police_attack_factor"               , ("modifier_army_sub_unit_military_police_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_military_police_defence_factor"              , ("modifier_army_sub_unit_military_police_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_military_police_max_org_factor"              , ("modifier_army_sub_unit_military_police_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_military_police_speed_factor"                , ("modifier_army_sub_unit_military_police_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_militia_attack_factor"                       , ("modifier_army_sub_unit_militia_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_militia_defence_factor"                      , ("modifier_army_sub_unit_militia_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_militia_max_org_factor"                      , ("modifier_army_sub_unit_militia_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_militia_org_recovery_cap_factor"             , ("modifier_army_sub_unit_militia_org_recovery_cap_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_militia_speed_factor"                        , ("modifier_army_sub_unit_militia_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_mountaineers_attack_factor"                  , ("modifier_army_sub_unit_mountaineers_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_mountaineers_defence_factor"                 , ("modifier_army_sub_unit_mountaineers_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_mountaineers_max_org_factor"                 , ("modifier_army_sub_unit_mountaineers_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_mountaineers_speed_factor"                   , ("modifier_army_sub_unit_mountaineers_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_paratrooper_attack_factor"                   , ("modifier_army_sub_unit_paratrooper_attack_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_paratrooper_defence_factor"                  , ("modifier_army_sub_unit_paratrooper_defence_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_paratrooper_max_org_factor"                  , ("modifier_army_sub_unit_paratrooper_max_org_factor", MsgModifierPcPosReduced, Just 2))
        ,("modifier_army_sub_unit_paratrooper_speed_factor"                    , ("modifier_army_sub_unit_paratrooper_speed_factor", MsgModifierPcPosReduced, Just 2))
        ,("mountaineers_special_forces_contribution_factor"                    , ("MODIFIER_MOUNTAINEERS_SPECIAL_FORCES_CONTRIBUTION_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("naval_accidents_chance"                                             , ("MODIFIER_NAVAL_ACCIDENTS_CHANCE", MsgModifierPcNegReduced, Just 2))
        ,("naval_commando_raid_distance"                                       , ("MODIFIER_NAVAL_COMMANDO_RAID_DISTANCE", MsgModifierColourPos, Just 0))
        ,("naval_enemy_positioning_in_initial_attack"                          , ("MODIFIER_NAVAL_ENEMY_POSITIONING_IN_INITIAL_ATTACK", MsgModifierColourPos, Just 0))
        ,("naval_equipment_upgrade_xp_cost"                                    , ("MODIFIER_NAVAL_EQUIPMENT_UPGRADE_XP_COST", MsgModifierPcNegReduced, Just 0))
        ,("naval_heavy_gun_hit_chance_factor"                                  , ("MODIFIER_NAVAL_HEAVY_GUN_HIT_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_invasion_division_cap"                                        , ("MODIFIER_NAVAL_INVASION_DIVISION_CAP", MsgModifierColourPos, Just 0))
        ,("naval_invasion_penalty"                                             , ("MODIFIER_NAVAL_INVASION_PENALTY", MsgModifierPcNegReduced, Just 0))
        ,("naval_invasion_plan_cap"                                            , ("MODIFIER_NAVAL_INVASION_PLAN_CAP", MsgModifierColourPos, Just 0))
        ,("naval_light_gun_hit_chance_factor"                                  , ("MODIFIER_NAVAL_LIGHT_GUN_HIT_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_mine_hit_chance"                                              , ("MODIFIER_NAVAL_MINE_HIT_CHANCE", MsgModifierPcPosReduced, Just 2))
        ,("naval_mines_damage_factor"                                          , ("MODIFIER_NAVAL_MINES_DAMAGE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("naval_mission_xp_factor"                                            , ("MODIFIER_NAVAL_MISSION_XP_FACTOR", MsgModifierPcPosReduced, Just 3))
        ,("naval_morale"                                                       , ("MODIFIER_NAVAL_MORALE", MsgModifierColourPos, Just 1))
        ,("naval_retreat_chance_after_initial_combat"                          , ("MODIFIER_NAVAL_RETREAT_CHANCE_AFTER_INITIAL_COMBAT", MsgModifierPcPosReduced, Just 0))
        ,("naval_retreat_speed_after_initial_combat"                           , ("MODIFIER_NAVAL_RETREAT_SPEED_AFTER_INITIAL_COMBAT", MsgModifierPcPosReduced, Just 0))
        ,("naval_ship_recovery_chance"                                         , ("MODIFIER_NAVAL_SHIP_RECOVERY_CHANCE", MsgModifierPcPosReduced, Just 0))
        ,("naval_ship_recovery_chance_factor"                                  , ("MODIFIER_NAVAL_SHIP_RECOVERY_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("naval_strike"                                                       , ("MODIFIER_NAVAL_STRIKE", MsgModifierPcPosReduced, Just 0))
        ,("naval_supply_consumption_factor"                                    , ("MODIFIER_NAVAL_SUPPLY_CONSUMPTION_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("naval_torpedo_damage_reduction_factor"                              , ("MODIFIER_NAVAL_TORPEDO_DAMAGE_REDUCTION_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("naval_torpedo_enemy_critical_chance_factor"                         , ("MODIFIER_NAVAL_TORPEDO_ENEMY_CRITICAL_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("navy_anti_air_attack"                                               , ("MODIFIER_NAVY_ANTI_AIR_ATTACK", MsgModifierColourPos, Just 1))
        ,("navy_casualty_on_hit"                                               , ("MODIFIER_NAVAL_CASUALTY_ON_HIT_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("navy_casualty_on_sink"                                              , ("MODIFIER_NAVAL_CASUALTY_ON_SINK_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("navy_intel_decryption_bonus"                                        , ("MODIFIER_NAVY_INTEL_DECRYPTION_BONUS", MsgModifierColourPos, Just 0))
        ,("navy_leader_cost_factor"                                            , ("MODIFIER_NAVY_LEADER_COST_FACTOR", MsgModifierPcNegReduced, Just 1))
        ,("navy_leader_start_coordination_level"                               , ("MODIFIER_NAVY_LEADER_START_COORDINATION_LEVEL", MsgModifierColourPos, Just 0))
        ,("navy_leader_start_defense_level"                                    , ("MODIFIER_NAVY_LEADER_START_DEFENSE_LEVEL", MsgModifierColourPos, Just 0))
        ,("navy_leader_start_level"                                            , ("MODIFIER_NAVY_LEADER_START_LEVEL", MsgModifierColourPos, Just 0))
        ,("navy_leader_start_maneuvering_level"                                , ("MODIFIER_NAVY_LEADER_START_MANEUVERING_LEVEL", MsgModifierColourPos, Just 0))
        ,("navy_max_range"                                                     , ("MODIFIER_NAVY_MAX_RANGE", MsgModifierColourPos, Just 1))
        ,("navy_weather_penalty"                                               , ("MODIFIER_NAVY_WEATHER_PENALTY", MsgModifierPcNegReduced, Just 2))
        ,("night_spotting_chance"                                              , ("MODIFIER_NIGHT_SPOTTING_CHANCE", MsgModifierColourPos, Just 1))
        ,("nuclear_production"                                                 , ("MODIFIER_NUCLEAR_PRODUCTION", modYesNo, Just 0))
        ,("occupied_operative_recruitment_chance"                              , ("MODIFIER_OCCUPIED_OPERATIVE_RECRUITMENT_CHANCE", MsgModifierColourPos, Just 0))
        ,("operation_cost"                                                     , ("operation_cost", MsgModifierColourNeg, Just 0))
        ,("operation_infiltrate_outcome"                                       , ("operation_infiltrate_outcome", MsgModifierPcPosReduced, Just 0))
        ,("operation_outcome"                                                  , ("operation_outcome", MsgModifierPcPosReduced, Just 0))
        ,("operative_death_on_capture_chance"                                  , ("MODIFIER_OPERATIVE_DEATH_ON_CAPTURE_CHANCE", MsgModifierColourPos, Just 0))
        ,("org_damage_multiplier"                                              , ("MODIFIER_ORG_DAMAGE_MULTIPLIER", MsgModifierPcNegReduced, Just 1))
        ,("out_of_power_impact_factor"                                         , ("MODIFIER_OUT_OF_POWER_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("own_operative_capture_chance_factor"                                , ("MODIFIER_OWN_OPERATIVE_CAPTURE_CHANCE_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("own_operative_detection_chance"                                     , ("MODIFIER_OWN_OPERATIVE_DETECTION_CHANCE", MsgModifierColourPos, Just 2))
        ,("own_operative_forced_into_hiding_time_factor"                       , ("MODIFIER_OWN_OPERATIVE_FORCED_INTO_HIDING_TIME_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("own_operative_harmed_time_factor"                                   , ("MODIFIER_OWN_OPERATIVE_HARMED_TIME_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("own_operative_intel_extraction_rate"                                , ("MODIFIER_OWN_OPERATIVE_INTEL_EXTRACTION_RATE", MsgModifierColourPos, Just 0))
        ,("paratroopers_special_forces_contribution_factor"                    , ("MODIFIER_PARATROOPERS_SPECIAL_FORCES_CONTRIBUTION_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("peace_score_ratio_transferred_to_overlord"                          , ("MODIFIER_PEACE_SCORE_RATIO_TRANSFERRED_TO_OVERLORD", MsgModifierPcPosReduced, Just 2))
        ,("peace_score_ratio_transferred_to_players"                           , ("MODIFIER_PEACE_SCORE_RATIO_TRANSFERRED_TO_PLAYERS", MsgModifierPcPosReduced, Just 2))
        ,("planning_decay_rate_factor"                                         , ("MODIFIER_PLANNING_DECAY_RATE_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("port_strike"                                                        , ("MODIFIER_PORT_STRIKE_ATTACK_FACTOR", MsgModifierPcPosReduced, Just 1))
        ,("production_speed_buildings_powered_factor"                          , ("MODIFIER_PRODUCTION_SPEED_BUILDINGS_POWERED_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("production_speed_facility_factor"                                   , ("MODIFIER_PRODUCTION_SPEED_FACILITY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("railway_gun_bombardment_factor"                                     , ("MODIFIER_RAILWAY_GUN_BOMBARDMENT_FACTOR", MsgModifierPcPosReduced, Just 0))
        ,("rangers_special_forces_contribution_factor"                         , ("MODIFIER_RANGERS_SPECIAL_FORCES_CONTRIBUTION_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("recruitable_population"                                             , ("MODIFIER_RECRUITABLE_POPULATION", MsgModifierPcPosReduced, Just 3))
        ,("resources_to_overlord_factor"                                       , ("MODIFIER_RESOURCES_TO_OVERLORD_FACTOR", MsgModifierPcReducedSign, Just 2))
        ,("scientist_breakthrough_bonus_factor"                                , ("MODIFIER_SCIENTIST_BREAKTHROUGH_BONUS_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("scientist_research_bonus_factor"                                    , ("MODIFIER_SCIENTIST_RESEARCH_BONUS_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("scientist_xp_gain_factor"                                           , ("MODIFIER_SCIENTIST_XP_GAIN_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("shore_bombardment_collateral_damage_factor"                         , ("MODIFIER_SHORE_BOMBARDMENT_COLLATERAL_DAMAGE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("special_forces_doctrine_cost_factor"                                , ("MODIFIER_SPECIAL_FORCES_DOCTRINE_COST_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("special_project_facility_supply_consumption_factor"                 , ("MODIFIER_SPECIAL_PROJECT_FACILITY_SUPPLY_CONSUMPTION_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("state_production_speed_facility_factor"                             , ("MODIFIER_STATE_PRODUCTION_SPEED_FACILITY_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("str_damage_multiplier"                                              , ("MODIFIER_STR_DAMAGE_MULTIPLIER", MsgModifierPcNegReduced, Just 1))
        ,("submarine_attack"                                                   , ("MODIFIER_SUBMARINE_ATTACK", MsgModifierColourPos, Just 1))
        ,("tech_air_damage_factor"                                             , ("MODIFIER_TECH_AIR_DAMAGE_FACTOR", MsgModifierPcPosReduced, Just 2))
        ,("thermonuclear_production"                                           , ("MODIFIER_THERMONUCLEAR_PRODUCTION", modYesNo, Just 0))
        ,("thermonuclear_production_factor"                                    , ("MODIFIER_THERMONUCLEAR_PRODUCTION_FACTOR", MsgModifierColourPos, Just 0))
        ,("training_time_army"                                                 , ("MODIFIER_TRAINING_TIME_ARMY", MsgModifierColourPos, Just 1))
        ,("transport_capacity"                                                 , ("MODIFIER_TRANSPORT_CAPACITY", MsgModifierPcPosReduced, Just 0))
        ,("truck_attrition"                                                    , ("MODIFIER_TRUCK_ATTRITION", MsgModifierColourNeg, Just 2))
        ,("underway_replenishment_convoy_cost"                                 , ("MODIFIER_UNDERWAY_REPLENISHMENT_CONVOY_COST", MsgModifierPcNegReduced, Just 2))
        ,("underway_replenishment_range"                                       , ("MODIFIER_UNDERWAY_REPLENISHMENT_RANGE", MsgModifierPcPosReduced, Just 2))
        ,("unit_medal_effectiveness"                                           , ("unit_medal_effectiveness", MsgModifierPcPosReduced, Just 0))
        ,("unit_upkeep_attrition_factor"                                       , ("MODIFIER_UNIT_UPKEEP_ATTRITION_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("war_support_reduction_on_damage"                                    , ("MODIFIER_WAR_SUPPORT_REDUCTION_ON_DAMAGE", MsgModifierPcNegReduced, Just 1))
        ,("winter_attrition"                                                   , ("MODIFIER_WINTER_ATTRITION", MsgModifierPcNegReduced, Just 1))

        -- Modifiers the documentation lists whose localization key the game
        -- does not spell the way it spells the modifier: a targeted one is named
        -- "… against a country", an industrial organization shortens to MIO, and
        -- a few are named for something else entirely. Run down by hand against
        -- the game.
        ,("additional_brigade_column_size"                                       , ("MODIFIER_BRIGADE_SIZE", MsgModifierColourPos, Just 0))
        ,("air_invasion_prep_days"                                               , ("MODIFIER_AIR_INVASION_PREPARATION_DAYS", MsgModifierColourNeg, Just 1))
        ,("amphibious_invasion_against"                                          , ("MODIFIER_AMPHIBIOUS_INVASION_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 1))
        ,("can_guarantee_other_ideologies"                                       , ("MODIFIER_GUARANTEE_OTHER_IDEOLOGIES", modYesNo, Just 0))
        ,("choose_preferred_tactics_cost"                                        , ("MODIFIER_CHOOSE_PREFERRED_TACTIC_COST", MsgModifierColourNeg, Just 0))
        ,("disable_strategic_redeployment_for_controller"                        , ("MODIFIER_STRATEGIC_REDEPLOYMENT_DISABLED_FOR_CONTROLLER", modNoYes, Just 0))
        ,("experience_gain_army_unit"                                            , ("MODIFIER_XP_GAIN_ARMY_UNIT", MsgModifierColourPos, Just 1))
        ,("experience_gain_navy_unit"                                            , ("MODIFIER_XP_GAIN_NAVY_UNIT", MsgModifierColourPos, Just 1))
        ,("female_random_admiral_chance"                                         , ("MODIFIER_FEMALE_ADMIRAL_CHANCE", MsgModifierSign, Just 0))
        ,("female_random_country_leader_chance"                                  , ("MODIFIER_FEMALE_COUNTRY_LEADER_CHANCE", MsgModifierSign, Just 0))
        ,("female_random_operative_chance"                                       , ("MODIFIER_FEMALE_OPERATIVE_CHANCE", MsgModifierSign, Just 0))
        ,("female_random_scientist_chance"                                       , ("MODIFIER_FEMALE_SCIENTIST_CHANCE", MsgModifierSign, Just 0))
        ,("fortification_damage"                                                 , ("MODIFIER_FORTIFICATION_COLLATERAL_DAMAGE", MsgModifierPcPosReduced, Just 1))
        ,("invasion_preparation_against"                                         , ("MODIFIER_NAVAL_INVASION_PREPARATION_AGAINST_A_COUNTRY", MsgModifierColourNeg, Just 1))
        ,("lend_lease_tension_with_overlord"                                     , ("MODIFIER_LEND_LEASE_TENSION_LIMIT_WITH_OVERLORD", MsgModifierPcNegReduced, Just 1))
        ,("max_fuel_building"                                                    , ("MODIFIER_MAX_FUEL_ADD", MsgModifierColourPos, Just 2))
        ,("military_industrial_organization_design_team_assign_cost"             , ("MODIFIER_MIO_DESIGN_TEAM_ASSIGN_COST", MsgModifierPcNegReduced, Just 0))
        ,("military_industrial_organization_design_team_change_cost"             , ("MODIFIER_MIO_DESIGN_TEAM_CHANGE_COST", MsgModifierPcNegReduced, Just 0))
        ,("military_industrial_organization_industrial_manufacturer_assign_cost" , ("MODIFIER_MIO_INDUSTRIAL_MANUFACTURER_ASSIGN_COST", MsgModifierPcNegReduced, Just 0))
        ,("military_industrial_organization_policy_cooldown"                     , ("MODIFIER_MIO_POLICY_COOLDOWN_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("military_industrial_organization_policy_cost"                         , ("MODIFIER_MIO_POLICY_COST_FACTOR", MsgModifierPcNegReduced, Just 0))
        ,("military_industrial_organization_size_up_requirement"                 , ("MODIFIER_MIO_FUNDS_SIZE_UP_REQUIREMENT", MsgModifierPcNegReduced, Just 0))
        ,("military_industrial_organization_task_capacity"                        , ("MODIFIER_MIO_TASK_CAPACITY", MsgModifierColourPos, Just 0))
        ,("naval_critical_score_chance_factor_against"                           , ("MODIFIER_NAVAL_CRITICAL_SCORE_CHANCE_FACTOR_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 2))
        ,("naval_hit_chance_against"                                             , ("MODIFIER_NAVAL_HIT_CHANCE_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 0))
        ,("naval_invasion_planning_bonus_speed"                                  , ("MODIFIER_NAVAL_INVASION_PLANNING_SPEED", MsgModifierPcPosReduced, Just 0))
        ,("naval_invasion_prep_days"                                             , ("MODIFIER_NAVAL_INVASION_PREPARATION_DAYS", MsgModifierColourNeg, Just 0))
        ,("navy_capital_ship_attack_factor_against"                              , ("MODIFIER_NAVY_CAPITAL_SHIP_ATTACK_FACTOR_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 2))
        ,("navy_capital_ship_defence_factor_against"                             , ("MODIFIER_NAVY_CAPITAL_SHIP_DEFENCE_FACTOR_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 2))
        ,("navy_screen_attack_factor_against"                                    , ("MODIFIER_NAVY_SCREEN_ATTACK_FACTOR_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 2))
        ,("navy_screen_defence_factor_against"                                   , ("MODIFIER_NAVY_SCREEN_DEFENCE_FACTOR_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 2))
        ,("paratrooper_weight_factor"                                            , ("MODIFIER_UNIT_SIZE_FACTOR_FOR_PARADROP", MsgModifierPcNegReduced, Just 1))
        ,("river_crossing_factor"                                                , ("MODIFIER_RIVER_CROSSING_PENALTY_FACTOR", MsgModifierPcNegReduced, Just 2))
        ,("river_crossing_factor_against"                                        , ("MODIFIER_RIVER_CROSSING_PENALTY_FACTOR_AGAINST_A_COUNTRY", MsgModifierPcNegReduced, Just 2))
        ,("spotting_chance_against"                                              , ("MODIFIER_SPOTTING_CHANCE_AGAINST_A_COUNTRY", MsgModifierPcPosReduced, Just 0))
        ]

-------------------------------------------------
-- Handler for add_dynamic_modifier --
-------------------------------------------------
data AddDynamicModifier = AddDynamicModifier
    { adm_modifier :: Text
    , adm_scope :: Either Text (Text, Text)
    , adm_days :: Maybe Double
    }
newADM :: AddDynamicModifier
newADM = AddDynamicModifier undefined (Left "THIS") Nothing
addDynamicModifier :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addDynamicModifier stmt@[pdx| %_ = @scr |] =
    pp_adm (foldl' addLine newADM scr)
    where
        addLine adm [pdx| modifier = $mod |] = adm { adm_modifier = mod }
        addLine adm [pdx| scope = $tag |] = adm { adm_scope = Left tag }
        addLine adm [pdx| scope = $vartag:$var |] = adm { adm_scope = Right (vartag, var) }
        addLine adm [pdx| days = !amt |] = adm { adm_days = Just amt }
        addLine adm stmt = trace ("Unknown in add_dynamic_modifier: " ++ show stmt) adm
        pp_adm adm = do
            let days = maybe "" formatDays (adm_days adm)
            mmod <- HM.lookup (adm_modifier adm) <$> getDynamicModifiers
            thescope <- getCurrentScope
            dynflag <- case thescope of
                Just HOI4Country -> eflag (Just HOI4Country) $ adm_scope adm
                Just HOI4ScopeState -> eGetState $ adm_scope adm
                Just HOI4From -> return $ Just "FROM"
                _ -> return $ Just "<!-- check script -->"
            let dynflagd = fromMaybe "<!-- check script -->" dynflag
            case mmod of
                Just mod -> withCurrentIndent $ \i -> do
                    effect <- fold <$> indentUp (traverse (modifierMSG False "") (dmodEffects mod))
                    trigger <- indentUp $ ppMany (dmodEnable mod)
                    let name = dmodLocName mod
                        locName = maybe ("<tt>" <> adm_modifier adm <> "</tt>") (Doc.doc2text . iquotes) name
                    return $ ((i, MsgAddDynamicModifier locName dynflagd days) : effect) ++ (if null trigger then [] else (i+1, MsgLimit) : trigger)
                Nothing -> trace ("add_dynamic_modifier: Modifier " ++ T.unpack (adm_modifier adm) ++ " not found") $ preStatement stmt
addDynamicModifier stmt = trace ("Not handled in addDynamicModifier: " ++ show stmt) $ preStatement stmt

removeDynamicModifier :: (HOI4Info g, Monad m) => StatementHandler g m
removeDynamicModifier stmt@[pdx| %_ = $txt |] = withLocAtom MsgRemoveDynamicMod stmt
removeDynamicModifier stmt@[pdx| %_ = @dyn |] = do
    case dyn of
        [stmtd@[pdx| %_ = $txt |]] ->  withLocAtom MsgRemoveDynamicMod stmtd
        _-> preStatement stmt
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

-- | Split a script into the chunks that are shown as a whole.
chunkScript :: (HOI4Info g, Monad m) => GenericScript -> PPT g m [ScriptChunk]
chunkScript scr = chunkIdeaSlots <$> chunkDynModVars scr

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
    curind <- getCurrentIndent
    let curindent = fromMaybe 1 curind
        name = fromMaybe (dmodName dmod) (dmodLocName dmod)
    headmsg <- msgToPP $ if isSet then MsgSetDynamicModifier else MsgModifyDynamicModifier
    modicon <- maybe (return "") (getGameInterface "idea_unknown") (dmodIcon dmod)
    modmsg <- withCurrentIndentCustom 1 $ \_ ->
        fold <$> traverse (modifierMSG False "" . modStmt) mods
    return $ headmsg
        ++ ((0, MsgEffectBox name (dmodName dmod) modicon "") : modmsg)
        ++ [(0, MsgEffectBoxEnd curindent)]
    where
        modStmt (key, val) = Statement (GenericLhs key []) OpEq (FloatRhs val)

flagTextMaybe :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
flagTextMaybe txt = eflag (Just HOI4Country) (Left txt)

hasDynamicModifier :: (HOI4Info g, Monad m) => StatementHandler g m
hasDynamicModifier stmt@[pdx| %_ = @dyn |] = if length dyn == 2
    then textAtom "scope" "modifier" MsgHasDynamicModFlag flagTextMaybe stmt
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
                let idpob_loc = fromMaybe ("<tt>" <> idpob <> "</tt>") midpob_loc
                case mmod of
                    Just mod -> withCurrentIndent $ \i -> do
                        effect <- fold <$> indentUp (traverse (modifierMSG False "") (modEffects mod))
                        let name = modLocName mod
                            locName = maybe ("<tt>" <> modi <> "</tt>") (Doc.doc2text . iquotes) name
                        return ((i, MsgAddPowerBalanceModifier idpob_loc idpob locName modi) : effect)
                    _ -> trace ("add_power_balance_modifier: Modifier " ++ T.unpack modi ++ " not found") $ preStatement stmt
            _-> preStatement stmt
addPowerBalanceModifier stmt = trace ("Not handled in addPowerBalanceModifier: " ++ show stmt) $ preStatement stmt


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
withCharacter msg stmt@[pdx| %_ = ?txt |] = do
    chaname <- getCharacterName txt
    msgToPP $ msg chaname
withCharacter _ stmt = preStatement stmt

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
        [pdx| %_ = $ideotype|] -> do
            subideos <- getIdeology
            case HM.lookup ideotype subideos of
                Just ideo -> getGameL10n ideo
                _-> return "<!-- Check Script -->"
        _->return "<!-- Check Script -->") ideo
    return (ideoloc, traitmsg)
parseLeader stmt = return ("<!-- Check Script -->", [])


createLeader :: (Monad m, HOI4Info g) => StatementHandler g m
createLeader stmt@[pdx| %_ = @scr |] = do
        let (name, rest) = extractStmt (matchLhsText "name") scr
            (ideo, rest') = extractStmt (matchLhsText "ideology") rest
            (traits, _) = extractStmt (matchLhsText "traits") rest'
        nameloc <- case name of
            Just [pdx| %_ = ?id |] -> getCharacterName id
            _ -> return ""
        traitmsg <- case traits of
            Just [pdx| %_ = @arr |] -> do
                let traitbare = mapMaybe getbaretraits arr
                concatMapM ppHt traitbare
            _-> return []
        ideoloc <- maybe (return "") (\case
            [pdx| %_ = $ideotype|] -> do
                subideos <- getIdeology
                case HM.lookup ideotype subideos of
                    Just ideo -> getGameL10n ideo
                    _-> return "<!-- Check Script -->"
            _-> return "<!-- Check Script -->") ideo
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
    | txt == "yes" = msgToPP $ MsgPromoteCharacter ""
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
    subideos <- getIdeology
    ideoloc <- maybe (return "") getGameL10n (HM.lookup atom subideos)
    case HM.lookup what chas of
        Just ccha -> do
            let nameloc = cha_loc_name ccha
                ideolocd = if T.null ideoloc
                    then fromMaybe "" (cha_leader_ideology ccha)
                    else ideoloc
            traitmsg <- case cha_leader_traits ccha of
                Just trts -> do
                    concatMapM ppHt trts
                _-> return []
            basemsg <- if not (T.null ideoloc)
                then msgToPP $ MsgAddCountryLeaderRolePromoted nameloc ideolocd
                else msgToPP $ MsgPromoteCharacter nameloc
            return $ basemsg ++ traitmsg
        _-> if not (T.null what)
            then preStatement stmt
            else msgToPP $ MsgAddCountryLeaderRolePromoted "" ideoloc

ppHt :: (Monad m, HOI4Info g) => Text -> PPT g m IndentedMessages
ppHt trait = do
    traitloc <- Doc.oneLine <$> getGameL10n trait
    namemsg <- indentUp $ plainMsg' ("'''" <> traitloc <> "'''")
    traitmsg' <- indentUp $ indentUp $ getLeaderTraits trait
    return $ namemsg : traitmsg'

getbaretraits :: GenericStatement -> Maybe Text
getbaretraits (StatementBare (GenericLhs trait [])) = Just trait
getbaretraits stmt = Nothing

getCharacterName :: (Monad m, HOI4Info g) =>
    Text -> PPT g m Text
getCharacterName idn = do
    characters <- getCharacters
    case HM.lookup idn characters of
        Just charid -> return $ cha_loc_name charid
        _ -> getGameL10n idn

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
            traitloc <- getGameL10n $ ht_trait ht
            namemsg <- indentUp $ plainMsg' ("'''" <> traitloc <> "'''")
            traitmsg' <- indentUp $ indentUp $ getLeaderTraits (ht_trait ht)
            let traitmsg = namemsg : traitmsg'
            case (ht_character ht, ht_ideology ht) of
                (Just char, Just ideo) -> do
                    charloc <- getCharacterName char
                    ideoloc <- getGameL10n ideo
                    baseMsg <- msgToPP $ MsgTraitCharIdeo charloc addremove ideoloc
                    return $ baseMsg ++ traitmsg
                (Just char, _) -> do
                    charloc <- getCharacterName char
                    baseMsg <- msgToPP $ MsgTraitChar charloc addremove
                    return $ baseMsg ++ traitmsg
                (_, Just ideo) -> do
                    ideoloc <- getGameL10n ideo
                    baseMsg <- msgToPP $ MsgTraitIdeo addremove ideoloc
                    return $ baseMsg ++ traitmsg
                _ -> do
                    baseMsg <- msgToPP $ MsgTrait addremove
                    return $ baseMsg ++ traitmsg
handleTrait _ stmt = preStatement stmt

addRemoveLeaderTrait :: (Monad m, HOI4Info g) => ScriptMessage -> StatementHandler g m
addRemoveLeaderTrait msg stmt@[pdx| %_ = $trait |] = do
    traitloc <- getGameL10n trait
    namemsg <- indentUp $ plainMsg' ("'''" <> traitloc <> "'''")
    traitmsg' <- indentUp $ indentUp $ getLeaderTraits trait
    let traitmsg = namemsg : traitmsg'
    baseMsg <- msgToPP msg
    return $ baseMsg ++ traitmsg
addRemoveLeaderTrait _ stmt = preStatement stmt

addRemoveUnitTrait :: (Monad m, HOI4Info g) => ScriptMessage -> StatementHandler g m
addRemoveUnitTrait msg stmt@[pdx| %_ = $trait |] = do
    traitloc <- getGameL10n trait
    namemsg <- indentUp $ plainMsg' ("'''" <> traitloc <> "'''")
    traitmsg' <- indentUp $ indentUp $ getUnitTraits trait
    let traitmsg = namemsg : traitmsg'
    baseMsg <- msgToPP msg
    return $ baseMsg ++ traitmsg
addRemoveUnitTrait _ stmt = preStatement stmt

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
customEffectTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
customEffectTooltip = tooltipWith MsgCustomEffectTooltip

-- | Handler for a @tooltip@, the sentence written in place of whatever conditions
-- or effects it stands next to. Written the same two ways as a custom effect
-- tooltip.
tooltip :: (HOI4Info g, Monad m) => StatementHandler g m
tooltip = tooltipWith MsgTooltip

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

-- | Handler for @custom_override_tooltip@, which runs the effects inside it but
-- shows only its own sentence in place of theirs. What the game hides here is
-- exactly what the wiki is for, so the sentence heads the block and the effects
-- are written out under it.
customOverrideTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
customOverrideTooltip stmt@[pdx| %_ = @scr |] = do
    let (mtooltip, rest) = extractStmt (matchLhsText "tooltip") scr
    ttmsg <- maybe (return []) tooltip mtooltip
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
    Just token -> fromMaybe ("<tt>" <> key <> "</tt>") <$> locFormatter fmt token
    Nothing -> fillConstants =<< getGameL10nArgs args key
    where (fmt, rest) = T.breakOn "|" key

-- | Fill in every script constant a piece of localization refers to in brackets.
-- The game reads those as it draws the text, and unlike the variables that share
-- the same bracket syntax, a constant is the same number whenever it is read, so
-- it can be filled in here as well. A reference to something that is not a
-- constant holding a number is left as it stands for a human to deal with.
fillConstants :: (HOI4Info g, Monad m) => Text -> PPT g m Text
fillConstants text = do
    constants <- getScriptConstants
    return (fill constants text)
    where
        marker = "[?constant:"
        fill constants t = case T.breakOn marker t of
            (_, rest) | T.null rest -> t
            (before, rest) ->
                let afterMarker = T.drop (T.length marker) rest
                    (path, closing) = T.breakOn "]" afterMarker
                in case (HM.lookup path constants, T.stripPrefix "]" closing) of
                    (Just val, Just after) ->
                        before <> Doc.doc2text (plainNum val) <> fill constants after
                    -- Whatever this refers to, the marker itself is done with, so
                    -- what follows is searched without it and the recursion ends.
                    _ -> before <> marker <> fill constants afterMarker

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
            _ -> return ("<tt>" <> unquoted <> "</tt>")
    | otherwise = locKeyText HM.empty unquoted
    where unquoted = T.dropAround (== '"') val

-- | Handler for @show_unit_leaders_tooltip@, which names a commander in a
-- tooltip. Script uses it to show what a @hidden_effect@ next to it just did, so
-- the commander's name is all there is to say.
showUnitLeader :: (HOI4Info g, Monad m) => StatementHandler g m
showUnitLeader [pdx| %_ = ?token |] = do
    nameloc <- getCharacterName token
    msgToPP (MsgShowUnitLeader (Doc.oneLine nameloc) token)
showUnitLeader stmt = preStatement stmt

-- | Handlers for the tooltips that name a military industrial organization, one
-- of the department traits it can take on, or one of the policies it can follow.
showMio :: (HOI4Info g, Monad m) => StatementHandler g m
showMio = mioTooltip MsgShowMio

unlockMio :: (HOI4Info g, Monad m) => StatementHandler g m
unlockMio = mioTooltip MsgUnlockMio

unlockMioTrait :: (HOI4Info g, Monad m) => StatementHandler g m
unlockMioTrait = mioTooltip MsgUnlockMioTrait

unlockMioPolicy :: (HOI4Info g, Monad m) => StatementHandler g m
unlockMioPolicy = mioTooltip MsgUnlockMioPolicy

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
