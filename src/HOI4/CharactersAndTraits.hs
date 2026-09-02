{-
Module      : HOI4.CharactersAndTraits
Description : Feature handler for characetr and trait features in Hearts of Iron IV
-}
module HOI4.CharactersAndTraits (
         parseHOI4Characters
        ,parseHOI4CountryLeaderTraits
        ,parseHOI4UnitLeaderTraits
    ) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (..))

import Data.List ( foldl' )
import Data.Maybe (fromMaybe, mapMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
 -- everything
import QQ (pdx)
import SettingsTypes ( PPT
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , withCurrentFile
                     , hoistErrors)
import ParseWarnings
import HOI4.Common -- everything
import StatementUtils -- everything
import HOI4.Localization
import HOI4.Messages (wikifyLocColours)
----------------
-- Characters --
----------------

newHOI4Character :: Text -> Text -> FilePath -> HOI4Character
newHOI4Character chatag locname = HOI4Character chatag chatag locname Nothing Nothing Nothing Nothing []

-- | Note a military post the character is written for. A character whose entry
-- differs between DLCs is written out once per DLC, each with the same posts,
-- so a post already noted is not noted again.
addUnitRole :: Text -> HOI4Character -> HOI4Character
addUnitRole role hChar
    | role `elem` cha_unit_roles hChar = hChar
    | otherwise = hChar { cha_unit_roles = cha_unit_roles hChar ++ [role] }

parseHOI4Characters :: (HOI4Info g, IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Character, HashMap Text HOI4Advisor)
parseHOI4Characters scripts = do
    charmap <- keyedBy cha_id <$>
        parseScriptFiles "characters"
            (\scr -> mapM character $ concatMap (\case
                [pdx| characters = @chars |] -> chars
                _ -> scr) scr)
            scripts
    chartokmap <- parseCharToken charmap
    return (charmap, chartokmap)
    where
        parseCharToken :: (HOI4Info g, IsGameData (GameData g), Monad m) =>
            HashMap Text HOI4Character ->  PPT g m (HashMap Text HOI4Advisor)
        parseCharToken chas = do
            let chaselem = HM.elems chas
                chastoken = mapMaybe (\c -> case cha_advisor c of
                    Just adv ->
                        Just $ map (\a ->
                            (adv_idea_token a, advisorNamed a c)) adv
                    _ -> Nothing)
                    chaselem
            return $ HM.fromList $ concat chastoken
        advisorNamed a c = a { adv_cha_name = cha_name c,adv_cha_id = cha_id c, adv_cha_portrait = cha_portrait c}

character :: (HOI4Info g, IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4Character))
character = onTopLevelCompound "character" $ \id parts ->
    withCurrentFile $ \file -> do
        locname <- wikifyLocColours <$> getGameL10n id
        hoistErrors $ foldM characterAddSection
                            (Just (newHOI4Character id locname file))
                            parts

characterAddSection :: (HOI4Info g, MonadError Text m) =>
    Maybe HOI4Character -> GenericStatement -> PPT g m (Maybe HOI4Character)
characterAddSection Nothing _ = return Nothing
characterAddSection hChar stmt
    = sequence (characterAddSection' <$> hChar <*> pure stmt)
    where
        characterAddSection' hChar stmt@[pdx| name = %name |] = case name of
            StringRhs name -> do
                nameLoc <- wikifyLocColours <$> getGameL10n name
                return hChar {cha_loc_name = nameLoc, cha_name = name}
            GenericRhs name [] -> do
                nameLoc <- wikifyLocColours <$> getGameL10n name
                return hChar {cha_loc_name  = nameLoc, cha_name = name}
            _ -> warn (BadValue "character name" stmt) $ return hChar
        characterAddSection' hChar [pdx| advisor = @adv |] = do
            let oldadv = fromMaybe [] (cha_advisor hChar )
            advif <- getAdvinfo hChar adv
            return hChar { cha_advisor = Just  (oldadv ++ [advif])}
        characterAddSection' hChar [pdx| country_leader = @clead |] =
            let cleadtraits = concatMap getTraits clead
                ideo = getLeaderIdeo clead in
            return hChar {cha_leader_traits = Just cleadtraits, cha_leader_ideology = ideo}
        characterAddSection' hChar [pdx| portraits = @scr |] =
            let port = getSmallPortrait scr in
            return hChar { cha_portrait = port }
        characterAddSection' hChar [pdx| corps_commander = %_ |] =
            return (addUnitRole "corps_commander" hChar)
        characterAddSection' hChar [pdx| field_marshal = %_ |] =
            return (addUnitRole "field_marshal" hChar)
        characterAddSection' hChar [pdx| navy_leader = %_ |] =
            return (addUnitRole "navy_leader" hChar)
        characterAddSection' hChar [pdx| scientist = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| gender = %_ |] =
            return hChar
        -- A character whose entry differs between DLCs is written as several
        -- instances, each holding the sections the character would otherwise
        -- hold directly. Which instance is in play depends on what the game is
        -- running with, so all of them are read: an advisor post that only one
        -- of them offers is still a post the character can hold, and script that
        -- points at its idea token has to find it.
        characterAddSection' hChar [pdx| instance = @scr |] =
            foldM instanceSection hChar scr
        characterAddSection' hChar [pdx| allowed_civil_war = %_ |] =
            return hChar
        -- What decides which of a character's instances is the one in play. All
        -- of them are read, so there is nothing to decide.
        characterAddSection' hChar [pdx| allowed = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| can_be_captured = %_ |] =
            return hChar
        characterAddSection' hChar stmt
            = warn (UnknownSection "character" stmt) $ return hChar

        -- Instances disagree about the character's name, since that is often the
        -- whole reason for having more than one of them, so the name the
        -- character is localized under outside them wins. Only where there is no
        -- such name does an instance get to supply one.
        instanceSection hChar stmt@[pdx| name = %_ |]
            | cha_loc_name hChar /= cha_id hChar = return hChar
            | otherwise = characterAddSection' hChar stmt
        instanceSection hChar stmt = characterAddSection' hChar stmt

        getTraits stmt@[pdx| traits = @traits |] = map
            (\case
                StatementBare (GenericLhs trait []) -> trait
                _ -> warn (BadValue "character traits" stmt) "")
            traits
        getTraits _ = []
        getLeaderIdeo stmt = do
            let (ideo, _) = extractStmt (matchLhsText "ideology") stmt
            case ideo of
                Just [pdx| %_ = $ideot |] -> Just ideot
                _ -> Nothing

--getSmallPortrait :: GenericStatement -> HOI4Character -> HOI4Character
getSmallPortrait scr = getPortrait scr
    where
        getPortrait [] = Nothing
        getPortrait ([pdx| $_ = @scr |]:can) = getPortrait' scr can
        getPortrait (x:can) = getPortrait can
        getPortrait' x can = getSmall x can
        getSmall [] can = getPortrait can
        getSmall (x:xs) can = getSmall' x xs can
        getSmall' [pdx| small = $port |] _ _ = Just port
        getSmall' [pdx| small = ?port |] _ _ = Just port
        getSmall' _ x can = getSmall x can


traitFromArray :: GenericStatement -> Maybe Text
traitFromArray (StatementBare (GenericLhs e [])) = Just e
traitFromArray stmt = warn (UnknownSection "character trait array" stmt) Nothing


newAI :: HOI4Advisor
newAI = HOI4Advisor "" "" "" "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing True ""
getAdvinfo :: forall g m. (HOI4Info g, Monad m) => HOI4Character ->
    [GenericStatement] -> PPT g m HOI4Advisor
getAdvinfo cha = foldM addLine newAI
    where
        addLine :: HOI4Advisor -> GenericStatement -> PPT g m HOI4Advisor
        addLine ai [pdx| slot = $txt |] = return ai { adv_advisor_slot = txt }
        addLine ai [pdx| idea_token = $txt |] = return ai { adv_idea_token = txt}
        addLine ai [pdx| on_add = %rhs |] = case rhs of
                CompoundRhs [] -> return ai
                CompoundRhs scr -> return ai { adv_on_add = Just scr }
                _-> return ai
        addLine ai [pdx| on_remove = %rhs |] = case rhs of
                CompoundRhs [] -> return ai
                CompoundRhs scr -> return ai { adv_on_remove = Just scr }
                _-> return ai
        addLine ai [pdx| traits = @rhs |] = do
            let traits = Just (mapMaybe traitFromArray rhs)
            return ai {adv_traits = traits}
        addLine ai stmt@[pdx| modifier = %rhs |] = case rhs of
                CompoundRhs [] -> return ai
                CompoundRhs _ -> return ai { adv_modifier = Just stmt }
                _-> return ai
        addLine ai stmt@[pdx| research_bonus = %rhs |] = case rhs of
                CompoundRhs [] -> return ai
                CompoundRhs _ -> return ai { adv_research_bonus = Just stmt }
                _-> return ai
        addLine ai [pdx| allowed = %rhs |] = case rhs of
                CompoundRhs [] -> return ai
                CompoundRhs scr -> return ai { adv_allowed = Just scr }
                _-> return ai
        addLine ai [pdx| available = %rhs|] = case rhs of
                CompoundRhs [] -> return ai
                CompoundRhs scr -> return ai { adv_available = Just scr }
                _-> return ai
        addLine ai [pdx| visible = %rhs|] = case rhs of
                CompoundRhs [] -> return ai
                CompoundRhs scr -> return ai { adv_visible = Just scr }
                _-> return ai
        addLine ai [pdx| ledger = %_|] = return ai
        addLine ai stmt@[pdx| cost = %rhs|] = case rhs of
                (floatRhs -> Just num) -> return ai { adv_cost = Just num }
                _-> return $ warn (BadValue "advisor cost" stmt) ai
        addLine ai [pdx| ai_will_do = %_|] = return ai
        addLine ai [pdx| removal_cost = %_|] = return ai
        addLine ai [pdx| do_effect = %_|] = return ai
        addLine ai [pdx| desc = %_|] = return ai
        addLine ai [pdx| picture = %_|] = return ai
        addLine ai [pdx| name = %_|] = return ai
        -- Whether the advisor's effects always show on the actions tooltip;
        -- display only, nothing for the page.
        addLine ai [pdx| always_show_on_actions_tooltip = %_ |] = return ai
        addLine ai [pdx| can_be_fired = %rhs|]
            | GenericRhs "no" [] <- rhs = return ai { adv_can_be_fired = True }
            | GenericRhs "yes" [] <- rhs = return ai { adv_can_be_fired = False }
        addLine ai stmt = warn (UnknownSection "advisor info" stmt) $ return ai

parseHOI4CountryLeaderTraits :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4CountryLeaderTrait)
parseHOI4CountryLeaderTraits scripts = keyedBy clt_id <$>
    parseScriptFiles "country leader traits"
        (mapM parseHOI4CountryLeaderTrait . concatMap (\case
            [pdx| leader_traits = @traits |] -> traits
            _ -> []))
        scripts

parseHOI4CountryLeaderTrait :: (HOI4Info g, MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4CountryLeaderTrait))
parseHOI4CountryLeaderTrait [pdx| $id = @effects |]
    = withCurrentFile $ \file -> do
        mlocid <- fmap wikifyLocColours <$> getGameL10nIfPresent id
        let cclt = foldl' addSection (HOI4CountryLeaderTrait {
                clt_id = id
            ,   clt_name = id
            ,   clt_loc_name = mlocid
            ,   clt_path = file
            ,   clt_targeted_modifier = Nothing
            ,   clt_equipment_bonus = Nothing
            ,   clt_hidden_modifier = Nothing
            ,   clt_modifier = Nothing
            ,   clt_cp_cap = Nothing
            }) effects
        return $ Right (Just cclt)
    where
        addSection :: HOI4CountryLeaderTrait -> GenericStatement -> HOI4CountryLeaderTrait
        addSection clt stmt@[pdx| $lhs = @_ |] = case lhs of
            "targeted_modifier" -> let oldstmt = fromMaybe [] (clt_targeted_modifier clt) in
                    clt { clt_targeted_modifier = Just (oldstmt ++ [stmt]) }
            "equipment_bonus" -> clt { clt_equipment_bonus = Just stmt }
            "hidden_modifier" -> clt { clt_hidden_modifier = Just stmt }
            "ai_strategy" -> clt
            "ai_will_do" -> clt
            "random" -> clt
            _ -> warn (UnknownSection "country leader trait" stmt) clt
         -- Must be an effect
        addSection clt [pdx| random = %_ |] = clt
        addSection clt [pdx| command_power = %_ |] = clt
        addSection clt [pdx| command_cap_increase = $txt |] = clt { clt_cp_cap = Just txt }
        --addSection clt [pdx| command_cap_increase = %_ |] = clt { clt_cp_cap = Just "actual_number_or_something_else,_check_file" }
        addSection clt [pdx| sprite = %_ |] = clt
        addSection clt [pdx| name = $txt |] = clt { clt_name = txt }
        addSection clt stmt =
            let oldmod = fromMaybe [] (clt_modifier clt) in
            clt { clt_modifier = Just (oldmod ++ [stmt]) }
parseHOI4CountryLeaderTrait stmt = rejectForm "country leader trait" stmt

parseHOI4UnitLeaderTraits :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4UnitLeaderTrait)
parseHOI4UnitLeaderTraits scripts = keyedBy ult_id <$>
    parseScriptFiles "unit leader traits"
        (mapM parseHOI4UnitLeaderTrait . concatMap (\case
            [pdx| leader_traits = @traits |] -> traits
            _ -> []))
        scripts

parseHOI4UnitLeaderTrait :: (HOI4Info g, MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4UnitLeaderTrait))
parseHOI4UnitLeaderTrait [pdx| $id = @effects |]
    = withCurrentFile $ \file -> do
        mlocid <- fmap wikifyLocColours <$> getGameL10nIfPresent id
        let cult = foldl' addSection (HOI4UnitLeaderTrait {
                ult_id = id
            ,   ult_loc_name = mlocid
            ,   ult_path = file
            ,   ult_modifier = Nothing
            ,   ult_trait_xp_factor = Nothing
            ,   ult_non_shared_modifier = Nothing
            ,   ult_corps_commander_modifier = Nothing
            ,   ult_field_marshal_modifier = Nothing
            ,   ult_sub_unit_modifiers = Nothing
            ,   ult_attack_skill = Nothing
            ,   ult_defense_skill = Nothing
            ,   ult_planning_skill = Nothing
            ,   ult_logistics_skill = Nothing
            ,   ult_maneuvering_skill = Nothing
            ,   ult_coordination_skill = Nothing
            }) effects
        return $ Right (Just cult)
    where
        addSection :: HOI4UnitLeaderTrait -> GenericStatement -> HOI4UnitLeaderTrait
        addSection ult stmt@[pdx| $lhs = @_ |] = case lhs of
            "modifier" -> ult { ult_modifier = Just stmt }
            "non_shared_modifier" -> ult { ult_non_shared_modifier = Just stmt }
            "corps_commander_modifier" -> ult { ult_corps_commander_modifier = Just stmt }
            "field_marshal_modifier" -> ult { ult_field_marshal_modifier = Just stmt }
            "sub_unit_modifiers" -> ult { ult_sub_unit_modifiers = Just stmt }
            "trait_xp_factor" -> ult { ult_trait_xp_factor = Just stmt }
            "new_commander_weight" -> ult
            "on_add" -> ult
            "on_remove " -> ult
            "daily_effect" -> ult
            "num_parents_needed" -> ult
            "prerequisites" -> ult
            "gain_xp" -> ult
            "gain_xp_leader" -> ult
            "gain_xp_on_spotting" -> ult
            "show_in_combat" -> ult
            "allowed" -> ult
            "ai_will_do" -> ult
            "type" -> ult
            "unit_trigger" -> ult -- what triggers are needed for it to gain xp?
            "unit_type" -> ult -- what unit types it applies to?
            "any_parent" -> ult -- unknown what it does for now. AAT
            "parent" -> ult -- unknown what it does for now. AAT
            "country_trigger" -> ult -- country-scope conditions for gaining xp. AAT
            "allowed_ship_equipments" -> ult -- hull types the trait's ship may have. AAT
            _ -> warn (UnknownSection "unit leader trait" stmt) ult
         -- Must be an effect
        -- Where the trait sits in the GUI relative to its parent; the offset
        -- may be negative, so it is caught before the numeric dispatch below.
        addSection ult [pdx| leader_default_proximity_offset = %_ |] = ult
        addSection ult stmt@[pdx| $lhs = !num |] = case lhs of
            "attack_skill" -> ult { ult_attack_skill = Just num}
            "defense_skill" -> ult { ult_defense_skill = Just num}
            "planning_skill" -> ult { ult_planning_skill = Just num}
            "logistics_skill" -> ult { ult_logistics_skill = Just num}
            "maneuvering_skill" -> ult { ult_maneuvering_skill = Just num}
            "coordination_skill" -> ult { ult_coordination_skill = Just num}
            "attack_skill_factor" -> ult
            "defense_skill_factor" -> ult
            "planning_skill_factor" -> ult
            "logistics_skill_factor" -> ult
            "maneuvering_skill_factor" -> ult
            "coordination_skill_factor" -> ult
            "gui_row" -> ult
            "gui_column" -> ult
            "cost" -> ult
            "num_parents_needed" -> ult
            "gain_xp_on_spotting" -> ult
            _ -> warn (UnknownSection "unit leader trait" stmt) ult
        addSection ult stmt@[pdx| $_ = !num |] = let _ = num ::Int in warn (UnknownSection "unit leader trait" stmt) ult
        addSection ult stmt@[pdx| $lhs = $_ |] = case lhs of
            "type" -> ult
            "trait_type" -> ult
            "slot" -> ult
            "specialist_advisor_trait" -> ult
            "expert_advisor_trait" -> ult
            "genius_advisor_trait" -> ult
            "custom_gain_xp_trigger_tooltip" -> ult
            "custom_prerequisite_tooltip" -> ult
            "parent" -> ult
            "mutually_exclusive" -> ult
            "enable_ability" -> ult
            "custom_effect_tooltip" -> ult
            "override_effect_tooltip" -> ult
            _ -> warn (UnknownSection "unit leader trait" stmt) ult
        addSection ult stmt = warn (UnknownSection "unit leader trait" stmt) ult
parseHOI4UnitLeaderTrait stmt = rejectForm "unit leader trait" stmt
