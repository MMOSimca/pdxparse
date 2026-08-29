{-|
Module      : HOI4.Common
Description : Message handler for Europa Hearts of Iron IV
-}
module HOI4.Common (
        ppScript
    ,   ppStatement
    ,   ppMtth
    ,   ppOne
    ,   ppMany
    ,   AIWillDo (..), AIModifier (..)
    ,   ppAiWillDo, ppAiMod
    ,   extractStmt, matchLhsText, matchExactText
    ,   module HOI4.Types
    ) where



import Control.Monad.State (gets)

import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void)


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- TODO: get rid of these, do icon key lookups from another module
import qualified Data.HashMap.Strict as HM
import Data.Trie (Trie)
import qualified Data.Trie as Tr


import Text.PrettyPrint.Leijen.Text (Doc)

import Abstract -- everything
import qualified Doc -- everything
import HOI4.Messages -- everything
import QQ (pdx)
import SettingsTypes -- everything
import HOI4.Handlers -- everything
import HOI4.SpecialHandlers -- everything
import HOI4.Types -- everything

-- no particular order from here... TODO: organize this!

-- | Format a script as wiki text.
ppScript :: (HOI4Info g, Monad m) =>
    GenericScript -> PPT g m Doc
ppScript [] = return "(Nothing)"
ppScript script = imsg2doc =<< ppMany script

-- | Format a single statement as wiki text.
ppStatement :: (HOI4Info g, Monad m) =>
    GenericStatement -> PPT g m Doc
ppStatement statement = imsg2doc =<< ppIndent statement

ppIndent :: (Monad m, HOI4Info g) => GenericStatement -> PPT g m IndentedMessages
ppIndent stmt = indentUp $ ppOne stmt

flagTextMaybe :: (HOI4Info g, Monad m) => Text -> PPT g m (Text,Text)
flagTextMaybe = fmap (mempty,) . flagText (Just HOI4Country)

-- | Extract the appropriate message(s) from a script. Statements are handled
-- one at a time, except for the runs of them that only make sense together.
ppMany :: (HOI4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppMany scr = indentUp (concat <$> (mapM ppChunk =<< chunkScript scr))

-- | Extract the appropriate message(s) from one chunk of a script.
ppChunk :: (HOI4Info g, Monad m) => ScriptChunk -> PPT g m IndentedMessages
ppChunk (PlainStmt stmt) = ppOne stmt
ppChunk (DynModChunk dmod isSet mods) = ppDynModChunk dmod isSet mods
ppChunk (IdeaSlotChunk tt ideas) = ppIdeaSlotChunk tt ideas
ppChunk (StateChunk states block_pp) = do
    header <- msgToPP (MsgState states)
    return (header ++ block_pp)

-- | Write out what a block in a character's scope comes to. Script says who it
-- is about once and then everything it does to them, and where that comes to a
-- single line the two read as one sentence about the person rather than as a
-- heading with a lone item under it.
--
-- Anything longer keeps the heading, and so does a line that is not a statement
-- about the person: a @<pre>@ has to keep a line of its own to be seen, and a
-- heading of its own would be left hanging.
foldCharacter :: (HOI4Info g, Monad m) => Text -> IndentedMessages -> PPT g m IndentedMessages
foldCharacter name msgs@[(_, msg)] = do
    text <- T.strip <$> messageText msg
    -- The line stands where the heading would have, not where the message it is
    -- made from stood, which was a level further in.
    if isClause text && isThirdPerson text
        then plainMsg ("'''" <> name <> "''' " <> lowerFirst text)
        else heading name msgs
foldCharacter name msgs = heading name msgs

-- | The character's name as a heading, with what happens to them under it.
heading :: (HOI4Info g, Monad m) => Text -> IndentedMessages -> PPT g m IndentedMessages
heading name msgs = (: msgs) <$> plainMsg' (name <> ":")

-- | The number of statements in a script, counting the ones nested inside
-- compound statements as well.
scriptSize :: GenericScript -> Int
scriptSize = sum . map stmtSize
    where
        stmtSize [pdx| %_ = @scr |] = 1 + scriptSize scr
        stmtSize _ = 1

-- | Write out the body of a scripted effect or trigger in place of the name it
-- is invoked by. Script names a block once and calls it wherever it is wanted,
-- which keeps the script tidy, but it leaves a reader of the wiki holding a
-- name and nowhere to find out what it stands for.
ppScriptedBlock :: (HOI4Info g, Monad m) =>
    Text -> Text -> GenericStatement -> GenericStatement -> PPT g m IndentedMessages
ppScriptedBlock what name [pdx| %_ = @scr |] stmt | not (null scr) = do
    expanding <- getExpandedBlocks
    if name `elem` expanding then
        -- The block has come round to invoking itself, whether directly or by
        -- way of another one. Its body is already written out further up the
        -- page, so name it and leave it there.
        plainStatement (what <> "(written out above) ") stmt
    else do
        -- How large a body may be for it to be written out where it is invoked,
        -- and whether a larger one is folded away behind its name rather than
        -- left as the name alone. The script around the call is what the page is
        -- about, and a hundred lines of a helper that fifty other pages call as
        -- well would bury it, so the reader is offered the choice of unfolding
        -- it instead.
        limit <- gets (inlineScriptLimit . getSettings)
        collapse <- gets (collapseLargeScripts . getSettings)
        if scriptSize scr <= limit then
            -- Small enough to read as part of the script around it. 'ppMany'
            -- indents what it writes, and there is nothing here for it to be
            -- indented under, so take that step back off.
            withExpandedBlock name (indentDown (ppMany scr))
        else if not collapse then
            -- Too long, and unfolding blocks are not wanted on this wiki. The
            -- call is all there is left to say which block was passed over, so
            -- it is set as code and said to be one we chose not to write out,
            -- rather than left looking like a statement we failed to read.
            plainMsg $ "Abnormally Large " <> what <> "`"
                        <> Doc.doc2text (genericStatement2doc stmt) <> "`"
        else do
            -- Too long to read in passing. The name stays as the line the reader
            -- sees, with the body folded away behind it. It has to be written as
            -- html on the one line: a wiki list broken across the lines of a
            -- block element stops being a list.
            body <- withExpandedBlock name (ppMany scr)
            case body of
                [] -> plainStatement what stmt
                _ -> do
                    -- The body starts at whatever level the call happened to be
                    -- written at, and 'imsg2doc_html' writes a list only for
                    -- messages above level zero, so stand it on a level of its
                    -- own.
                    let base = minimum (map fst body)
                    bodyDoc <- imsg2doc_html [(i - base + 1, msg) | (i, msg) <- body]
                    plainMsg $ mconcat
                        [ what, "<tt>", Doc.doc2text (genericStatement2doc stmt), "</tt> "
                        , "<div class=\"mw-collapsible mw-collapsed\""
                        , " data-expandtext=\"show\" data-collapsetext=\"hide\">"
                        , Doc.oneLine (Doc.doc2text bodyDoc)
                        , "</div>"
                        ]
ppScriptedBlock what _ _ stmt = plainStatement what stmt

-- | Table of handlers for statements. Dispatch on strings is /much/ quicker
-- using a lookup table than a huge @case@ expression, which uses @('==')@ on
-- each one in turn.
--
-- When adding a new statement handler, add it to one of the sections in
-- alphabetical order if possible, and use one of the generic functions for it
-- if applicable.
ppHandlers :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
ppHandlers = foldl' Tr.unionL Tr.empty
    [ handlersRhsIrrelevant
    , handlersNumeric
    , handlersNumericCompare
    , handlersNumericIcons
    , handlersModifiers
    , handlersCompound
    , handlersLocRhs
    , handlersState
    , handlersTypewriter
    , handlersSimpleIcon
    , handlersSimpleFlag
    , handlersFlagOrYesNo
    , handlersYesNo
    , handlersTextValue
    , handlersTextAtom
    , handlersSpecialComplex
    , handlersIdeas
    , handlersMisc
    , handlersIgnored
    ]

-- | Handlers for statements where RHS is irrelevant (usually "yes")
handlersRhsIrrelevant :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersRhsIrrelevant = Tr.fromList
        [("dismantle_faction"       , rhsAlwaysYes MsgDismantleFaction)
        ,("drop_cosmetic_tag"       , rhsAlwaysYes MsgDropCosmeticTag)
        ,("kill_country_leader"     , rhsAlwaysYes MsgKillCountryLeader)
        ,("leave_faction"           , rhsAlwaysYes MsgLeaveFaction)
        ,("reserve_dynamic_country" , rhsAlwaysYes MsgReserveDynamicCountry)
        ,("retire"                  , rhsAlwaysYes MsgRetire)
        ,("retire_country_leader"   , rhsAlwaysYes MsgRetireCountryLeader)
        ,("set_country_leader_description" , rhsIgnored MsgSetLeaderDescription)
        ,("set_faction_leader"      , rhsAlwaysYes MsgSetFactionLeader)
        ,("set_portraits"            , rhsIgnored MsgSetPortraits)
        ]

-- | Handlers for numeric statements
handlersNumeric :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumeric = Tr.fromList
        [("add_compliance"                   , numeric MsgAddCompliance)
        ,("add_research_slot"                , numeric MsgAddResearchSlot)
        ,("add_resistance"                   , numeric MsgAddResistance)
        ,("add_threat"                       , numeric MsgAddThreat)
        ,("reset_province_name"              , numeric MsgResetProvinceName)
        ,("set_compliance"                   , numeric MsgSetCompliance)
        ,("set_political_power"              , numeric MsgSetPoliticalPower)
        ,("set_resistance"                   , numeric MsgSetResistance)
        ,("set_stability"                    , numeric MsgSetStability)
        ,("set_war_support"                  , numeric MsgSetWarSupport)
        ,("add_logistics"                    , numeric MsgAddLogistics)
        ,("add_planning"                     , numeric MsgAddPlanning)
        ,("add_defense"                      , numeric MsgAddDefense)
        ,("add_attack"                       , numeric MsgAddAttack)
        ,("add_coordination"                 , numeric MsgAddCoordination)
        ,("add_maneuver"                     , numeric MsgAddManeuver)
        ,("add_skill_level"                  , numeric MsgAddSkillLevel)
        ,("add_faction_initiative"           , numeric MsgAddFactionInitiative)
        ,("add_faction_influence_ratio"      , numeric MsgAddFactionInfluenceRatio)
        ,("add_mio_research_bonus"           , numeric MsgAddMioResearchBonus)
        ,("add_cic"                          , numeric MsgAddCic)
        ,("controls_province"                , numeric MsgControlsProvince)
        ,("random_select_amount"             , numeric MsgRandomSelectAmount)
        ,("add_legitimacy"                   , numeric MsgAddLegitimacy)
        ,("has_id"                           , numeric MsgHasUnitLeaderId)
        ]

-- | Handlers for numeric statements that compare
handlersNumericCompare :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericCompare = Tr.fromList
        [("air_base"                         , numericCompare "more than" "less than" MsgAirBase MsgAirBaseVar)
        ,("alliance_strength_ratio"          , numericCompare "more than" "less than" MsgAllianceStrengthRatio MsgAllianceStrengthRatioVar)
        ,("amount_research_slots"            , numericCompare "more than" "less than" MsgAmountResearchSlots MsgAmountResearchSlotsVar)
        ,("any_war_score"                    , numericCompare "over" "under" MsgAnyWarScore MsgAnyWarScoreVar)
        ,("arms_factory"                     , numericCompare "more than" "less than" MsgArmsFactory MsgArmsFactoryVar)
        ,("command_power"                    , numericCompare "more than" "less than" MsgCommandPower MsgCommandPowerVar)
        ,("compare_autonomy_progress_ratio"  , numericCompare "over" "under" MsgCompareAutonomyProgressRatio MsgCompareAutonomyProgressRatioVar)
        ,("dockyard"                         , numericCompare "more than" "less than" MsgDockyard MsgDockyardVar)
        ,("compliance"                       , numericCompare "more than" "less than" MsgCompliance MsgComplianceVar)
        ,("resistance"                       , numericCompare "more than" "less than" MsgResistance MsgResistanceVar)
        ,("enemies_strength_ratio"           , numericCompare "over" "under" MsgEnemiesStrengthRatio MsgEnemiesStrengthRatioVar)
        ,("has_added_tension_amount"         , numericCompare "more than" "less than" MsgHasAddedTensionAmount MsgHasAddedTensionAmountVar)
        ,("has_air_experience"               , numericCompare "more than" "less than" MsgHasAirExperience MsgHasAirExperienceVar)
        ,("has_army_experience"              , numericCompare "more than" "less than" MsgHasArmyExperience MsgHasArmyExperienceVar)
        ,("has_bombing_war_support"          , numericCompare "more than" "less than" MsgHasBombingWarSupport MsgHasBombingWarSupportVar)
        ,("has_casualties_war_support"       , numericCompare "more than" "less than" MsgHasCasualtiesWarSupport MsgHasCasualtiesWarSupportVar)
        ,("has_convoys_war_support"          , numericCompare "more than" "less than" MsgHasConvoysWarSupport MsgHasConvoysWarSupportVar)
        ,("has_equipment"                    , numericCompareCompoundLoc "More than" "Less than" MsgHasEquipment MsgHasEquipmentVar)
        ,("has_navy_experience"              , numericCompare "more than" "less than" MsgHasNavyExperience MsgHasNavyExperienceVar)
        ,("has_army_manpower"                , numericCompareCompound "At least" "At most" MsgHasArmyManpower MsgHasArmyManpowerVar)
        ,("has_legitimacy"                   , numericCompare "more than" "less than" MsgHasLegitimacy MsgHasLegitimacyVar)
        ,("has_manpower"                     , numericCompare "more than" "less than" MsgHasManpower MsgHasManpowerVar)
        ,("has_political_power"              , numericCompare "more than" "less than" MsgHasPoliticalPower MsgHasPoliticalPowerVar)
        ,("political_power_daily"            , numericCompare "more than" "less than" MsgPoliticalPowerDaily MsgPoliticalPowerDailyVar)
        ,("has_stability"                    , numericCompare "more than" "less than" MsgHasStability MsgHasStabilityVar)
        ,("has_war_support"                  , numericCompare "more than" "less than" MsgHasWarSupport MsgHasWarSupportVar)
        ,("industrial_complex"               , numericCompare "more than" "fewer than" MsgIndustrialComplex MsgIndustrialComplexVar)
        ,("infrastructure"                   , numericCompare "more than" "fewer than" MsgInfrastructure MsgInfrastructureVar)
        ,("nuclear_reactor"                  , numericCompare "more than" "less than" MsgNuclearReactor MsgNuclearReactorVar)
        ,("num_of_controlled_factories"      , numericCompare "more than" "fewer than" MsgNumOfControlledFactories MsgNumOfControlledFactoriesVar)
        ,("num_of_controlled_states"         , numericCompare "more than" "fewer than" MsgNumOfControlledStates MsgNumOfControlledStatesVar)
        ,("num_of_civilian_factories"        , numericCompare "More" "Fewer" MsgNumOfCivilianFactories MsgNumOfCivilianFactoriesVar)
        ,("num_of_available_civilian_factories" , numericCompare "more than" "fewer than" MsgNumOfAvailableCivilianFactories MsgNumOfAvailableCivilianFactoriesVar)
        ,("num_of_civilian_factories_available_for_projects" , numericCompare "more than" "less than" MsgNumOfProjectFactories MsgNumOfProjectFactoriesVar)
        ,("num_of_factories"                 , numericCompare "More" "Fewer" MsgNumOfFactories MsgNumOfFactoriesVar)
        ,("num_of_military_factories"        , numericCompare "More" "Fewer" MsgNumOfMilitaryFactories MsgNumOfMilitaryFactoriesVar)
        ,("num_of_nukes"                     , numericCompare "more than" "fewer than" MsgNumOfNukes MsgNumOfNukesVar)
        ,("num_of_naval_factories"           , numericCompare "More" "Fewer" MsgNumOfNavalFactories MsgNumOfNavalFactoriesVar)
        ,("num_of_operatives"                , numericCompare "more than" "fewer than" MsgNumOfOperatives MsgNumOfOperativesVar)
        ,("num_subjects"                     , numericCompare "more than" "fewer than" MsgNumSubjects MsgNumSubjectsVar)
        ,("original_research_slots"          , numericCompare "more than" "fewer than" MsgOriginalResearchSlots MsgOriginalResearchSlotsVar)
        ,("state_population"                 , numericCompare "more than" "less than" MsgStatePopulation MsgStatePopulationVar)
        ,("surrender_progress"               , numericCompare "more than" "less than" MsgSurrenderProgress MsgSurrenderProgressVar)
        ,("threat"                           , numericCompare "more than" "less than" MsgThreat MsgThreatVar)
        ,("fascism"                          , numericCompare "more than" "less than" MsgFascismCompare MsgFascismCompareVar)
        ,("democratic"                       , numericCompare "more than" "less than" MsgDemocraticCompare MsgDemocraticCompareVar)
        ,("communism"                        , numericCompare "more than" "less than" MsgCommunismCompare MsgCommunismCompareVar)
        ,("neutrality"                       , numericCompare "more than" "less than" MsgNeutralityCompare MsgNeutralityCompareVar)
        ,("num_faction_members"              , numericCompare "more than" "fewer than" MsgNumFactionMembers MsgNumFactionMembersVar)
        ,("faction_influence_ratio"          , numericCompare "more than" "less than" MsgFactionInfluenceRatio MsgFactionInfluenceRatioVar)
        ,("average_stats"                    , numericCompare "more than" "less than" MsgAverageStats MsgAverageStatsVar)
        ,("political_power_growth"           , numericCompare "more than" "less than" MsgPoliticalPowerGrowth MsgPoliticalPowerGrowthVar)
        ,("agency_upgrade_number"            , numericCompare "more than" "fewer than" MsgAgencyUpgrades MsgAgencyUpgradesVar)
        -- A building's own name is a trigger for how many levels of it a state
        -- has. The ones above have a message of their own; the rest are named by
        -- their icon. See 'HOI4.Messages.buildingKeys'.
        ,("anti_air_building"                , buildingLevel "anti_air_building")
        ,("air_facility"                     , buildingLevel "air_facility")
        ,("bunker"                           , buildingLevel "bunker")
        ,("coastal_bunker"                   , buildingLevel "coastal_bunker")
        ,("energy_infrastructure"            , buildingLevel "energy_infrastructure")
        ,("fuel_silo"                        , buildingLevel "fuel_silo")
        ,("industrial_infrastructure"        , buildingLevel "industrial_infrastructure")
        ,("land_facility"                    , buildingLevel "land_facility")
        ,("mega_gun_emplacement"             , buildingLevel "mega_gun_emplacement")
        ,("naval_base"                       , buildingLevel "naval_base")
        ,("naval_facility"                   , buildingLevel "naval_facility")
        ,("naval_headquarters"               , buildingLevel "naval_headquarters")
        ,("naval_supply_hub"                 , buildingLevel "naval_supply_hub")
        ,("nuclear_facility"                 , buildingLevel "nuclear_facility")
        ,("radar_station"                    , buildingLevel "radar_station")
        ,("rail_way"                         , buildingLevel "rail_way")
        ,("rocket_site"                      , buildingLevel "rocket_site")
        ,("stronghold_network"               , buildingLevel "stronghold_network")
        ,("supply_node"                      , buildingLevel "supply_node")
        ,("synthetic_refinery"               , buildingLevel "synthetic_refinery")
        ]

-- | Handlers for numeric statements with icons
handlersNumericIcons :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericIcons = Tr.fromList
        [("add_manpower"             , numericIconLoc "Manpower" "MANPOWER" MsgGainLosePosIcon MsgGainLoseLocIconVar)
        ,("add_extra_state_shared_building_slots", numericIcon "building slot" MsgAddExtraStateSharedBuildingSlots MsgAddExtraStateSharedBuildingSlotsVar)
        ,("add_political_power"      , numericIconLoc "Political Power" "POLITICAL_POWER" MsgGainLosePosIcon MsgGainLoseLocIconVar)
        ,("add_command_power"        , numericIconLoc "Command Power" "COMMAND_POWER" MsgGainLosePosIcon MsgGainLoseLocIconVar)
        ,("add_stability"            , numericIconLoc "Stability" "STABILITY" MsgGainLocPC MsgGainLoseLocIconVar)
        ,("add_war_support"          , numericIconLoc "War Support" "WAR_SUPPORT" MsgGainLocPC MsgGainLoseLocIconVar)
        ,("air_experience"           , numericIconLoc "Air Exp" "AIR_EXPERIENCE" MsgGainLosePosIcon MsgGainLoseLocIconVar)
        ,("army_experience"          , numericIconLoc "Army Exp" "ARMY_EXPERIENCE" MsgGainLosePosIcon MsgGainLoseLocIconVar)
        ,("navy_experience"          , numericIconLoc "Navy Exp" "NAVY_EXPERIENCE" MsgGainLosePosIcon MsgGainLoseLocIconVar)
        ]

-- | Handlers for statements pertaining to modifiers
handlersModifiers :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersModifiers = Tr.fromList
        [("add_dynamic_modifier"        , addDynamicModifier)
        ,("remove_dynamic_modifier"     , removeDynamicModifier)
        ,("has_dynamic_modifier"        , hasDynamicModifier)
        ,("modifier"                    , handleModifier)
        ,("add_state_modifier"          , plainmodifiermsg MsgAddStateModifier)
        ,("add_power_balance_modifier"  , addPowerBalanceModifier)
        ,("remove_power_balance_modifier" , textAtomKey "id" "modifier" MsgRemovePowerBalanceModifier tryLoc)
        ,("has_power_balance_modifier"  , textAtomKey "id" "modifier" MsgHasPowerBalanceModifier tryLoc)
        ,("research_bonus"              , handleResearchBonus)
        ,("targeted_modifier"           , handleTargetedModifier)
        ,("equipment_bonus"             , handleEquipmentBonus)
        ,("hidden_modifier"             , handleHiddenModifier)

        ,("non_shared_modifier"         , handleModifier)
        ,("corps_commander_modifier"    , handleModifier)
        ,("field_marshal_modifier"      , handleModifier)
        ,("sub_unit_modifiers"          , handleEquipmentBonus)
        ]

-- | Handlers for simple compound statements
handlersCompound :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersCompound = Tr.fromList
        -- Note that "any" can mean "all" or "one or more" depending on context.
        [        -- There is a semantic distinction between "all" and "every",
        -- namely that the former means "this is true for all <type>" while
        -- the latter means "do this for every <type>."
        -- trigger scopes
         ("all_allied_country" {- sic -}, scope HOI4Country     . compoundMessage MsgAllAlliedCountry)
        ,("all_army_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllArmyLeader)
        ,("all_character"               , scope HOI4ScopeCharacter   . compoundMessage MsgAllCharacter)
        ,("all_controlled_state"        , scope HOI4ScopeState  . compoundMessage MsgAllControlledState)
        ,("all_core_state"              , scope HOI4ScopeState  . compoundMessage MsgAllCoreState)
        ,("all_country"                 , scope HOI4Country     . compoundMessage MsgAllCountry)
        ,("all_country_with_original_tag", scope HOI4Country    . compoundMessageExtractTag "original_tag_to_check" MsgAllCountryWithOriginalTag)
        ,("all_enemy_country"           , scope HOI4Country     . compoundMessage MsgAllEnemyCountry)
        ,("all_guaranteed_country"      , scope HOI4Country     . compoundMessage MsgAllGuaranteedCountry)
        ,("all_navy_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllNavyLeader)
        ,("all_neighbor_country"        , scope HOI4Country     . compoundMessage MsgAllNeighborCountry)
        ,("all_neighbor_state"          , scope HOI4ScopeState  . compoundMessage MsgAllNeighborState)
        ,("all_occupied_country"        , scope HOI4Country     . compoundMessage MsgAllOccupiedCountry)
        ,("all_operative_leader"        , scope HOI4Operative   . compoundMessage MsgAllOperativeLeader)
        ,("all_other_country"           , scope HOI4Country     . compoundMessage MsgAllOtherCountry)
        ,("all_owned_state"             , scope HOI4ScopeState  . compoundMessage MsgAllOwnedState)
        ,("all_state"                   , scope HOI4ScopeState  . compoundMessage MsgAllState)
        ,("all_subject_countries"{- sic -}, scope HOI4Country    . compoundMessage MsgAllSubjectCountries)
        ,("all_unit_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllUnitLeader)
        ,("any_allied_country"          , scope HOI4Country     . compoundMessageScope MsgAnyAlliedCountry)
        ,("any_army_leader"             , scope HOI4UnitLeader  . compoundMessageScope MsgAnyArmyLeader)
        ,("any_character"               , scope HOI4ScopeCharacter   . compoundMessageScope MsgAnyCharacter)
        ,("any_controlled_state"        , scope HOI4ScopeState  . compoundMessageScope MsgAnyControlledState)
        ,("any_core_state"              , scope HOI4ScopeState  . compoundMessageScope MsgAnyCoreState)
        ,("any_country"                 , scope HOI4Country     . compoundMessageScope MsgAnyCountry)
        ,("any_country_division"        , scope HOI4Division     . compoundMessageScope MsgAnyCountryDivision)
        ,("any_country_with_core"       , scope HOI4Country     . compoundMessageScope MsgAnyCountryWithCore)
        ,("any_country_with_original_tag", scope HOI4Country    . compoundMessageExtractTag "original_tag_to_check" MsgAnyCountryWithOriginalTag)
        ,("any_enemy_country"           , scope HOI4Country     . compoundMessageScope MsgAnyEnemyCountry)
        ,("any_guaranteed_country"      , scope HOI4Country     . compoundMessageScope MsgAnyGuaranteedCountry)
        ,("any_home_area_neighbor_country", scope HOI4Country    . compoundMessageScope MsgAnyHomeAreaNeighborCountry)
        ,("any_navy_leader"             , scope HOI4UnitLeader  . compoundMessageScope MsgAnyNavyLeader)
        ,("any_neighbor_country"        , scope HOI4Country     . compoundMessageScope MsgAnyNeighborCountry)
        ,("any_neighbor_state"          , scope HOI4ScopeState  . compoundMessageScope MsgAnyNeighborState)
        ,("any_occupied_country"        , scope HOI4Country     . compoundMessageScope MsgAnyOccupiedCountry)
        ,("any_operative_leader"        , scope HOI4Operative   . compoundMessageScope MsgAnyOperativeLeader)
        ,("any_other_country"           , scope HOI4Country     . compoundMessageScope MsgAnyOtherCountry)
        ,("any_owned_state"             , scope HOI4ScopeState  . compoundMessageScope MsgAnyOwnedState)
        ,("any_state"                   , scope HOI4ScopeState  . compoundMessageScope MsgAnyState)
        ,("any_state_division"          , scope HOI4Division    . compoundMessageScope MsgAnyStateDivision)
        ,("any_subject_country"         , scope HOI4Country     . compoundMessageScope MsgAnySubjectCountry)
        ,("any_unit_leader"             , scope HOI4UnitLeader  . compoundMessageScope MsgAnyUnitLeader)
        -- effect scopes
        ,("every_army_leader"           , scope HOI4UnitLeader  . compoundMessageScope MsgEveryArmyLeader)
        ,("every_character"             , scope HOI4ScopeCharacter   . compoundMessageScope MsgEveryCharacter)
        ,("every_controlled_state"      , scope HOI4ScopeState  . compoundMessageScope MsgEveryControlledState)
        ,("every_core_state"            , scope HOI4ScopeState  . compoundMessageScope MsgEveryCoreState)
        ,("every_country"               , scope HOI4Country     . compoundMessageScope MsgEveryCountry)
        ,("every_country_division"      , scope HOI4Division     . compoundMessageScope MsgEveryCountryDivision)
        ,("every_country_with_original_tag", scope HOI4Country  . compoundMessageExtractTag "original_tag_to_check" MsgEveryCountryWithOriginalTag)
        ,("every_enemy_country"         , scope HOI4Country     . compoundMessageScope MsgEveryEnemyCountry)
        ,("every_navy_leader"           , scope HOI4UnitLeader  . compoundMessageScope MsgEveryNavyLeader)
        ,("every_military_industrial_organization" , compoundMessageScope MsgEveryMio)
        ,("all_military_industrial_organization" , compoundMessageScope MsgAllMio)
        ,("any_military_industrial_organization" , compoundMessageScope MsgAllMio)
        ,("random_military_industrial_organization" , compoundMessageScope MsgRandomMio)
        ,("every_neighbor_country"      , scope HOI4Country     . compoundMessageScope MsgEveryNeighborCountry)
        ,("every_neighbor_state"        , scope HOI4ScopeState  . compoundMessageScope MsgEveryNeighborState)
        ,("every_occupied_country"      , scope HOI4Country     . compoundMessageScope MsgEveryOccupiedCountry)
        ,("every_operative"             , scope HOI4Operative   . compoundMessageScope MsgEveryOperative)
        ,("every_other_country"         , scope HOI4Country     . compoundMessageScope MsgEveryOtherCountry)
        ,("every_owned_state"           , scope HOI4ScopeState  . compoundMessageScope MsgEveryOwnedState)
        ,("every_possible_country"      , scope HOI4Country     . compoundMessageScope MsgEveryPossibleCountry)
        ,("every_state"                 , scope HOI4ScopeState  . compoundMessageScope MsgEveryState)
        ,("every_state_division"        , scope HOI4Division     . compoundMessageScope MsgEveryStateDivision)
        ,("every_subject_country"       , scope HOI4Country     . compoundMessageScope MsgEverySubjectCountry)
        ,("every_unit_leader"           , scope HOI4UnitLeader  . compoundMessageScope MsgEveryUnitLeader)
        ,("global_every_army_leader"    , scope HOI4UnitLeader  . compoundMessageScope MsgGlobalEveryArmyLeader)
        ,("random_army_leader"          , scope HOI4UnitLeader  . compoundMessageScope MsgRandomArmyLeader)
        ,("random_character"            , scope HOI4ScopeCharacter   . compoundMessageScope MsgRandomCharacter)
        ,("random_controlled_state"     , scope HOI4ScopeState  . compoundMessageScope MsgRandomControlledState)
        ,("random_core_state"           , scope HOI4ScopeState  . compoundMessageScope MsgRandomCoreState)
        ,("random_country"              , scope HOI4Country     . compoundMessageScope MsgRandomCountry)
        ,("random_country_division"     , scope HOI4Country     . compoundMessageScope MsgRandomCountryDivision)
        ,("random_country_with_original_tag", scope HOI4Country . compoundMessageExtractTag "original_tag_to_check" MsgRandomCountryWithOriginalTag)
        ,("random_enemy_country"        , scope HOI4Country     . compoundMessageScope MsgRandomEnemyCountry)
        ,("random_navy_leader"          , scope HOI4UnitLeader  . compoundMessageScope MsgRandomNavyLeader)
        ,("random_neighbor_country"     , scope HOI4Country     . compoundMessageScope MsgRandomNeighborCountry)
        ,("random_neighbor_state"       , scope HOI4ScopeState  . compoundMessageScope MsgRandomNeighborState)
        ,("random_occupied_country"     , scope HOI4Country     . compoundMessageScope MsgRandomOccupiedCountry)
        ,("random_operative"            , scope HOI4Operative   . compoundMessageScope MsgRandomOperative)
        ,("random_other_country"        , scope HOI4Country     . compoundMessageScope MsgRandomOtherCountry)
        ,("random_owned_controlled_state" , scope HOI4ScopeState . compoundMessageScope MsgRandomOwnedControlledState)
        ,("random_owned_state"          , scope HOI4ScopeState  . compoundMessageScope MsgRandomOwnedState)
        ,("random_state"                , scope HOI4ScopeState  . compoundMessageScope MsgRandomState)
        ,("random_state_division"       , scope HOI4Division    . compoundMessageScope MsgRandomStateDivision)
        ,("random_subject_country"      , scope HOI4Country     . compoundMessageScope MsgRandomSubjectCountry)
        ,("random_unit_leader"          , scope HOI4UnitLeader  . compoundMessageScope MsgRandomUnitLeader)
        -- dual scopes
        ,("root"                        , compoundMessagePronoun) --ROOT
        ,("prev"                        , compoundMessagePronoun) --PREV
        ,("prev.prev"                   , compoundMessagePronoun) -- need beter way
        ,("prev.prev.prev"              , compoundMessagePronoun) -- need beter way
        ,("from"                        , compoundMessagePronoun) --FROM
        ,("from.from"                   , compoundMessagePronoun) -- need beter way
        ,("from.from.from"              , compoundMessagePronoun) -- need beter way
        -- no THIS, not used on LHS
        ,("overlord"                    , scope HOI4Country   . compoundMessageScope MsgOverlordSCOPE)
        ,("faction_leader"              , scope HOI4Country   . compoundMessageScope MsgFactionLeaderSCOPE)
        ,("owner"                       , compoundMessagePronoun)
        ,("controller"                  , scope HOI4Country   . compoundMessageScope MsgControllerSCOPE)
        ,("capital_scope"               , scope HOI4ScopeState  . compoundMessageScope MsgCapitalSCOPE)
        ,("event_target"        , compoundMessageTagged MsgSCOPEEventTarget (Just HOI4Misc)) -- Tagged blocks
        ,("var"                 , compoundMessageTagged MsgSCOPEVariable (Just HOI4Misc)) -- Tagged blocks
        -- arrays
        ,("all_of_scopes"               , scope HOI4Misc . compoundMessageExtract "array" MsgAllOfScopes)
        ,("any_of_scopes"               , scope HOI4Misc . compoundMessageExtract "array" MsgAnyOfScopes)
        ,("for_each_scope_loop"         , scope HOI4Misc . compoundMessageExtract "array" MsgForEachScopeLoop)
        ,("random_scope_in_array"       , scope HOI4Misc . compoundMessageExtract "array" MsgRandomScopeInArray)
        -- flow control
        ,("and"                         , compoundMessage MsgAnd) --AND
        ,("not"                         , compoundMessageNot) --NOT
        ,("or"                          , compoundMessage MsgOr) --OR
        ,("count_triggers"          ,   compoundMessageExtractNum "amount" MsgCountTriggers)
        ,("hidden_trigger"          ,                      compoundMessage MsgHiddenTriggers)
        ,("custom_trigger_tooltip"  ,                      compoundMessage MsgCustomTriggerTooltip)
        ,("hidden_effect"           ,                      compoundMessage MsgHiddenEffect)
        -- What follows is done to the country made up here, not to the one the
        -- script was about.
        ,("create_dynamic_country"  , scope HOI4Country . compoundMessageExtractTag "original_tag" MsgCreateDynamicCountry)
        ,("else"                    ,                      compoundMessage MsgElse)
        ,("else_if"                 ,                      compoundMessageCondition MsgElseIf)
        ,("if"                      ,                      compoundMessageCondition MsgIf)
        ,("limit"                   , setIsInEffect False . compoundMessage MsgLimit) -- always needs editing
        ,("prioritize"              ,                       prioritize) -- always needs editing
        ,("while_loop_effect"       ,                       compoundMessage MsgWhile) -- always needs editing
        ,("for_loop_effect"         ,                       compoundMessage MsgFor) -- always needs editing
        -- random and random_list are also part of flow control but are more complicated
        ]

-- Helpers for LocRhs
withLocAtomName :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
withLocAtomName msg = withLocAtom' msg (<> "_name")

-- | Handlers for simple statements where RHS is a localizable atom
handlersLocRhs :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersLocRhs = Tr.fromList
        [("create_faction"        , withLocAtom MsgCreateFaction)
        ,("set_state_name"        , withLocAtom MsgSetStateName)
        ,("set_state_category"    , withLocAtom MsgSetStateCategory)
        ,("custom_effect_tooltip" , customEffectTooltip)
        ,("has_country_leader_with_trait" , withLocAtom MsgHasCountryLeaderWithTrait)
        ,("has_decision"          , withLocAtomKey MsgHasDecision)
        ,("has_power_balance"     , withLocAtomCompound MsgHasPowerBalance)
        ,("has_tech"              , withLocAtom MsgHasTech)
        ,("has_template"          , withLocAtom MsgHasTemplate)
        ,("occupation_law"        , withLocAtom MsgOccupationLaw)
        ,("is_character"          , withLocAtom MsgIsCharacter)
        ,("is_on_continent"       , withLocAtom MsgIsOnContinent)
        ,("is_in_tech_sharing_group" , withLocAtomName MsgIsInTechSharingGroup)
        ,("add_to_tech_sharing_group" , withLocAtomName MsgAddToTechSharingGroup)
        ,("remove_power_balance"  , withLocAtomCompound MsgRemovePowerBalance)
        ,("has_idea_with_trait"   , withLocAtom MsgHasIdeaWithTrait)
        ,("has_state_category"    , withLocAtom MsgHasStateCategory)
        ,("has_country_leader_ideology" , withLocAtom MsgHasCountryLeaderIdeology)
        -- The rules are localized under their name in capitals, the same way
        -- 'HOI4.Handlers.setRule' looks them up.
        ,("has_rule"              , withLocAtom' MsgHasCountryRule T.toUpper)
        ,("is_researching_technology" , withLocAtom MsgIsResearchingTechnology)
        ,("set_faction_name"      , withLocAtom MsgSetFactionName)
        ,("retire_ideology_leader" , withLocAtom MsgRetireIdeologyLeader)
        ,("tooltip"               , tooltip)
        ]

-- | Handlers for statements whose RHS is a state ID
handlersState :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersState = Tr.fromList
        [("add_state_claim"     , withState MsgAddStateClaim)
        ,("add_state_core"      , withState MsgAddStateCore)
        ,("controls_state"      , withState MsgControlsState)
        ,("has_full_control_of_state" , withState MsgHasFullControlOfState)
        ,("owns_state"          , withState MsgOwnsState)
        ,("remove_state_claim"  , withState MsgRemoveStateClaim)
        ,("remove_state_core"   , withState MsgRemoveStateCore)
        ,("set_state_controller" , withState MsgSetStateController)
        ,("set_state_owner"     , withState MsgSetStateOwner)
        ,("state"               , withState MsgStateId)
        ,("remove_resource_rights" , withState MsgRemoveResourceRights)
        ,("transfer_state"      , withState MsgTransferState)
        ]

-- | Simple statements whose RHS should be presented as is, in typewriter face
--   or just need the RHS unmodified
handlersTypewriter :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTypewriter = Tr.fromList
        [("clr_character_flag"  , withMaybelocAtom2 MsgCharacterFlag MsgClearFlag)
        ,("clr_country_flag"    , withMaybelocAtom2 MsgCountryFlag MsgClearFlag)
        ,("clr_global_flag"     , withMaybelocAtom2 MsgGlobalFlag MsgClearFlag)
        ,("clr_state_flag"      , withMaybelocAtom2 MsgStateFlag MsgClearFlag)
        ,("clr_unit_leader_flag" , withMaybelocAtom2 MsgUnitLeaderFlag MsgClearFlag)
        ,("has_focus_tree"        , withNonlocAtom MsgHasFocusTree)
        ,("save_event_target_as", withNonlocAtom MsgSaveEventTargetAs)
        ,("save_global_event_target_as", withNonlocAtom MsgSaveGlobalEventTargetAs)
        ,("set_cosmetic_tag"    , withNonlocAtom MsgSetCosmeticTag)
        ,("has_cosmetic_tag"    , withNonlocAtom MsgHasCosmeticTag)

        ,("load_oob"            , withNonlocAtom MsgLoadOob)
        ,("has_event_target"    , withNonlocAtom MsgHasEventTarget)
        ,("clear_global_event_target" , withNonlocAtom MsgClearGlobalEventTarget)
        ,("clear_array"         , withNonlocAtom MsgClearArray)
        ,("clear_temp_array"    , withNonlocAtom MsgClearTempArray)
        ,("has_faction_template" , withNonlocAtom MsgHasFactionTemplate)
        ,("has_opinion_modifier"  , withNonlocAtom MsgHasOpinionMod)
        ]

-- | Handlers for simple statements with icon
handlersSimpleIcon :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSimpleIcon = Tr.fromList
        [("can_construct_building"  , withLocAtomIcon MsgCanConstructBuilding False)
        ,("has_autonomy_state"      , withLocAtomIcon MsgHasAutonomyState True)
        ,("has_government"          , withLocAtomIcon MsgHasGovernment False)
        ]

-- | Handlers for simple statements with a flag or pronoun
handlersSimpleFlag :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSimpleFlag = Tr.fromList
        [("add_claim_by"            , withFlag MsgAddClaimBy)
        ,("add_core_of"             , withFlag MsgAddCoreOf)
        ,("add_nationality"         , withFlag MsgAddNationality)
        ,("add_to_faction"          , withFlag MsgAddToFaction)
        ,("change_tag_from"         , withFlag MsgChangeTagFrom)
        ,("country_exists"          , withFlag MsgCountryExists)
        ,("has_defensive_war_with"  , withFlag MsgHasDefensiveWarWith)
        ,("give_guarantee"          , withFlag MsgGiveGuarantee)
        ,("give_military_access"    , withFlag MsgGiveMilitaryAccess)
        ,("has_attache_from"        , withFlag MsgHasAttacheFrom)
        ,("has_border_war_with"     , withFlag MsgHasBorderWarWith)
        ,("has_guaranteed"          , withFlag MsgHasGuaranteed)
        ,("has_military_access_to" , withFlag MsgHasMilitaryAccessTo)
        ,("gives_military_access_to" , withFlag MsgGivesMilitaryAccessTo)
        ,("has_non_aggression_pact_with" , withFlag MsgHasNonAggressionPactWith)
        ,("has_offensive_war_with"  , withFlag MsgHasOffensiveWarWith)
        ,("has_subject"             , withFlag MsgHasSubject)
        ,("occupied_country_tag"    , withFlag MsgOccupiedCountryTag)
        ,("inherit_technology"      , withFlag MsgInheritTechnology)
        ,("is_ally_with"            , withFlag MsgIsAllyWith)
        ,("is_controlled_by"        , withFlag MsgIsControlledBy)
        ,("is_exiled_in"            , withFlag MsgIsExiledIn)
        ,("is_fully_controlled_by"  , withFlag MsgIsFullyControlledBy)
        ,("is_guaranteed_by"        , withFlag MsgIsGuaranteedBy)
        ,("is_hosting_exile"        , withFlag MsgIsHostingExile)
        ,("is_in_faction_with"      , withFlag MsgIsInFactionWith)
        ,("is_justifying_wargoal_against" , withFlag MsgIsJustifyingWargoalAgainst)
        ,("is_neighbor_of"          , withFlag MsgIsNeighborOf)
        ,("is_owned_and_controlled_by" , withFlag MsgIsOwnedAndControlledBy)
        ,("is_puppet_of"            , withFlag MsgIsPuppetOf)
        ,("is_claimed_by"           , withFlag MsgIsClaimedBy)
        ,("is_core_of"              , withFlag MsgIsStateCore)
        ,("is_subject_of"           , withFlag MsgIsSubjectOf)
        ,("is_owned_by"             , withFlag MsgIsOwnedBy)
        ,("recall_volunteers_from"  , withFlag MsgRecallVolunteersFrom)
        ,("release"                 , withFlag MsgRelease)
        ,("release_puppet"          , withFlag MsgReleasePuppet)
        ,("release_puppet_on_controlled" , withFlag MsgReleasePuppet)
        ,("remove_claim_by"         , withFlag MsgRemoveClaimBy)
        ,("remove_core_of"          , withFlag MsgRemoveCoreOf)
        ,("remove_from_faction"     , withFlag MsgRemoveFromFaction)
        ,("set_state_controller_to" , withFlag MsgSetStateControllerTo)
        ,("tag"                     , withFlag MsgCountryIs)
        ,("transfer_state_to"       , withFlag MsgTransferStateTo)
        ,("has_war_with"            , withFlag MsgHasWarWith)
        ,("has_war_together_with"   , withFlag MsgHasWarTogetherWith)
        ,("release_on_controlled"   , withFlag MsgReleaseOnControlled)
        ,("end_puppet"              , withFlag MsgEndPuppet)
        ,("send_embargo"            , withFlag MsgSendEmbargo)
        ,("break_embargo"           , withFlag MsgBreakEmbargo)
        ,("add_civil_war_target"    , withFlag MsgAddCivilWarTarget)
        ,("set_state_owner_to"      , withFlag MsgSetStateOwnerTo)
        ,("original_tag"            , withFlag MsgOriginalTag)
        ]

-- | Handlers for simple generic statements with a flag or "yes"/"no"
handlersFlagOrYesNo :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersFlagOrYesNo = Tr.fromList
        [("start_resistance"            , withFlagOrBool MsgStartResistance MsgCountryStartResistance)
        ]

-- | Handlers for yes/no statements
handlersYesNo :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersYesNo = Tr.fromList
        [("is_ai"                       , withBool MsgIsAIControlled)
        ,("advisor_can_be_fired"        , withBool MsgAdvisorCanBeFired)
        ,("is_advisor"                  , withBool MsgIsAdvisor)
        ,("is_hired_as_advisor"         , withBool MsgIsHiredAsAdvisor)
        ,("always"                      , withBool MsgAlways)
        ,("country_lock_all_division_template" , withBool MsgLockDivision)
        ,("exists"                      , withBool MsgExists)
        ,("has_attache"                 , withBool MsgHasAttache)
        ,("has_border_war"              , withBool MsgHasBorderWar)
        ,("has_capitulated"             , withBool MsgHasCapitulated)
        ,("has_civil_war"               , withBool MsgHasCivilWar)
        ,("has_defensive_war"           , withBool MsgHasDefensiveWar)
        ,("has_intelligence_agency"     , withBool MsgHasIntelligenceAgency)
        ,("is_field_marshal"            , withBool MsgIsFieldMarshal)
        ,("is_corps_commander"          , withBool MsgIsCorpsCommander)
        ,("has_done_agency_upgrade"     , hasDoneAgencyUpgrade)
        ,("create_intelligence_agency"  , createIntelligenceAgency)
        ,("upgrade_intelligence_agency" , upgradeIntelligenceAgency)
        ,("has_offensive_war"           , withBool MsgHasOffensiveWar)
        ,("has_war"                     , withBool MsgHasWar)
        ,("has_war_with_major"          , withBool MsgHasWarWithMajor)
        ,("is_capital"                  , withBool MsgIsCapital)
        ,("is_coastal"                  , withBool MsgIsCoastal)
        ,("is_country_leader"           , withBool MsgIsCountryLeader)
        ,("is_demilitarized_zone"       , withBool MsgIsDemilitarizedZone)
        ,("is_faction_leader"           , withBool MsgIsFactionLeader)
        ,("is_female"                   , withBoolHOI4Scope MsgIsFemaleLeader MsgIsFemale)
        ,("is_government_in_exile"      , withBool MsgIsGovernmentInExile)
        ,("is_historical_focus_on"      , withBool MsgIsHistoricalFocusOn)
        ,("is_operative_captured"       , withBool MsgIsOperativeCaptured)
        ,("is_in_faction"               , withBool MsgIsInFaction)
        ,("is_in_home_area"             , withBool MsgIsInHomeArea)
        ,("is_island_state"             , withBool MsgIsIslandState)
        ,("is_major"                    , withBool MsgIsMajor)
        ,("is_puppet"                   , withBool MsgIsPuppet)
        ,("is_subject"                  , withBool MsgIsSubject)
        ,("is_unit_leader"              , withBool MsgIsUnitLeader)
        ,("has_elections"               , withBool MsgHasElections)
        ,("has_resistance"              , withBool MsgHasResistance)
        ,("is_debug"                    , withBool MsgIsDebug)
        ,("impassable"                  , withBool MsgIsImpassable)
        ,("has_any_power_balance"       , withBool MsgHasAnyPowerBalance)
        ,("set_demilitarized_zone"      , withBool MsgSetDemilitarizedZone)
        ,("set_major"                   , withBool MsgSetMajor)
        ]

-- Helpers for text/value pairs
tryLocAndIconTitle :: (HOI4Info g, Monad m) => Text -> PPT g m (Text, Text)
tryLocAndIconTitle t = tryLocAndIcon (t <> "_title")

-- | Handlers for text/value pairs.
--
-- $textvalue
handlersTextValue :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTextValue = Tr.fromList
        [("add_offsite_building"        , textValue "type" "level" MsgAddOffsiteBuilding MsgAddOffsiteBuildingVar tryLocAndIcon)
        ,("add_popularity"              , textValue "ideology" "popularity" MsgAddPopularity MsgAddPopularityVar ideologyIconLoc)
        ,("add_power_balance_value"     , textValueKey "id" "value" MsgAddPowerBalanceValue MsgAddPowerBalanceValueVar)
        ,("core_compliance"             , textValueCompare "occupied_country_tag" "value" "more than" "less than" MsgCoreCompliance MsgCoreComplianceVar flagTextMaybe)
        ,("core_resistance"             , textValueCompare "occupied_country_tag" "value" "more than" "less than" MsgCoreResistance MsgCoreResistanceVar flagTextMaybe)
        ,("has_volunteers_amount_from"  , textValueCompare "tag" "count" "more than" "less than" MsgHasVolunteersAmountFrom MsgHasVolunteersAmountFromVar flagTextMaybe)
        ,("modify_tech_sharing_bonus"   , textValue "id" "bonus" MsgModifyTechSharingBonus MsgModifyTechSharingBonusVar tryLocMaybe) --icon ignored
        ,("power_balance_value"         , textValueCompare "id" "value" "more than" "less than" MsgPowerBalanceValue MsgPowerBalanceValueVar tryLocMaybe)
        ,("set_province_name"           , textValue "name" "id" MsgSetProvinceName MsgSetProvinceNameVar tryLocMaybe)
        ,("set_victory_points"          , valueValue "province" "value" MsgSetVictoryPoints MsgSetVictoryPointsVar)
        ,("add_victory_points"          , valueValue "province" "value" MsgAddVictoryPoints MsgAddVictoryPointsVar)
        ,("stockpile_ratio"             , textValueCompare "archetype" "ratio" "more than" "less than" MsgStockpileRatio MsgStockpileRatioVar tryLocMaybe)
        ,("strength_ratio"              , textValueCompare "tag" "ratio" "more than" "less than" MsgStrengthRatio MsgStrengthRatioVar flagTextMaybe)
        ,("remove_building"             , textValue "type" "level" MsgRemoveBuilding MsgRemoveBuildingVar tryLocAndIcon)
        ,("modify_character_flag"       , withNonlocTextValue "flag" "value" MsgCharacterFlag MsgModifyFlag MsgModifyFlagVar) -- Localization/icon ignored
        ,("modify_country_flag"         , withNonlocTextValue "flag" "value" MsgCountryFlag MsgModifyFlag MsgModifyFlagVar) -- Localization/icon ignored
        ,("modify_global_flag"          , withNonlocTextValue "flag" "value" MsgGlobalFlag MsgModifyFlag MsgModifyFlagVar) -- Localization/icon ignored
        ,("modify_state_flag"           , withNonlocTextValue "flag" "value" MsgStateFlag MsgModifyFlag MsgModifyFlagVar) -- Localization/icon ignored
        ,("fighting_army_strength_ratio" , textValueCompare "tag" "ratio" "more than" "less than" MsgFightingArmyStrengthRatio MsgFightingArmyStrengthRatioVar flagTextMaybe)
        ,("distance_to"                 , textValueCompare "target" "value" "more than" "less than" MsgDistanceTo MsgDistanceToVar flagTextMaybe)
        ,("set_political_party"         , textValue "ideology" "popularity" MsgSetPoliticalParty MsgSetPoliticalPartyVar tryLocAndIcon)
        ,("modify_unit_leader_flag"     , withNonlocTextValue "flag" "value" MsgUnitLeaderFlag MsgModifyFlag MsgModifyFlagVar) -- Localization/icon ignored
        ]

flagMaybeText :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
flagMaybeText txt = eflag (Just HOI4Country) (Left txt)
-- | Handlers for text/atom pairs
handlersTextAtom :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTextAtom = Tr.fromList
        [("has_game_rule"               , textAtom "rule" "option" MsgHasRule tryLoc)
        ,("has_core_occupation_modifier" , textAtom "occupied_country_tag" "modifier" MsgHasCoreOccupationModifier flagMaybeText)
        ]

-- | Handlers for special complex statements
-- most of these have a non-standard right-hand side which contains multiple fields in brackets
handlersSpecialComplex :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSpecialComplex = Tr.fromList
        [("add_building_construction"    , addBuildingConstruction)
        ,("construct_building_in_random_province" , constructBuildingInRandomProvince)
        ,("add_doctrine_cost_reduction"  , addDoctrineCostReduction)
        ,("add_province_modifier"        , addProvinceModifier True)
        ,("remove_province_modifier"     , addProvinceModifier False)
        ,("add_equipment_to_stockpile"   , addEquipment)
        ,("add_named_threat"             , addNamedThreat)
        ,("add_opinion_modifier"         , opinion MsgAddOpinion MsgAddOpinionDur MsgAddTradeOpinion)
        ,("add_relation_modifier"        , relationModifier MsgAddRelationModifier True)
        ,("add_relation_rule_override"   , addRelationRuleOverride)
        ,("add_mastery"                  , addMastery False)
        ,("add_daily_mastery"            , addMastery True)
        ,("add_mastery_bonus"            , addMasteryBonus)
        ,("activate_advisor"             , advisorPost MsgActivateAdvisor)
        ,("deactivate_advisor"           , advisorPost MsgDeactivateAdvisor)
        ,("set_can_be_fired_in_advisor_role" , setCanBeFiredInAdvisorRole)
        ,("remove_relation_modifier"     , relationModifier MsgRemoveRelationModifier False)
        ,("has_relation_modifier"        , relationModifier MsgHasRelationModifier False)
        ,("amount_taken_ideas"           , amountTakenIdeas)
        ,("add_scientist_xp"             , addScientistXp)
        ,("gain_xp"                      , gainXp)
        ,("has_resources_in_country"     , hasResourcesInCountry)
        ,("add_tech_bonus"               , addTechBonus)
        ,("add_breakthrough_progress"    , addBreakthrough MsgAddBreakthroughProgress MsgBreakthroughProgress)
        ,("add_breakthrough_points"      , addBreakthrough MsgAddBreakthroughPoints MsgBreakthroughPoints)
        ,("add_to_war"                   , addToWar)
        ,("annex_country"                , annexCountry)
        ,("reverse_add_opinion_modifier" , opinion MsgReverseAddOpinion MsgReverseAddOpinionDur MsgReverseAddTradeOpinion)
        ,("build_railway"                , buildRailway)
        ,("can_build_railway"            , canBuildRailway)
        ,("create_equipment_variant"     , createEquipmentVariant)
        ,("create_wargoal"               , createWargoal)
        ,("create_unit"                  , createUnit)
        ,("custom_trigger_tooltip"       , customTriggerTooltip)
        ,("custom_override_tooltip"      , customOverrideTooltip)
        ,("country_event"                , triggerEvent MsgCountryEvent)
        ,("declare_war_on"               , declareWarOn)
        ,("free_building_slots"          , freeBuildingSlots)
        ,("has_completed_focus"          , handleFocus MsgHasCompletedFocus)
        ,("complete_national_focus"      , handleFocus MsgCompleteNationalFocus)
        ,("unlock_national_focus"        , handleFocus MsgUnlockNationalFocus)
        ,("focus"                        , handleFocus MsgFocus) -- used in pre-requisite for focuses
        ,("focus_progress"               , focusProgress MsgFocusProgress)
        ,("uncomplete_national_focus"    , focusUncomplete MsgUncompleteNationalFocus)
        ,("has_army_size"                , hasArmySize)
        ,("has_navy_size"                , hasNavySize)
        ,("has_opinion"                  , hasOpinion MsgHasOpinion)
        ,("has_country_leader"           , hasCountryLeader)
        ,("is_power_balance_in_range"    , powerBalanceRange)
        ,("add_opinion_modifier"         , opinion MsgAddOpinion (\modid what who _years -> MsgAddOpinion modid what who) MsgAddTradeOpinion)
        ,("load_focus_tree"              , loadFocusTree)
        ,("modify_building_resources"    , modifyBuildingResources)
        ,("naval_strength_comparison"    , navalStrengthComparison)
        ,("release_autonomy"             , setAutonomy MsgReleaseAutonomy)
        ,("remove_opinion_modifier"      , opinion MsgRemoveOpinionMod (\modid what who _years -> MsgRemoveOpinionMod modid what who) MsgRemoveTradeOpinion)
        ,("set_autonomy"                 , setAutonomy MsgSetAutonomy)
        ,("set_building_level"           , setBuildingLevel)
        ,("set_politics"                 , setPolitics)
        ,("set_party_name"               , setPartyName)
        ,("start_civil_war"              , startCivilWar)
        ,("start_border_war"             , startBorderWar)
        ,("has_resources_amount"         , hasResourcesAmount)
        ,("any_province_building_level"  , anyProvinceBuildingLevel)
        ,("compare_autonomy_state"       , compareAutonomyState)
        ,("add_to_array"                 , arrayValue MsgAddToArray)
        -- The array is named on the left in the short form, so any name at all
        -- may stand there: @temp_states = THIS@ names no effect of the game's.
        ,("add_to_temp_array"            , arrayValue MsgAddToTempArray)
        ,("is_in_array"                  , arrayValue MsgIsInArray)
        ,("create_ship"                  , createShip)
        ,("transfer_ship"                , transferShip)
        ,("add_equipment_subsidy"        , addEquipmentSubsidy)
        ,("add_equipment_production"     , addEquipmentProduction)
        ,("create_production_license"    , createProductionLicense)
        ,("create_faction_from_template" , createFactionFromTemplate)
        ,("add_units_to_division_template" , addUnitsToDivisionTemplate)
        ,("remove_country_leader_role"   , removeCountryLeaderRole)
        ,("set_division_template_cap"    , setDivisionTemplateCap)
        ,("set_truce"                    , setTruce)
        ,("white_peace"                  , whitePeace)
        ,("puppet"                       , puppetCountry)
        ,("set_power_balance"            , setPowerBalance)
        ,("get_highest_scored_country"   , getHighestScoredCountry)
        ,("add_contested_owner"          , addContestedOwner)
        ,("has_shine_effect_on_focus"    , handleFocus MsgHasShineEffectOnFocus)
        ,("is_military_industrial_organization" , isMio)
        ,("has_doctrine"                  , hasDoctrine)
        ,("can_be_country_leader"        , canBeCountryLeader)
        ,("remove_from_array"            , arrayValue MsgRemoveFromArray)
        ,("activate_shine_on_focus"      , handleFocus MsgActivateShineOnFocus)
        ,("remove_unit_leader_role"      , rhsAlwaysYes MsgRemoveUnitLeaderRole)
        ,("complete_mio_trait"           , completeMioTrait)
        ,("transfer_units_fraction"      , transferUnitsFraction)
        ,("add_resistance_target"        , addResistanceTarget)

        -- Events
        ,("news_event"                   , triggerEvent MsgNewsEvent)
        ,("state_event"                  , triggerEvent MsgStateEvent)
        ,("unit_leader_event"            , triggerEvent MsgUnitLeaderEvent)
        ,("operative_leader_event"       , triggerEvent MsgOperativeEvent)

        -- Flags
        ,("set_character_flag"           , setFlag MsgCharacterFlag)
        ,("set_country_flag"             , setFlag MsgCountryFlag)
        ,("set_global_flag"              , setFlag MsgGlobalFlag)
        ,("set_state_flag"               , setFlag MsgStateFlag)
        ,("set_unit_leader_flag"         , setFlag MsgUnitLeaderFlag)
        ,("has_character_flag"           , hasFlag MsgCharacterFlag)
        ,("has_country_flag"             , hasFlag MsgCountryFlag)
        ,("has_global_flag"              , hasFlag MsgGlobalFlag)
        ,("has_state_flag"               , hasFlag MsgStateFlag)
        ,("has_unit_leader_flag"         , hasFlag MsgUnitLeaderFlag)

        ,("set_nationality"              , setNationality)

        -- Effects
        -- simpleEffectAtom and simpleEffectNum

        -- Variables
        ,("set_variable"                 , setVariable MsgSetVariable MsgSetVariableVal)
        ,("set_temp_variable"            , setVariable MsgSetTempVariable MsgSetTempVariableVal)
        ,("add_to_variable"              , setVariable MsgAddVariable MsgAddVariableVal)
        ,("add_to_temp_variable"         , setVariable MsgAddTempVariable MsgAddTempVariableVal)
        ,("subtract_from_variable"       , setVariable MsgSubVariable MsgSubVariableVal)
        ,("subtract_from_temp_variable"  , setVariable MsgSubTempVariable MsgSubTempVariableVal)
        ,("multiply_variable"            , setVariable MsgMulVariable MsgMulVariableVal)
        ,("multiply_temp_variable"       , setVariable MsgMulTempVariable MsgMulTempVariableVal)
        ,("divide_variable"              , setVariable MsgDivVariable MsgDivVariableVal)
        ,("divide_temp_variable"         , setVariable MsgDivTempVariable MsgDivTempVariableVal)
        ,("check_variable"               , checkVariable MsgCheckVariable MsgCheckVariableVal)
        ,("clamp_variable"               , clampVariable MsgClampVariableValVal MsgClampVariableValVar MsgClampVariableVarVal MsgClampVariableVarVar)
        ,("clamp_temp_variable"          , clampVariable MsgClampTempVariableValVal MsgClampTempVariableValVar MsgClampTempVariableVarVal MsgClampTempVariableVarVar)
        ,("is_variable_equal"            , setVariable MsgEquVariable MsgEquVariableVal)
        ,("export_to_variable"           , exportVariable)
        ,("round_variable"              , withNonlocAtom MsgRoundVariable)
        ,("round_temp_variable"         , withNonlocAtom MsgRoundTempVariable)
        ,("clear_variable"              , withNonlocAtom MsgClearVariable)
        ,("has_variable"                , withNonlocAtom MsgHasVariable)

        -- Decisions
        ,("activate_decision"            , locandid MsgActivateDecision)
        ,("remove_decision"              , locandid MsgRemoveDecision)
        ,("activate_mission"             , locandid MsgActivateMission)
        ,("remove_mission"               , locandid MsgRemoveMission)
        ,("has_active_mission"           , locandid MsgHasActiveMission)
        ,("activate_targeted_decision"   , textAtomKey "target" "decision" MsgActivateTargetedDecision flagMaybeText)
        ,("remove_targeted_decision"     , textAtomKey "target" "decision" MsgRemoveTargetedDecision flagMaybeText)
        -- The tactic is named by a localization key of its own, kept apart from
        -- the rest in tactics_l_english.yml.
        ,("unlock_tactic"                , withLocAtom MsgUnlockTactic)
        ,("unlock_decision_category_tooltip" , withLocAtom MsgUnlockDecisionCategoryTooltip)
        ,("unlock_decision_tooltip"       , unlockDecisionTooltip)
        ,("add_days_remove"              , textValueKey "decision" "days" MsgAddDaysRemove MsgAddDaysRemoveVar)
        ,("add_days_mission_timeout"     , textValueKey "mission" "days" MsgAddDaysMissionTimeout MsgAddDaysMissionTimeoutVar)
        ,("activate_mission_tooltip"      , withLocAtom MsgActivateMissionTooltip)

        -- Tooltips
        ,("show_unit_leaders_tooltip"     , showUnitLeader)
        ,("character_list_tooltip"        , characterListTooltip)
        ,("mio"                          , mioScope)
        ,("add_mio_size"                 , numeric MsgAddMioSize)
        ,("add_mio_funds"                , numeric MsgAddMioFunds)
        ,("add_mio_funds_gain_factor"    , numeric MsgAddMioFundsGainFactor)
        ,("reduce_focus_completion_cost" , reduceFocusCompletionCost)
        ,("set_division_template_lock"   , setDivisionTemplateLock)
        ,("clear_division_template_cap"  , clearDivisionTemplateCap)
        ,("is_special_project_completed" , withLocAtom MsgIsSpecialProjectCompleted)
        ,("promote_leader"               , rhsAlwaysYes MsgPromoteToFieldMarshal)
        ,("show_mio_tooltip"              , showMio)
        ,("unlock_military_industrial_organization_tooltip" , unlockMio)
        ,("unlock_mio_trait_tooltip"      , unlockMioTrait)
        ,("unlock_mio_policy_tooltip"     , unlockMioPolicy)
        -- Written inside a modifier block, where 'modifierMSG' handles it; this
        -- is for anywhere else it may turn up.
        ,("custom_modifier_tooltip"       , tooltipWith MsgCustomModifierTooltip)
        -- What one option of an event comes to, for an effect that will offer
        -- the player that event.
        ,("event_option_tooltip"          , eventOptionTooltip)
        ]

-- | Handlers for "ideas", which include character traits, national spirits, laws, and more
handlersIdeas :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersIdeas = Tr.fromList
        [("has_idea"                    , handleIdeas False MsgHasIdea)
        ,("add_ideas"                   , handleIdeas True MsgAddIdea)
        ,("remove_ideas"                , handleIdeas False MsgRemoveIdea)
        ,("add_timed_idea"              , handleTimedIdeas MsgAddTimedIdea)
        ,("modify_timed_idea"           , handleTimedIdeas MsgModifyTimedIdea)
        ,("swap_ideas"                  , handleSwapIdeas)
        ,("show_ideas_tooltip"          , showIdea)
        ]

-- | Handlers for miscellaneous statements
handlersMisc :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersMisc = Tr.fromList
        [("add_ace"                     , addAce)
        ,("add_ai_strategy"             , addAiStrategy)
        ,("add_autonomy_ratio"          , addAutonomyRatio MsgAddAutonomyRatio MsgAddAutonomyRatioVar)
        ,("add_autonomy_score"          , addAutonomyRatio MsgAddAutonomyScore MsgAddAutonomyScoreVar)
        ,("add_field_marshal_role"      , addFieldMarshalRole MsgAddFieldMarshalRole)
        ,("create_field_marshal"        , addFieldMarshalRole MsgAddFieldMarshalRole) -- deprecated
        ,("add_corps_commander_role"    , addFieldMarshalRole MsgAddCorpsCommanderRole)
        ,("create_corps_commander"      , addFieldMarshalRole MsgAddCorpsCommanderRole) -- deprecated
        ,("add_naval_commander_role"    , addFieldMarshalRole MsgAddNavalCommanderRole)
        ,("create_navy_leader"          , addFieldMarshalRole MsgAddNavalCommanderRole) -- deprecated
        ,("add_advisor_role"            , addAdvisorRole)
        ,("remove_advisor_role"         , removeAdvisorRole)
        ,("add_country_leader_role"     , addLeaderRole)
        ,("create_country_leader"       , createLeader)
        ,("create_operative_leader"     , createOperativeLeader)
        ,("promote_character"           , promoteCharacter)
        ,("damage_building"             , damageBuilding)
        ,("delete_unit_template_and_units" , deleteUnits MsgDeleteUnitTemplateAndunits)
        ,("delete_units"                , deleteUnits MsgDeleteUnits)
        ,("division_template"           , divisionTemplate)
        ,("army_manpower_in_state"      , divisionsInState MsgArmyManpowerInState)
        ,("divisions_in_state"          , divisionsInState MsgDivisionsInState)
        ,("divisions_in_border_state"   , divisionsInState MsgDivisionsInBorderState)
        ,("add_country_leader_trait"    , addRemoveLeaderTrait MsgAddCountryLeaderTrait)
        ,("remove_country_leader_trait" , addRemoveLeaderTrait MsgRemoveCountryLeaderTrait)
        ,("add_unit_leader_trait"       , addRemoveUnitTrait MsgAddUnitLeaderTrait)
        ,("remove_unit_leader_trait"    , addRemoveUnitTrait MsgRemoveUnitLeaderTrait)
        ,("add_timed_unit_leader_trait" , addTimedTrait)
        ,("swap_ruler_traits"           , swapLeaderTrait)
        ,("swap_country_leader_traits"  , swapLeaderTrait)
        ,("has_trait"                   , withLocAtom MsgHasTrait)
        ,("add_resource"                , addResource)
        ,("date"                        , handleDate "After" "Before")
        ,("has_start_date"              , handleDate "Game initially started after" "Game initially started before")
        ,("random"                      , random)
        ,("random_list"                 , randomList)

        -- Special
        ,("add_trait"           , handleTrait True)
        ,("remove_trait"        , handleTrait False)
        ,("diplomatic_relation" , diplomaticRelation)
        ,("give_resource_rights" , giveResourceRights)
        ,("has_character"       , withCharacter MsgHasCharacter)
        ,("retire_character"    , withCharacter MsgRetireCharacter)
        ,("has_dlc"             , hasDlc)
        ,("has_wargoal_against" , hasWarGoalAgainst)
        ,("region"              , withRegion)
        ,("send_equipment"      , sendEquipment)
        ,("set_capital"         , setCapital MsgSetCapital)
        ,("set_character_name"  , setCharacterName)
        ,("set_popularities"    , setPopularities)
        ,("set_rule"            , setRule MsgSetRule)
        ,("set_technology"      , setTechnology)

        ,("effect_tooltip"        , customTriggerTooltip) -- shows the effects but doesn't execute them, don't know if I want it to show up in the parser
        ]

-- | Handlers for ignored statements
handlersIgnored :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersIgnored = Tr.fromList
        [("custom_tooltip", return $ return [])
        ,("display_individual_scopes", return $ return [])
        ,("play_song"     , return $ return [])
        ,("goto"          , return $ return [])
        ,("log"           , return $ return [])
        ,("required_personality", return $ return[]) -- From the 1.30 patch notes: "The required_personality field will now be ignored"
        ,("highlight"     , return $ return [])
        -- Redraws the focus tree so that the branches it allows are worked out
        -- again. Nothing of the country changes by it.
        ,("mark_focus_tree_layout_dirty", return $ return [])
        -- Keeps the tooltip of a scope showing even where the scope catches
        -- nothing, so that a reader is told the effect is there rather than
        -- shown a gap. The wiki works no conditions out and so lists what a
        -- scope does whatever it catches, which is what this asks for anyway.
        ,("visible_when_empty", return $ return [])
        ,("picture"       , return $ return []) -- Some modifiers have custom pictures
        ]

-- | Extract the appropriate message(s) from a single statement. Note that this
-- may produce many lines (via 'ppMany'), since some statements are compound.
ppOne :: (HOI4Info g, Monad m) => StatementHandler g m
ppOne stmt@[pdx| %lhs = %rhs |] = ppOne' stmt lhs rhs
ppOne stmt@[pdx| %lhs > %rhs |] = ppOne' stmt lhs rhs
ppOne stmt@[pdx| %lhs < %rhs |] = ppOne' stmt lhs rhs
ppOne stmt = preStatement stmt
ppOne' :: (HOI4Info g, Monad m) =>
    GenericStatement
    -> Lhs lhs
    -> Rhs Void Void
    -> PPT g m IndentedMessages
ppOne' stmt lhs rhs = case lhs of
    GenericLhs label _ -> case Tr.lookup (TE.encodeUtf8 (T.toLower label)) ppHandlers of
        Just handler -> handler stmt
        -- default
        Nothing -> if isTag label
             then case rhs of
                CompoundRhs scr ->
                    withCurrentIndent $ \_ -> do -- force indent level at least 1
                        lflag <- plainMsg' . (<>"<!-- " <> label <> " -->" <>":") =<< flagText (Just HOI4Country) label
                        scriptMsgs <- scope HOI4Country $ ppMany scr
                        return (lflag : scriptMsgs)
                _ -> preStatement stmt
             else case rhs of
                CompoundRhs scr -> do
                    characters <- getCharacters
                    case HM.lookup label characters of
                        Just charid -> withCurrentIndent $ \_ -> do  -- force indent level at least 1
                            scriptMsgs <- withCurrentCharacter label $
                                            scope HOI4ScopeCharacter $ ppMany scr
                            foldCharacter (cha_loc_name charid) scriptMsgs
                        _
                            | any (`T.isSuffixOf` label) [".owner",".OWNER",".Owner"] -> withCurrentIndent $ \_ -> do -- force indent level at least 1
                                    let labelstrip
                                            | ".owner" `T.isSuffixOf` label = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".owner" label)
                                            | ".Owner" `T.isSuffixOf` label = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".Owner" label)
                                            | ".OWNER" `T.isSuffixOf` label = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".OWNER" label)
                                            | otherwise = label
                                    stateloc <-
                                        if all isDigit $ T.unpack labelstrip
                                        then getStateLoc $ read (T.unpack labelstrip)
                                        else do
                                            mstate <- eGetState (Left labelstrip)
                                            return $ fromMaybe "<!--CHECK SCRIPT-->" mstate
                                    lowner <- msgToPP' $ MsgOwnerOfSCOPE stateloc
                                    scriptMsgs <- scope HOI4Country $ ppMany scr
                                    return (lowner : scriptMsgs)
                            | otherwise -> preStatement stmt
                GenericRhs t []
                    | T.toLower t == "no"|| T.toLower t == "yes" -> do
                        scripteffect <- getScriptedEffects
                        scripttrigger <- getScriptedTriggers
                        case HM.lookup label scripteffect of
                            Just effect -> ppScriptedBlock "Scripted Effect: " label effect stmt
                            _ -> case HM.lookup label scripttrigger of
                                Just trigger -> ppScriptedBlock "Scripted Trigger: " label trigger stmt
                                _ -> preStatement stmt
                _ -> preStatement stmt
    AtLhs _ -> return [] -- don't know how to handle these
    IntLhs n -> do -- Treat as a province tag
        case rhs of
            CompoundRhs scr -> do
                        state_loc <- getStateLoc n
                        header <- msgToPP (MsgState state_loc)
                        scriptMsgs <- scope HOI4ScopeState $ ppMany scr
                        return (header ++ scriptMsgs)
            _ -> preStatement stmt
    CustomLhs _ -> preStatement stmt


-- | Try to extract one matching statement
extractStmt :: (a -> Bool) -> [a] -> (Maybe a, [a])
extractStmt p xs = extractStmt' p xs []
    where
        extractStmt' _ [] acc = (Nothing, acc)
        extractStmt' p (x:xs) acc =
            if p x then
                (Just x, acc++xs)
            else
                extractStmt' p xs (acc++[x])

-- | Predicate for matching text on the left hand side
matchLhsText :: Text -> GenericStatement -> Bool
matchLhsText t [pdx| $lhs = %_ |] | t == lhs = True
matchLhsText t [pdx| $lhs < %_ |] | t == lhs = True
matchLhsText t [pdx| $lhs > %_ |] | t == lhs = True
matchLhsText _ _ = False

-- | Predicate for matching text on boths sides
matchExactText :: Text -> Text -> GenericStatement -> Bool
matchExactText l r [pdx| $lhs = $rhs |] | l == lhs && r == T.toLower rhs = True
matchExactText _ _ _ = False
