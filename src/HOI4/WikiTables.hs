{-|
Module      : HOI4.WikiTables
Description : Static tables mapping game ids to wiki icons and pages

Pure lookup tables kept up by hand: which icon or image the wiki shows for a
script atom, and which wiki page each doctrine and national focus is written on.
-}
module HOI4.WikiTables (
        scriptIconTable
    ,   iconTerm
    ,   scriptIconFileTable
    ,   iconKey
    ,   doctrineFolders
    ,   doctrineFolderIds
    ,   agencyUpgradeBranches
    ,   focusPages
    ,   focusPageSplits
    ,   focusTagPages
    ,   expansionOfPrefix
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

-- | The expansion whose content a script file holds, named as the wiki's
-- @Expansion@ template names it, worked out from the prefix the file's name
-- starts with (@MUN_Czechoslovakia.txt@ is Peace for Our Time content).
--
-- The game ships every expansion's script in the base folders, so the file
-- name is the only thing that says which expansion added it. Each pairing
-- below is the one the expansion's own folder under @dlc/@ shows, where its
-- interface files carry the script prefix (@mun_portraits.gfx@) and its music
-- files the name the wiki uses (@music_pfot.asset@).
expansionOfPrefix :: Text -> Maybe Text
expansionOfPrefix prefix = HM.lookup (T.toLower prefix) expansionPrefixes

expansionPrefixes :: HashMap Text Text
expansionPrefixes = HM.fromList
    [ ("tfv",  "tfv")   -- Together for Victory
    , ("dod",  "dod")   -- Death or Dishonor
    , ("wtt",  "wtt")   -- Waking the Tiger
    , ("mtg",  "mtg")   -- Man the Guns
    , ("lar",  "lar")   -- La Resistance
    , ("bftb", "bftb")  -- Battle for the Bosporus
    , ("bfb",  "bftb")
    , ("nsb",  "nsb")   -- No Step Back
    , ("bba",  "bba")   -- By Blood Alone
    , ("aat",  "aat")   -- Arms Against Tyranny
    , ("toa",  "toa")   -- Trial of Allegiance
    , ("wuw",  "gtd")   -- Gotterdammerung
    , ("ww",   "gtd")
    , ("got",  "gtd")
    , ("goe",  "goe")   -- Graveyard of Empires
    , ("sea",  "ncns")  -- No Compromise, No Surrender
    , ("ncns", "ncns")
    , ("mun",  "pfot")  -- Peace for Our Time
    , ("taog", "taog")  -- Thunder at Our Gates
    ]

-- | Table of script atom -> icon key. Only ones that are different are listed.
-- This is for buildings and the like named by a script atom; buildings named by
-- the localization keep their localized name as their icon key, see
-- 'HOI4.Messages.buildingsToIcons'.
scriptIconTable :: HashMap Text Text
scriptIconTable = HM.fromList
    [("industrial_complex"  , "cic")
    ,("arms_factory"        , "mic")
    ,("dockyard"            , "nic")
    ,("air_base"            , "air base")
    ,("naval_base"          , "naval base")
    ,("coastal_bunker"      , "coastal fort")
    ,("anti_air_building"   , "static aa")
    ,("synthetic_refinery"  , "synthetic")
    ,("radar_station"       , "radar station")
    ,("rocket_site"         , "rocket site")
    ,("nuclear_reactor"     , "reactor")
    ,("bunker"              , "land fort")
    ,("supply_node"         , "supply hub")
    ,("rail_way"            , "railway")
    ,("fuel_silo"           , "fuel silo")
    ,("energy_infrastructure" , "reinforced electrical grid")
    ,("industrial_infrastructure" , "high capacity electrical grid")
    ,("naval_supply_hub"    , "naval supply hub")
    ,("naval_headquarters"  , "naval headquarters")
    ,("nuclear_facility"    , "nuclear facility")
    ,("air_facility"        , "air facility")
    ,("naval_facility"      , "naval facility")
    ,("land_facility"       , "land facility")
    -- autonomy
    ,("autonomy_dominion"   , "dominion")
    ,("autonomy_satellite"  , "satellite")
    -- ideologies. Script keeps the non-aligned under a word neither the game
    -- nor the wiki shows a reader: the files say "neutrality" where both say
    -- "Non-Aligned".
    ,("neutrality"          , "Non-Aligned")
    ]

-- | The term the wiki's icon template knows a script atom by, for the messages
-- that write the template themselves rather than going through 'icon'. An atom
-- the table says nothing about is its own term.
iconTerm :: Text -> Text
iconTerm atom = HM.findWithDefault atom atom scriptIconTable

-- | Table of script atom -> file. For things that don't have icons and should instead just
-- show an image. An empty string can be used as a short hand for just appending ".png".
scriptIconFileTable :: HashMap Text Text
scriptIconFileTable = HM.fromList
    [
    ]

-- Given a script atom, return the corresponding icon key, if any.
iconKey :: Text -> Maybe Text
iconKey atom = HM.lookup atom scriptIconTable

doctrineFolderIds :: [Text]
doctrineFolderIds = ["land", "air", "naval", "special_forces"]

-- | The doctrine folder each track, subdoctrine and grand doctrine sits under,
-- and so the page each is written about on. Script names one of these without
-- saying where in the tree it is, and the tree is spread over a set of files
-- which say little else we want; there are a hundred or so parts and they change
-- about once a game version, so they are listed here instead.
doctrineFolders :: HashMap Text Text
doctrineFolders = HM.fromList
    [   ("air_cavalry", "land")
    ,   ("air_subdoctrine_aerial_reconnaissance", "air")
    ,   ("air_subdoctrine_bomber_interception", "air")
    ,   ("air_subdoctrine_carpet_bombing", "air")
    ,   ("air_subdoctrine_carrier_strikes", "air")
    ,   ("air_subdoctrine_coastal_air_patrol", "air")
    ,   ("air_subdoctrine_deep_air_raids", "air")
    ,   ("air_subdoctrine_dive_bombers", "air")
    ,   ("air_subdoctrine_dogfighting_mastery", "air")
    ,   ("air_subdoctrine_escort_fighter", "air")
    ,   ("air_subdoctrine_fighter_bombers", "air")
    ,   ("air_subdoctrine_fighter_central_field", "air")
    ,   ("air_subdoctrine_fighter_homeland_defense", "air")
    ,   ("air_subdoctrine_flexible_fire_support", "air")
    ,   ("air_subdoctrine_flying_artillery", "air")
    ,   ("air_subdoctrine_flying_fortresses", "air")
    ,   ("air_subdoctrine_heavy_aircraft_focus", "air")
    ,   ("air_subdoctrine_long_range_escort", "air")
    ,   ("air_subdoctrine_naval_strike_tactics", "air")
    ,   ("air_subdoctrine_naval_torpedo_tactics", "air")
    ,   ("air_subdoctrine_night_bombing", "air")
    ,   ("air_subdoctrine_open_ocean_air_patrol", "air")
    ,   ("air_subdoctrine_operational_air_support", "air")
    ,   ("air_subdoctrine_tactical_battlefield_support", "air")
    ,   ("air_subdoctrine_tactical_flexibility", "air")
    ,   ("air_subdoctrine_theater_interdiction", "air")
    ,   ("anti_aircraft_cruisers", "naval")
    ,   ("anti_tank_frontline", "land")
    ,   ("armor", "land")
    ,   ("armored_cavalry", "land")
    ,   ("armored_cavalry_no_lar", "land")
    ,   ("armored_infantry_support", "land")
    ,   ("armored_raiders", "naval")
    ,   ("armored_spearhead", "land")
    ,   ("armored_spearhead_no_lar", "land")
    ,   ("assault_infantry", "land")
    ,   ("battlecruiser_supremacy", "naval")
    ,   ("battleship_antiair_screen", "naval")
    ,   ("broad_naval_support", "naval")
    ,   ("capital_hunters", "naval")
    ,   ("capital_ships", "naval")
    ,   ("carrier_battlegroups", "naval")
    ,   ("carriers", "naval")
    ,   ("coastal_defence_fleet", "naval")
    ,   ("coastal_minelaying", "naval")
    ,   ("combat_support", "land")
    ,   ("commandos", "land")
    ,   ("convoy_escort", "naval")
    ,   ("deep_battle", "land")
    ,   ("deep_battle_no_lar", "land")
    ,   ("defensive_postures", "land")
    ,   ("dispersed_operations", "land")
    ,   ("escort_carrier_support", "naval")
    ,   ("expeditionary_warfare", "land")
    ,   ("field_engineering", "land")
    ,   ("fighter_aircraft", "air")
    ,   ("fire_concentration", "land")
    ,   ("floating_airfields", "naval")
    ,   ("flying_batteries", "land")
    ,   ("grand_assault", "land")
    ,   ("grand_battleplan", "land")
    ,   ("great_war_infantry", "land")
    ,   ("guerilla_war", "land")
    ,   ("heavy_aircraft", "air")
    ,   ("hunter_killers", "naval")
    ,   ("infantry", "land")
    ,   ("infiltration_tactics", "land")
    ,   ("irregulars", "land")
    ,   ("jeune_ecole", "naval")
    ,   ("large_unit_tactics", "land")
    ,   ("last_stand", "land")
    ,   ("light_task_forces", "naval")
    ,   ("line_of_battle", "naval")
    ,   ("long_range_submarines", "naval")
    ,   ("marines_1", "special_forces")
    ,   ("marines_2", "special_forces")
    ,   ("mass_assault", "land")
    ,   ("massed_carrier_fleet", "naval")
    ,   ("medium_aircraft", "air")
    ,   ("mission_type_tactics", "land")
    ,   ("mobile_defense", "land")
    ,   ("mobile_infantry", "land")
    ,   ("mobile_recon_and_assault", "land")
    ,   ("mobile_recon_and_assault_no_lar", "land")
    ,   ("mountaineers_1", "special_forces")
    ,   ("mountaineers_2", "special_forces")
    ,   ("mounted_infantry", "land")
    ,   ("naval_gunfire_support", "naval")
    ,   ("new_base_strike", "naval")
    ,   ("new_battlefield_support", "air")
    ,   ("new_convoy_raiding", "naval")
    ,   ("new_fleet_in_being", "naval")
    ,   ("new_mobile_warfare", "land")
    ,   ("new_operational_integrity", "air")
    ,   ("new_strategic_destruction", "air")
    ,   ("operations", "land")
    ,   ("paratroopers_1", "special_forces")
    ,   ("paratroopers_2", "special_forces")
    ,   ("patrol_boats", "naval")
    ,   ("peoples_war", "land")
    ,   ("rangers_1", "special_forces")
    ,   ("rangers_2", "special_forces")
    ,   ("rapid_domination", "land")
    ,   ("screen_support_focus", "naval")
    ,   ("screens", "naval")
    ,   ("self_propelled_support", "land")
    ,   ("siege_artillery", "land")
    ,   ("special_forces_first", "special_forces")
    ,   ("special_forces_quality", "special_forces")
    ,   ("special_forces_quantity", "special_forces")
    ,   ("special_forces_second", "special_forces")
    ,   ("streamlined_deployment", "land")
    ,   ("strike_aircraft", "air")
    ,   ("submarine_coastal_defense", "naval")
    ,   ("submarine_fleet_operations", "naval")
    ,   ("submarines", "naval")
    ,   ("superior_firepower", "land")
    ,   ("support_integrated_operations", "naval")
    ,   ("tank_destroyer_force", "land")
    ,   ("torpedo_primacy", "naval")
    ,   ("wolfpacks", "naval")
    ]

-- | The branch of the agency each upgrade belongs to, and so the heading each
-- is written under.
agencyUpgradeBranches :: HashMap Text Text
agencyUpgradeBranches = HM.fromList
    [ ("upgrade_economy_civilian"       , "branch_intelligence")
    , ("upgrade_army_department"        , "branch_intelligence")
    , ("upgrade_naval_department"       , "branch_intelligence")
    , ("upgrade_airforce_department"    , "branch_intelligence")
    , ("upgrade_passive_defense"        , "branch_defense")
    , ("upgrade_anti_partisan"          , "branch_defense")
    , ("upgrade_blueprint_stealing"     , "branch_operation")
    , ("upgrade_portable_radios"        , "branch_operation")
    , ("upgrade_invisible_ink"          , "branch_operation")
    , ("upgrade_plastic_explosives"     , "branch_operation")
    , ("upgrade_suicide_pills"          , "branch_operation")
    , ("upgrade_training_centers"       , "branch_operative")
    , ("upgrade_commando_training"      , "branch_operative")
    , ("upgrade_interrogation_techniques", "branch_operative")
    , ("upgrade_diplo_training"         , "branch_operative")
    , ("upgrade_psycho_warfare"         , "branch_operative")
    , ("upgrade_form_department"        , "branch_crypto")
    , ("upgrade_decryption_boost"       , "branch_crypto")
    , ("upgrade_crypto_strength"        , "branch_crypto")
    ]

-- | The wiki page each focus file is written up on, keyed on the name of the file.
focusPages :: HashMap Text Text
focusPages = HM.fromList
    [ ("generic.txt"                            , "generic")
    , ("horn_of_africa.txt"                     , "hoa")
    , ("australia.txt"                          , "ast")
    , ("australia_taog.txt"                     , "ast2")
    , ("india.txt"                              , "raj")
    , ("india_goe.txt"                          , "raj2")
    , ("canada.txt"                             , "can")
    , ("new_zealand.txt"                        , "nzl")
    , ("south_africa.txt"                       , "saf")
    , ("czechoslovakia.txt"                     , "cze")
    , ("czechoslovakia_mu.txt"                  , "cze2")
    , ("hungary.txt"                            , "hun")
    , ("hungary_wuw.txt"                        , "hun2")
    , ("habsburg_joint.txt"                     , "habsburg")
    , ("romania.txt"                            , "rom")
    , ("yugoslavia.txt"                         , "yug")
    , ("japan.txt"                              , "jap")
    , ("usa.txt"                                , "usa")
    , ("uk.txt"                                 , "eng")
    , ("netherlands.txt"                        , "hol")
    , ("mexico.txt"                             , "mex")
    , ("france.txt"                             , "fra")
    , ("free_france.txt"                        , "fre")
    , ("vichy_france.txt"                       , "vic")
    , ("portugal.txt"                           , "por")
    , ("bulgaria.txt"                           , "bul")
    , ("greece.txt"                             , "gre")
    , ("turkey.txt"                             , "tur")
    , ("poland.txt"                             , "pol")
    , ("baltic_shared.txt"                      , "baltic")
    , ("estonia.txt"                            , "est")
    , ("latvia.txt"                             , "lat")
    , ("lithuania.txt"                          , "lit")
    , ("italy.txt"                              , "ita")
    , ("ethiopia.txt"                           , "eth")
    , ("switzerland.txt"                        , "swi")
    , ("austria.txt"                            , "aus")
    , ("belgium.txt"                            , "bel")
    , ("congo.txt"                              , "cog")
    , ("congo_shared.txt"                       , "belcog")
    , ("denmark.txt"                            , "den")
    , ("finland.txt"                            , "fin")
    , ("iceland.txt"                            , "ice")
    , ("norway.txt"                             , "nor")
    , ("sweden.txt"                             , "swe")
    , ("nordic_shared.txt"                      , "nordic")
    , ("argentina.txt"                          , "arg")
    , ("brazil.txt"                             , "bra")
    , ("chile.txt"                              , "chl")
    , ("paraguay.txt"                           , "par")
    , ("uruguay.txt"                            , "urg")
    , ("paraguay_uruguay_shared_branch.txt"     , "guay")
    , ("toa_shared_military_branch.txt"         , "smb")
    , ("afghanistan.txt"                        , "afg")
    , ("iraq.txt"                               , "irq")
    , ("persia.txt"                             , "per")
    , ("goe_shared_saadabad_branch.txt"         , "ssb")
    , ("philippines.txt"                        , "phi")
    , ("indonesia.txt"                          , "ins")
    , ("indonesia_joint.txt"                    , "ins_join")
    , ("siam.txt"                               , "sia")
    , ("abdacom_shared_branch.txt"              , "abdacom")
    , ("austro_hungarian_releasable_shared.txt" , "slo")
    , ("china_shared.txt"                       , "chishared")
    , ("china_warlord.txt"                      , "warlord")
    , ("china_shared_tsr.txt"                   , "chishared2")
    , ("china_warlord_sea.txt"                  , "warlord2")
    , ("china_nationalist.txt"                  , "chi")
    , ("china_nationalist_sea.txt"              , "chi2")
    , ("china_communist.txt"                    , "prc")
    , ("china_communist_sea.txt"                , "prc2")
    , ("china_nationalist_warlord_tsr.txt"      , "warlordchi")
    , ("china_communist_warlord_tsr.txt"        , "warlordprc")
    , ("tsr_lingguang_incident_joint_branch.txt", "lingg")
    , ("ncns_ma_clique_joint_branch.txt"        , "ma_clique")
    , ("manchukuo.txt"                          , "man")
    , ("manchukuo_tsr.txt"                      , "man2")
    ]

-- | A file the wiki writes up over two pages, with the focus the second page
-- opens with. Naming the focus rather than a line or a count keeps the split in
-- the right place when the game adds focuses to the first half.
focusPageSplits :: HashMap Text (Text, Text, Text)
focusPageSplits = HM.fromList
    [ ("germany.txt", ("gerh", "GER_oppose_hitler_ww", "gero"))
    , ("soviet.txt" , ("sovi", "SOV_the_path_of_marxism_leninism", "sovp"))
    ]

-- | Where the two halves of a file are not written one after the other, the tag
-- an id opens with tells them apart. Spain's two sides share a file this way.
focusTagPages :: [(Text, Text)]
focusTagPages =
    [ ("SPR_", "spr")
    , ("SPA_", "spa")
    ]
