{-|
Module      : HOI4.WikiTables
Description : Static tables mapping game ids to wiki icons and pages

Pure lookup tables kept up by hand: which icon or image the wiki shows for a
script atom, and which wiki page each doctrine and national focus is written on.
-}
module HOI4.WikiTables (
        scriptIconTable
    ,   buildingIconTable
    ,   iconTerm
    ,   scriptIconFileTable
    ,   iconKey
    ,   doctrineFolders
    ,   doctrineFolderIds
    ,   agencyUpgradeBranches
    ,   focusPages
    ,   focusPageSplits
    ,   focusTagPages
    ,   focusPage
    ,   focusPageTag
    ,   focusModuleOf
    ,   focusModuleIds
    ,   focusSuffix
    ,   focusSuffixes
    ,   expansionOfPrefix
    ,   tagAliases
    ) where

import Control.Applicative ((<|>))

import Data.Char (chr)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import HOI4.Types (HOI4NationalFocus (..))

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

-- | The key the wiki's icon template knows each building by, for the buildings
-- whose script name is not that key. The game's building list comes from its
-- files ('HOI4.Misc.parseHOI4Buildings'); this table only says how the wiki
-- draws each, which is nothing the game files can tell us, so it has to be kept
-- by hand. 'HOI4.Settings.checkBuildingIcons' warns when the two drift apart:
-- a building the game defines that has no entry and whose name the template
-- cannot know, or an entry for a building the game no longer defines.
--
-- Buildings named by the localization keep their localized name as their icon
-- key, see 'HOI4.Messages.buildingsToIcons'. The landmarks have no table
-- entry: they share one icon, see 'HOI4.Localization.buildingIcon'.
buildingIconTable :: HashMap Text Text
buildingIconTable = HM.fromList
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
    ,("nuclear_reactor_heavy_water" , "hw reactor")
    ,("commercial_nuclear_reactor" , "civilian nuclear reactor")
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
    ,("stronghold_network"  , "stronghold network")
    ,("mega_gun_emplacement" , "mega gun emplacement")
    -- The three dams are one building to the wiki, and the two canals are
    -- their own.
    ,("dam_mountain"        , "dam")
    ,("cataract_dam_mountain" , "dam")
    ,("canal_kiel"          , "kiel canal locks")
    ,("canal_panama"        , "panama canal locks")
    ]

-- | Table of script atom -> icon key. Only ones that are different are listed.
-- This is for buildings and the like named by a script atom.
scriptIconTable :: HashMap Text Text
scriptIconTable = HM.union buildingIconTable $ HM.fromList
    [
    -- autonomy
     ("autonomy_dominion"   , "dominion")
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
    , ("indonesia_joint.txt"                    , "inshol")
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

-- | The wiki page a focus is written up on. Which file script keeps a focus in
-- says which page it belongs to, bar three files the wiki writes up over two
-- pages each: Spain's two sides are told apart by the tag their ids carry, and
-- Germany's and the Soviet Union's halves each run in one stretch, so the focus
-- the second half opens with says where the break falls.
focusPage :: HashMap Text HOI4NationalFocus -> HOI4NationalFocus -> Maybe Text
focusPage focuses nf = byTag <|> bySplit <|> HM.lookup file focusPages
    where
        file = T.toLower (T.takeWhileEnd (\c -> c /= '/' && c /= chr 92) (T.pack (nf_path nf)))
        byTag = listToMaybe
            [ page | (tag, page) <- focusTagPages, tag `T.isPrefixOf` nf_id nf ]
        bySplit = do
            (before, marker, from) <- HM.lookup file focusPageSplits
            split <- HM.lookup marker focuses
            return (if nf_ordinal nf >= nf_ordinal split then from else before)

-- | The tag the wiki's @{{Focus}}@ template looks a page's focuses up under. A
-- page is its own tag, bar the two trees the wiki writes over two pages each:
-- both halves of Germany's are looked up as @ger@ and both of the Soviet
-- Union's as @sov@, each half keeping its own page.
focusPageTag :: Text -> Text
focusPageTag page = HM.findWithDefault page page focusPageTags

focusPageTags :: HashMap Text Text
focusPageTags = HM.fromList
    [ ("gerh", "ger")
    , ("gero", "ger")
    , ("sovi", "sov")
    , ("sovp", "sov")
    ]

-- | The @Module:Focus/<expansion>@ submodule a tag's focuses are written in,
-- one submodule per expansion.
--
-- This is the wiki's own @cfg.tags.tagset@, from @Module:Focus/config@. Nothing
-- in the game files says which submodule the wiki files a tree under, so the
-- pairing is kept by hand: a new expansion adds its tags here and its own id to
-- 'focusModuleIds'.
focusModuleOf :: Text -> Maybe Text
focusModuleOf tag = HM.lookup tag focusModules

-- | The submodules, in the order the wiki's config lists them.
focusModuleIds :: [Text]
focusModuleIds =
    [ "generic", "tfv", "dod", "wtt", "mtg", "lar", "bftb", "nsb"
    , "bba", "aat", "toa", "gtd", "goe", "ncns", "pfot", "taog"
    ]

focusModules :: HashMap Text Text
focusModules = HM.fromList
    [ ("generic", "generic"), ("warlord", "generic"), ("warlord2", "generic")
    , ("hoa", "generic")
    , ("ast", "tfv"), ("can", "tfv"), ("nzl", "tfv"), ("raj", "tfv"), ("saf", "tfv")
    , ("cze", "dod"), ("hun", "dod"), ("rom", "dod"), ("yug", "dod")
    , ("ger", "wtt"), ("jap", "wtt"), ("chi", "wtt"), ("prc", "wtt")
    , ("man", "wtt"), ("chishared", "wtt")
    , ("usa", "mtg"), ("eng", "mtg"), ("hol", "mtg"), ("mex", "mtg")
    , ("fra", "lar"), ("por", "lar"), ("spr", "lar"), ("spa", "lar")
    , ("vic", "lar"), ("fre", "lar")
    , ("bul", "bftb"), ("gre", "bftb"), ("tur", "bftb")
    , ("sov", "nsb"), ("pol", "nsb"), ("est", "nsb"), ("lat", "nsb")
    , ("lit", "nsb"), ("baltic", "nsb")
    , ("ita", "bba"), ("eth", "bba"), ("swi", "bba")
    , ("den", "aat"), ("fin", "aat"), ("ice", "aat"), ("nor", "aat")
    , ("swe", "aat"), ("nordic", "aat")
    , ("arg", "toa"), ("bra", "toa"), ("chl", "toa"), ("par", "toa")
    , ("urg", "toa"), ("smb", "toa"), ("guay", "toa")
    , ("aus", "gtd"), ("bel", "gtd"), ("cog", "gtd"), ("hun2", "gtd")
    , ("belcog", "gtd"), ("habsburg", "gtd")
    , ("afg", "goe"), ("irq", "goe"), ("per", "goe"), ("raj2", "goe")
    , ("ssb", "goe")
    , ("phi", "ncns"), ("chishared2", "ncns"), ("chi2", "ncns"), ("man2", "ncns")
    , ("prc2", "ncns"), ("warlordchi", "ncns"), ("warlordprc", "ncns")
    , ("lingg", "ncns"), ("ma_clique", "ncns")
    , ("cze2", "pfot")
    , ("ast2", "taog"), ("ins", "taog"), ("sia", "taog"), ("inshol", "taog")
    , ("abdacom", "taog")
    ]

-- | The letters the wiki tells two focuses of one tag apart by, where the game
-- gives both the same name. The suffix goes on the end of the link and of the
-- anchor the link lands on, so "Intervention in Spain CD" and "Intervention in
-- Spain H" are two rows a reader can be sent to separately.
--
-- Which letters those are is the wiki's own choice and nothing the game files
-- say, so they are kept here, keyed on the focus id in lower case, the way the
-- wiki's modules key their entries. 'HOI4.FocusModules.writeHOI4FocusModules'
-- warns when the game gives two focuses of a tag the same name and this table
-- tells them apart in neither, and when an entry names a focus the game no
-- longer has.
focusSuffix :: Text -> Maybe Text
focusSuffix theid = HM.lookup (T.toLower theid) focusSuffixes

focusSuffixes :: HashMap Text Text
focusSuffixes = HM.fromList
    -- Germany's two opposition trees, the base one and Gotterdammerung's.
    [ ("ger_prepare_for_the_next_blockade_ww", "WW")
    , ("ger_rebuild_the_nation_ww", "WW")
    , ("ger_fan_prussian_militarism", "WW")
    , ("ger_revive_the_kaiserreich_ww", "WW")
    , ("ger_re_establish_free_elections_ww", "WW")
    , ("ger_reverse_the_brain_drain_ww", "WW")
    , ("ger_shared_rd_programs_ww", "WW")
    , ("ger_pool_technical_know_how_ww", "WW")
    , ("ger_the_mannheim_project_ww", "WW")
    , ("ger_see_to_the_eastern_front_ww", "WW")
    , ("ger_safeguard_the_baltic_ww", "WW")
    , ("ger_danzig_for_guarantees_ww", "WW")
    , ("ger_support_the_finns_ww", "WW")
    , ("ger_carte_blanche_for_alsace_and_french_colonies_ww", "WW")
    , ("ger_bypass_maginot_in_the_south_ww", "WW")
    , ("ger_reinstate_imperial_possessions_ww", "WW")
    , ("ger_rebuild_the_high_seas_fleet_ww", "WW")
    , ("ger_break_anglo_french_colonial_hegemony_ww", "WW")
    , ("ger_our_place_in_the_sun_ww", "WW")
    , ("ger_schlieffen_once_more_ww", "WW")
    , ("ger_prepare_italian_coup_ww", "WW")
    , ("ger_assassinate_mussolini_ww", "WW")
    , ("ger_rekindle_imperial_sentiment_ww", "WW")
    , ("ger_expatriate_the_communists_ww", "WW")
    , ("ger_accept_british_naval_dominance_ww", "WW")
    , ("ger_revive_the_kaiserreich", "B")
    , ("ger_rebuild_the_nation", "B")
    , ("ger_fan_the_prussian_militarism", "B")
    , ("ger_rebuild_the_high_seas_fleet", "B")
    , ("ger_our_place_in_the_sun", "B")
    , ("ger_prepare_for_the_next_blockade", "B")
    , ("ger_break_the_anglo_french_colonial_hegemony", "B")
    , ("ger_schlieffen_once_more", "B")
    , ("ger_prepare_italian_coup", "B")
    , ("ger_assassinate_mussolini", "B")
    , ("ger_rekindle_imperial_sentiment", "B")
    , ("ger_expatriate_the_communists", "B")
    , ("ger_accept_british_naval_dominance", "B")
    , ("ger_carte_blanche_for_alsace_and_french_colonies", "B")
    , ("ger_bypass_maginot_in_the_south", "B")
    , ("ger_reinstate_imperial_possessions", "B")
    , ("ger_see_to_the_eastern_front", "B")
    , ("ger_danzig_for_guarantees", "B")
    , ("ger_safeguard_the_baltic", "B")
    , ("ger_support_the_finns", "B")
    , ("ger_reestablish_free_elections", "B")
    , ("ger_the_monarchy_compromise", "B")
    , ("ger_reverse_the_brain_drain", "B")
    , ("ger_shared_rd_programs", "B")
    , ("ger_the_mannheim_project", "B")
    , ("ger_pool_technical_know_how", "B")
    -- Japan's two trees, the base one and No Compromise, No Surrender's.
    , ("jap_the_fate_of_the_imperial_family", "b")
    , ("jap_finish_the_fight", "b")
    , ("jap_nationalize_the_zaibatsus", "b")
    , ("jap_establish_the_northern_resource_area", "b")
    , ("jap_national_defense_state", "b")
    , ("jap_rekindle_the_old_alliance", "b")
    , ("jap_sea_national_defense_state", "ncns")
    , ("jap_sea_establish_the_northern_resource_area", "ncns")
    , ("jap_sea_fate_of_the_imperial_family", "ncns")
    , ("jap_sea_nationalize_the_zaibatsus", "c")
    , ("jap_democratic_war_with_manchukuo", "ncns")
    , ("jap_sea_rekindle_the_old_alliance", "ncns")
    , ("jap_democratic_nationalize_zaibatsus", "d")
    -- The Soviet Union's two halves, and Poland's several governments.
    , ("sov_transformation_of_nature", "C")
    , ("sov_transformation_of_nature_alt", "ALT")
    , ("sov_organize_the_wreckers", "C")
    , ("sov_organize_wreckers", "P")
    , ("pol_pan_slavic_revanchism", "SR")
    , ("pol_join_allies", "SC")
    , ("pol_demand_lit_pavel", "P")
    , ("pol_demand_slovakia_pavel", "P")
    , ("pol_assert_eastern_claims_pavel", "P")
    , ("pol_pan_slavism", "R")
    , ("pol_demand_lit", "F")
    , ("pol_assert_eastern_claims", "F")
    , ("lit_claim_livonia", "S")
    , ("lit_claim_livonia_monarchy", "M")
    -- Italy's two rump states, and Ethiopia's two brigades.
    , ("ita_independence_rds", "RDS")
    , ("ita_independence_rsi", "RSI")
    , ("eth_international_brigades", "M")
    , ("eth_international_brigades_communist", "C")
    -- The Raj's two trees, and Iran's two coastal defences.
    , ("raj_education_efforts", "1")
    , ("raj_education_efforts_2", "2")
    , ("per_coastal_defense_initiative", "N")
    -- The Chinese warlords' two trees.
    , ("chi_sea_anti_communism", "N")
    , ("chi_tsr_anti_communism", "RG")
    -- Austria's two interventions in Spain.
    , ("aus_intervention_in_spain", "CD")
    , ("aus_spanish_intervention", "H")
    -- The rest the wiki has not told apart. Only the second of each pair is
    -- named, so that the focus the wiki already links by name keeps the name it
    -- is linked by and only the one that was unreachable gains a suffix.
    --
    -- The Dutch colonial focuses Thunder at Our Gates writes a second time.
    , ("hol_a_western_capital_taog", "TAOG")
    , ("hol_antilles_defenses_taog", "TAOG")
    , ("hol_colonial_shipbuilding_taog", "TAOG")
    , ("hol_continue_the_war_in_batavia_taog", "TAOG")
    , ("hol_curtail_colonial_autonomy_taog", "TAOG")
    , ("hol_expand_curacao_oil_refineries_taog", "TAOG")
    , ("hol_expand_the_colonial_army_taog", "TAOG")
    , ("hol_liberation_taog", "TAOG")
    , ("hol_obtain_foreign_colonial_investments_taog", "TAOG")
    , ("hol_open_second_paranam_bauxite_mine_taog", "TAOG")
    , ("hol_pre_empt_venezuelan_aggression_taog", "TAOG")
    , ("hol_prepare_for_our_return_taog", "TAOG")
    , ("hol_the_east_indies_war_machine_taog", "TAOG")
    , ("hol_the_western_possessions_taog", "TAOG")
    -- Poland's cryptography branch, as it stands without Man the Guns.
    , ("pol_expand_polish_intelligence_no_mtg", "noMTG")
    , ("pol_the_bombe_no_mtg", "noMTG")
    , ("pol_the_cyclometer_no_mtg", "noMTG")
    , ("pol_the_long_push_home_no_mtg", "noMTG")
    , ("pol_niech_zyje_opor_no_mtg", "noMTG")
    -- The Soviet Union's second glory of the Red Army.
    , ("sov_the_glory_of_the_red_army_alt", "ALT")
    -- The continuous focus a tree focus of the same name shares a tag with. Its
    -- link lands on the continuous focus page, which is written by hand, so the
    -- row there has to be given this anchor for the link to find it.
    , ("continuous_tech_share", "C")
    ]

-- | Country tag aliases and the wiki text each stands for. An alias, defined
-- in @common/country_tag_aliases@, is a three-letter tag that is not a country
-- of its own: the game resolves it when the script runs, by trigger (VIC is
-- France while she runs the Vichy focus tree), by best score (SOU is whichever
-- Soviet successor exists, Stalin's first), or by reading a variable (SB1 is
-- whoever Switzerland is appeasing). There being no country behind the tag,
-- there is no flag or localization to fall back on, so what the alias means is
-- written out by hand here -- one entry per alias in the game's file, in its
-- order. The text is finished wikitext: a flag template where the wiki has a
-- page for the resolved country, a base flag with a qualifier where it only
-- has the parent country's, and a phrase for the variable-driven ones.
tagAliases :: HashMap Text Text
tagAliases = HM.fromList
    [ ("SPA", "{{flag|Nationalist Spain}}")
    , ("SPB", "{{flag|Carlist Spain}}")
    , ("SPC", "{{flag|Anarchist Spain}}")
    , ("SPD", "{{flag|Republican Spain}}")
    , ("VIC", "{{flag|Vichy France}}")
    , ("BUZ", "{{flag|Bulgaria}} (Zveno government)")
    , ("BUF", "{{flag|Bulgaria}} (Fatherland Front)")
    , ("SOS", "{{flag|Soviet Union}} (Stalinist)")
    , ("SOT", "{{flag|Soviet Union}} (left opposition)")
    , ("SOB", "{{flag|Soviet Union}} (right opposition)")
    , ("SOP", "{{flag|Soviet Union}} (provisional government)")
    -- Whichever Soviet successor exists, Stalin's Soviet Union first.
    , ("SOU", "{{flag|Soviet Union}}")
    , ("RSI", "{{flag|Italian Social Republic}}")
    , ("RDS", "{{flag|Kingdom of Italy}} (Regno del Sud)")
    , ("SB1", "the country named in <tt>SWI.SWI_country_to_appease_1</tt>")
    , ("SB2", "the country named in <tt>SWI.SWI_country_to_appease_2</tt>")
    , ("SB3", "the country named in <tt>SWI.SWI_country_to_appease_3</tt>")
    , ("SB4", "the country named in <tt>SWI.SWI_country_to_appease_4</tt>")
    , ("FNO", "{{flag|Norway}} (fascist)")
    , ("FGR", "{{flag|German Reich}}")
    -- Reads @generic_operation_target@, set weekly for each major with an
    -- intelligence agency.
    , ("MOT", "the operation's target country")
    ]
