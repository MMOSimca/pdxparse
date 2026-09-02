{-|
Module      : HOI4.Types
Description : Types specific to Hearts of Iron IV
-}
module HOI4.Types (
        -- * Parser state
        HOI4Data (..), HOI4State (..)
    ,   HOI4Info (..)
        -- * Features
    ,   HOI4EvtTitle (..), HOI4EvtDesc (..), HOI4Event (..), HOI4Option (..)
    ,   HOI4Source (..), HOI4SourceWeight
    ,   HOI4EventTriggers, HOI4EventWeight
    ,   HOI4Decision (..), HOI4DecisionCost(..), HOI4DecisionDays(..), HOI4DecisionIcon(..), HOI4Decisioncat (..)
    ,   HOI4DecisionTriggers, HOI4DecisionWeight
    ,   HOI4Idea (..)
    ,   HOI4OpinionModifier (..)
    ,   HOI4DynamicModifier (..)
    ,   HOI4Modifier (..)
    ,   HOI4NationalFocus (..)

    ,   HOI4CountryHistory (..)
    ,   HOI4ScriptedLocText (..)
    ,   HOI4Character (..), HOI4Advisor (..)
    ,   HOI4CountryLeaderTrait (..)
    ,   HOI4UnitLeaderTrait (..)
    ,   HOI4BopRange (..)
    ,   HOI4Technology (..)
    ,   HOI4Building (..)
        -- * Low level types
    ,   HOI4Scope (..)
    ,   HOI4ScopeVal (..)
    ,   scopeValType, scopeValTag
    ,   allowedSoleTag, decTakerTag, catTakerTag, decTargetIdent
    ,   withDecisionIdents, withCategoryIdents, withFocusIdents
    ,   eventFirerTag, sourceFirerTag
    ,   AIWillDo (..)
    ,   AIModifier (..)
    ,   aiWillDo
    ) where

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)

import Abstract -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings
                     , IsGame (..), IsGameData (..), IsGameState (..))
import HOI4.Messages (ScriptMessage, ModifierDisplay)
--import Doc

--------------------------------------------
-- Types used by toplevel Settings module --
--------------------------------------------

-- | Settings, raw scripts, and parsed scripts.
data HOI4Data = HOI4Data {
        hoi4settings :: Settings
    ,   hoi4events :: HashMap Text HOI4Event
    ,   hoi4decisioncats :: HashMap Text HOI4Decisioncat
    ,   hoi4decisions :: HashMap Text HOI4Decision
    ,   hoi4ideas :: HashMap Text HOI4Idea
    ,   hoi4opmods :: HashMap Text HOI4OpinionModifier
    ,   hoi4eventTriggers :: HOI4EventTriggers
    ,   hoi4decisionTriggers :: HOI4DecisionTriggers
    ,   hoi4dynamicmodifiers :: HashMap Text HOI4DynamicModifier
    ,   hoi4modifiers :: HashMap Text HOI4Modifier
    ,   hoi4nationalfocusScripts :: HashMap FilePath GenericScript
    ,   hoi4nationalfocus :: HashMap Text HOI4NationalFocus
    ,   hoi4countryHistory :: HashMap Text HOI4CountryHistory
    ,   hoi4initialvariables :: HashMap Text Double -- ^ variable -> value it starts the game with
    ,   hoi4eventScripts :: HashMap FilePath GenericScript
    ,   hoi4decisioncatScripts :: HashMap FilePath GenericScript
    ,   hoi4decisionScripts :: HashMap FilePath GenericScript
    ,   hoi4ideaScripts :: HashMap FilePath GenericScript
    ,   hoi4opmodScripts :: HashMap FilePath GenericScript
    ,   hoi4onactionsScripts :: HashMap FilePath GenericScript
    ,   hoi4dynamicmodifierScripts :: HashMap FilePath GenericScript
    ,   hoi4modifierScripts :: HashMap FilePath GenericScript

    ,   hoi4countryHistoryScripts :: HashMap FilePath GenericScript -- Country Tag -> country tag + ideology
    ,   hoi4characterScripts :: HashMap FilePath GenericScript
    ,   hoi4characters :: HashMap Text HOI4Character
    ,   hoi4countryleadertraitScripts :: HashMap FilePath GenericScript
    ,   hoi4countryleadertraits :: HashMap Text HOI4CountryLeaderTrait
    ,   hoi4unitleadertraitScripts :: HashMap FilePath GenericScript
    ,   hoi4unitleadertraits :: HashMap Text HOI4UnitLeaderTrait
    ,   hoi4terrainScripts :: HashMap FilePath GenericScript
    ,   hoi4terrain :: [Text]
    ,   hoi4unittagScripts :: HashMap FilePath GenericScript
    ,   hoi4unittag :: [Text]
    ,   hoi4unitScripts :: HashMap FilePath GenericScript
    ,   hoi4unit :: [Text]
    ,   hoi4ideologyScripts :: HashMap FilePath GenericScript
    ,   hoi4ideology :: HashMap Text Text
    ,   hoi4chartoken :: HashMap Text HOI4Advisor
    ,   hoi4scriptedeffectScripts :: HashMap FilePath GenericScript
    ,   hoi4scriptedeffects :: HashMap Text GenericStatement
    ,   hoi4scriptedtriggerScripts :: HashMap FilePath GenericScript
    ,   hoi4scriptedtriggers :: HashMap Text GenericStatement
    ,   hoi4modifierdefinitionScripts :: HashMap FilePath GenericScript
    ,   hoi4modifierdefinitions :: HashMap Text ModifierDisplay
    ,   hoi4bopScripts :: HashMap FilePath GenericScript
    ,   hoi4bops :: HashMap Text HOI4BopRange
    ,   hoi4techScripts :: HashMap FilePath GenericScript
    ,   hoi4techs :: HashMap FilePath [HOI4Technology]
    ,   hoi4buildingScripts :: HashMap FilePath GenericScript
    ,   hoi4buildings :: HashMap Text HOI4Building
    ,   hoi4mioScripts :: HashMap FilePath GenericScript
    ,   hoi4mionames :: HashMap Text Text -- ^ MIO, trait and policy token -> name key
    ,   hoi4mioincludes :: HashMap Text Text -- ^ MIO token -> archetype it is built from
    ,   hoi4scriptedlocScripts :: HashMap FilePath GenericScript
    ,   hoi4scriptedloc :: HashMap Text [HOI4ScriptedLocText] -- ^ name -> the
                                 --   texts it can come to, in the order the game
                                 --   tries them
    ,   hoi4scriptconstantScripts :: HashMap FilePath GenericScript
    ,   hoi4scriptconstants :: HashMap Text Double -- ^ dotted path -> value
    ,   hoi4extraScripts :: HashMap String (HashMap FilePath GenericScript)
            -- ^ Scripts read only to be searched for fired events and
            --   activated decisions (special projects, operations, raids,
            --   resistance/compliance modifiers), keyed on category
    ,   hoi4lockeys :: [Text]
    ,   hoi4modkeys :: [Text]
    }

-- | State type for HOI4.
data HOI4State = HOI4State {
        hoi4scopeStack :: [HOI4Scope]
    ,   hoi4currentFile :: Maybe FilePath
    ,   hoi4currentIndent :: Maybe Int
    ,   hoi4IsInEffect :: Bool
    -- | Scripted effects and triggers whose bodies are being written out around
    -- the statement in hand, innermost first, so that one that comes round to
    -- invoking itself can be cut off.
    ,   hoi4expandedBlocks :: [Text]
    -- | The character a statement is about, where the script it stands in has
    -- scoped to one by name.
    ,   hoi4currentCharacter :: Maybe Text
    -- | What @ROOT@ stands for in the script being written out, where known.
    ,   hoi4rootIdent :: Maybe HOI4ScopeVal
    -- | What @FROM@ stands for in the script being written out, where known.
    ,   hoi4fromIdent :: Maybe HOI4ScopeVal
    -- | What each scope on 'hoi4scopeStack' stands for, aligned with it:
    -- pushing a scope pushes 'Nothing' here, and the handlers that scope to
    -- something they can name fill the top in.
    ,   hoi4identStack :: [Maybe HOI4ScopeVal]
    } deriving (Show)

-- | Interface for HOI4 feature handlers. Most of the methods just get data
-- tables from the parser state. These are empty until the relevant parsing
-- stages have been done. In order to avoid import loops, handlers don't know
-- the 'HOI4.Settings.HOI4' type itself, only its instances.
class (IsGame g,
       Scope g ~ HOI4Scope,
       IsGameData (GameData g),
       IsGameState (GameState g)) => HOI4Info g where
    -- | What @ROOT@ stands for in the script being written out, where known.
    getRootIdent :: Monad m => PPT g m (Maybe HOI4ScopeVal)
    -- | Set what @ROOT@ stands for, for the length of the given action.
    withRootIdent :: Monad m => Maybe HOI4ScopeVal -> PPT g m a -> PPT g m a
    -- | What @FROM@ stands for in the script being written out, where known.
    getFromIdent :: Monad m => PPT g m (Maybe HOI4ScopeVal)
    -- | Set what @FROM@ stands for, for the length of the given action.
    withFromIdent :: Monad m => Maybe HOI4ScopeVal -> PPT g m a -> PPT g m a
    -- | What the current scope (@THIS@) stands for, where known.
    getThisIdent :: Monad m => PPT g m (Maybe HOI4ScopeVal)
    -- | Say what the scope just entered stands for, for the length of the
    -- given action. Use just inside 'SettingsTypes.scope', which pushes an
    -- unknown; this fills it in.
    withThisIdent :: Monad m => Maybe HOI4ScopeVal -> PPT g m a -> PPT g m a
    -- | What the previous scope (@PREV@) stands for, where known.
    getPrevIdent :: Monad m => PPT g m (Maybe HOI4ScopeVal)
    -- | Get the title of an event by its ID. Only works if event scripts have
    -- been parsed.
    getEventTitle :: Monad m => Text -> PPT g m (Maybe Text)
    -- | Get the contents of all event script files.
    getEventScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Save (or amend) the contents of script event files in state.
    setEventScripts :: Monad m => HashMap FilePath GenericScript -> PPT g m ()
    -- | Get the parsed events table (keyed on event ID).
    getEvents :: Monad m => PPT g m (HashMap Text HOI4Event)
    -- | Get the contents of all idea groups files.
    getIdeaScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed idea groups table (keyed on idea group ID).
    getIdeas :: Monad m => PPT g m (HashMap Text HOI4Idea)
    -- | Get the contents of all opinion modifier script files.
    getOpinionModifierScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed opinion modifiers table (keyed on modifier ID).
    getOpinionModifiers :: Monad m => PPT g m (HashMap Text HOI4OpinionModifier)
    -- | Get the contents of all decision categories script files.
    getDecisioncatScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the contents of all decision script files.
    getDecisionScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed decision categories table (keyed on decision category ID).
    getDecisioncats :: Monad m => PPT g m (HashMap Text HOI4Decisioncat)
    -- | Get the parsed decisions table (keyed on decision ID).
    getDecisions :: Monad m => PPT g m (HashMap Text HOI4Decision)
    -- | Get the (known) event triggers
    getEventTriggers :: Monad m => PPT g m HOI4EventTriggers
    -- | Get the (known) event triggers
    getDecisionTriggers :: Monad m => PPT g m HOI4DecisionTriggers
    -- | Get the on actions script files
    getOnActionsScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the contents of all dynamic modifier script files.
    getDynamicModifierScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed dynamic modifiers table (keyed on modifier ID).
    getDynamicModifiers :: Monad m => PPT g m (HashMap Text HOI4DynamicModifier)
    -- | Get the contents of all modifier script files.
    getModifierScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed modifiers table (keyed on modifier ID).
    getModifiers :: Monad m => PPT g m (HashMap Text HOI4Modifier)
    -- | Get the content of all national focus files
    getNationalFocusScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get extra scripts parsed from command line arguments
    getNationalFocus :: Monad m => PPT g m (HashMap Text HOI4NationalFocus)

    -- | Get the country history files
    getCountryHistoryScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the country history parsed
    getCountryHistory :: Monad m => PPT g m (HashMap Text HOI4CountryHistory)
    -- | Get the value each variable holds when the game starts, keyed on the
    -- variable's name
    getInitialVariables :: Monad m => PPT g m (HashMap Text Double)
    -- | Get character script
    getCharacterScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the characters parsed
    getCharacters :: Monad m => PPT g m (HashMap Text HOI4Character)
    -- | Get leader traits script
    getCountryLeaderTraitScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the leader traits parsed
    getCountryLeaderTraits :: Monad m => PPT g m (HashMap Text HOI4CountryLeaderTrait)
    -- | Get leader traits script
    getUnitLeaderTraitScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the leader traits parsed
    getUnitLeaderTraits :: Monad m => PPT g m (HashMap Text HOI4UnitLeaderTrait)
    -- | Get terrain script
    getTerrainScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the terrain parsed
    getTerrain :: Monad m => PPT g m [Text]
    -- | Get unittags script
    getUnitTagScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the unittags parsed
    getUnitTag :: Monad m => PPT g m [Text]
    -- | Get unit script
    getUnitScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the unit parsed
    getUnit :: Monad m => PPT g m [Text]
    -- | Get leader traits script
    getIdeologyScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the leader traits parsed
    getIdeology :: Monad m => PPT g m (HashMap Text Text)
    -- | Get the advisors keyed on ideatoken
    getCharToken :: Monad m => PPT g m (HashMap Text HOI4Advisor)
    -- | Get scripted effects script
    getScriptedEffectScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the scripted effects parsed
    getScriptedEffects  :: Monad m => PPT g m (HashMap Text GenericStatement)
    -- | Get scripted triggers script
    getScriptedTriggerScripts  :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the scripted triggers parsed
    getScriptedTriggers  :: Monad m => PPT g m (HashMap Text GenericStatement)
    -- | Get modifier definition scripts
    getModifierDefintionScripts  :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the modifier definition  parsed
    getModifierDefinitions  :: Monad m => PPT g m (HashMap Text ModifierDisplay)
    -- | Get balance of power script
    getBopScripts  :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the balance of power parsed
    getBops  :: Monad m => PPT g m (HashMap Text HOI4BopRange)
    -- | Get technologies script
    getTechnologyScripts  :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the technologies parsed
    getTechnologies  :: Monad m => PPT g m (HashMap FilePath [HOI4Technology])
    -- | Get buildings script
    getBuildingScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the buildings parsed
    getBuildings :: Monad m => PPT g m (HashMap Text HOI4Building)
    -- | Get military industrial organization script
    getMioScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the name every military industrial organization, department trait
    -- and policy is known by, keyed on its token
    getMioNames :: Monad m => PPT g m (HashMap Text Text)
    getMioIncludes :: Monad m => PPT g m (HashMap Text Text)
    -- | Get scripted localization script
    getScriptedLocScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed scripted localizations, keyed on the name script calls
    -- each of them by
    getScriptedLoc :: Monad m => PPT g m (HashMap Text [HOI4ScriptedLocText])
    -- | Get script constant script
    getScriptConstantScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the numbers script can name instead of writing out, keyed on the
    -- dotted path that names each
    getScriptConstants :: Monad m => PPT g m (HashMap Text Double)
    -- | Get the scripts that are read only to be searched for fired events
    -- and activated decisions, keyed on category
    getExtraScripts :: Monad m => PPT g m (HashMap String (HashMap FilePath GenericScript))
    -- | Get the lockeys
    getLocKeys :: Monad m => PPT g m [Text]
    -- | Get the modkeys parsed
    getModKeys :: Monad m => PPT g m [Text]

-------------------
-- Feature types --
-------------------

-- | Event title type. As of HoI4 whatever version, titles may be conditional.
data HOI4EvtTitle
    = HOI4EvtTitleSimple Text  -- title = key
    | HOI4EvtTitleConditional GenericScript Text
            -- title = { text = key trigger = conditions }
    | HOI4EvtTitleCompound GenericScript
            -- title = { trigger = { conditional_expressions } }
    deriving (Show)

-- | Event description type. As of HOI4 whatever version, descriptions may be conditional.
data HOI4EvtDesc
    = HOI4EvtDescSimple Text  -- desc = key
    | HOI4EvtDescConditional GenericScript Text
            -- desc = { text = key trigger = conditions }
    | HOI4EvtDescCompound GenericScript
            -- desc = { trigger = { conditional_expressions } }
    deriving (Show)

-- | Event data.
data HOI4Event = HOI4Event {
    -- | Event ID
        hoi4evt_id :: Maybe Text
    -- | Event title l10n key
    ,   hoi4evt_title :: [HOI4EvtTitle]
    -- | Description
    ,   hoi4evt_desc :: [HOI4EvtDesc]
    -- | Type of thing the event happens to (e.g.  for a @country_event@ this
    -- is 'HOI4Country'). This is used to set the top level scope for its
    -- scripts.
    ,   hoi4evt_scope :: HOI4Scope
    -- | What conditions allow the event to trigger.
    ,   hoi4evt_trigger :: Maybe GenericScript
    -- | Whether the event is only triggered by script commands. If this is
    -- @False@ and the event also has a @mean_time_to_happen@, it can happen
    -- randomly.
    ,   hoi4evt_is_triggered_only :: Maybe Bool
    -- | If this is a random event, how unlikely this event is to happen.
    ,   hoi4evt_mean_time_to_happen :: Maybe GenericScript
    -- | Commands to execute as soon as the event fires.
    ,   hoi4evt_immediate :: Maybe GenericScript
    -- | Commands to execute after an option has been chosen.
    ,   hoi4evt_after :: Maybe GenericScript
    -- | Whether this is a hidden event (it will have no options).
    ,   hoi4evt_hide_window :: Bool
    -- | Whether this event can only happen once per campaign
    ,   hoi4evt_fire_only_once :: Bool
    -- | Whether this event is show to all countries (for example news events)
    ,   hoi4evt_major :: Bool
    -- | If the event is major this restricts who it is or isn't shown for.
    ,   hoi4evt_show_major :: Maybe GenericScript
    -- | List of options for the player/AI to choose from.
    ,   hoi4evt_options :: Maybe [HOI4Option]
    -- | If the event show to sender
    ,   hoi4evt_fire_for_sender :: Maybe Bool
    -- | The event's source file.
    ,   hoi4evt_path :: FilePath
    } deriving (Show)
-- | Event option data.
data HOI4Option = HOI4Option
    {   hoi4opt_name :: Maybe Text               -- ^ Text of the option
    ,   hoi4opt_trigger :: Maybe GenericScript   -- ^ Condition for the option to be available
    ,   hoi4opt_ai_chance :: Maybe AIWillDo -- ^ Probability that the AI will choose this option
    ,   hoi4opt_effects :: Maybe GenericScript   -- ^ What happens if the player/AI chooses this option
    } deriving (Show)

type HOI4SourceWeight = Maybe (Integer, Integer) -- Rational reduces the number, which we don't want
type HOI4EventWeight = HOI4SourceWeight
type HOI4DecisionWeight = HOI4SourceWeight

-- | A piece of game content that fires an event or activates a decision.
-- Shared between the event "triggered by" and decision "activated by" tables.
data HOI4Source =
      HOI4SrcImmediate Text                      -- Immediate effect of an event (arg is event ID)
    | HOI4SrcAfter Text                          -- After effect of an event, run when any option is chosen (arg is event ID)
    | HOI4SrcOption Text Text                    -- Effect of choosing an event option (args are event ID and option ID)
    | HOI4SrcDecComplete Text Text               -- Effect of completing a decision (args are id and localized decision text)
    | HOI4SrcDecRemove Text Text                 -- Effect of taking a timed decision and letting it finish (args are id and localized decision text)
    | HOI4SrcDecCancel Text Text                 -- Effect of taking a decision and it being canceled (args are id and localized decision text)
    | HOI4SrcDecTimeout Text Text                -- Effect of taking a decision/mission and letting it timeout (args are id and localized decision text)
    | HOI4SrcOnAction Text HOI4SourceWeight      -- An effect from on_actions (args are the trigger and weight)
    | HOI4SrcNFComplete Text Text Text           -- Effect of completing a national focus
    | HOI4SrcNFSelect Text Text Text             -- Effect of selecting a national focus
    | HOI4SrcIdeaOnAdd Text Text Text Text       -- Effect of adding an idea
    | HOI4SrcIdeaOnRemove Text Text Text Text    -- Effect of removing an idea
    | HOI4SrcCharacterOnAdd Text Text Text       -- Effect of adding an advisor
    | HOI4SrcCharacterOnRemove Text Text Text    -- Effect of removing an advisor
    | HOI4SrcScriptedEffect Text HOI4SourceWeight -- Effect of a scripted effect
    | HOI4SrcBopOnActivate Text                  -- Effect of a balance of power range activation
    | HOI4SrcBopOnDeactivate Text                -- Effect of a balance of power range deactivation
    | HOI4SrcSpecialProject Text                 -- Effect somewhere in a special project (arg is project ID)
    | HOI4SrcOperation Text                      -- Effect somewhere in an intelligence operation (arg is operation ID)
    | HOI4SrcRaid Text                           -- Effect somewhere in a raid (arg is raid ID)
    | HOI4SrcComplianceMod Text                  -- Effect of a resistance/compliance modifier (arg is modifier ID)
    deriving Show

type HOI4EventTriggers = HashMap Text [HOI4Source]

type HOI4DecisionTriggers = HashMap Text [HOI4Source]

-- | Idea data.
data HOI4Idea = HOI4Idea
    {   id_id :: Text -- ^ Idea ID
    ,   id_name :: Text -- ^ idea name
    ,   id_name_loc :: Text -- ^ Localized idea name
    ,   id_desc_loc :: Maybe Text
    ,   id_picture :: Text -- ^ uses idea ID unless filled in
    ,   id_allowed :: Maybe GenericScript
    ,   id_visible :: Maybe GenericScript
    ,   id_available :: Maybe GenericScript
    ,   id_modifier :: Maybe GenericStatement
    ,   id_targeted_modifier :: Maybe GenericScript
    ,   id_research_bonus :: Maybe GenericStatement
    ,   id_equipment_bonus :: Maybe GenericStatement
    ,   id_rule :: Maybe GenericScript
    ,   id_on_add :: Maybe GenericScript  -- ^ effects when the idea is added
    ,   id_on_remove :: Maybe GenericScript  -- ^ effects when the idea is removed
    ,   id_cancel :: Maybe GenericScript -- ^ tirggers for removing the idea
    ,   id_do_effect :: Maybe GenericScript -- ^ requirements for the idea's modifiers to work
    ,   id_allowed_civil_war :: Maybe GenericScript
    ,   id_traits :: Maybe GenericScript
    ,   id_law :: Bool -- ^ whether the slot holding it is one of the country's laws
    ,   id_category :: Text
    ,   id_path :: FilePath -- ^ Source file
    } deriving (Show)

-- | Decision data.
data HOI4Decisioncat = HOI4Decisioncat
    {   decc_name :: Text -- ^ Decision category ID
    ,   decc_name_loc :: Maybe Text -- ^ Localized decision category name
    ,   decc_desc_loc :: Maybe Text
    ,   decc_icon :: Text
    ,   decc_picture :: Maybe [Text] -- ^ Picture keys; several when script picks one by trigger
    ,   decc_custom_icon :: Maybe GenericScript
    ,   decc_visible :: Maybe GenericScript
    ,   decc_available :: Maybe GenericScript
    ,   decc_visiblity_type :: Maybe Text
    ,   decc_allowed :: Maybe GenericScript -- ^ Conditions that allow the category to appear
    ,   decc_visible_when_empty :: Maybe Text
    ,   decc_scripted_gui :: Maybe Text
    ,   decc_highlight_states :: Maybe GenericScript
    ,   decc_on_map_area :: Maybe GenericScript
    ,   decc_path :: FilePath -- ^ Source file
    } deriving (Show)

data HOI4DecisionCost
    = HOI4DecisionCostSimple Int
    | HOI4DecisionCostVariable Text
    deriving Show

-- | A count of days a decision runs for, waits out, or times out after. Script
-- writes it as a number, or names a variable the game works the number out from
-- as it draws the decision.
data HOI4DecisionDays
    = HOI4DecisionDaysSimple Int
    | HOI4DecisionDaysVariable Text
    deriving Show

data HOI4DecisionIcon
    = HOI4DecisionIconSimple Text
    | HOI4DecisionIconScript GenericScript
    deriving Show

-- | Decision data.
data HOI4Decision = HOI4Decision
    {   dec_name :: Text -- ^ Decision ID
    ,   dec_name_loc :: Text -- ^ Localized decision name
    ,   dec_desc :: Maybe Text -- ^ Descriptive text (shown on hover)
    ,   dec_icon :: Maybe HOI4DecisionIcon -- ^ Icon for the decision
    ,   dec_allowed :: Maybe GenericScript -- ^ Conditions that allow the player/AI to
                                           --   take the decision
    ,   dec_target_root_trigger :: Maybe GenericScript
    ,   dec_visible :: Maybe GenericScript
    ,   dec_available :: Maybe GenericScript
    ,   dec_is_good :: Bool -- ^ changes tooltip on whether timing out or compeleting mission is desirable, default is no and assumes complete_effect is desirable
    ,   dec_complete_effect :: Maybe GenericScript -- ^ the block of effects that gets executed immediately
                                                   --   when the decision is selected (Starting the timer if it has one).
    ,   dec_days_re_enable :: Maybe HOI4DecisionDays
    ,   dec_fire_only_once :: Bool
    ,   dec_cost :: Maybe HOI4DecisionCost
    ,   dec_custom_cost_text :: Maybe Text
    ,   dec_days_remove :: Maybe HOI4DecisionDays
    ,   dec_remove_effect :: Maybe GenericScript
    ,   dec_remove_trigger :: Maybe GenericScript
    ,   dec_modifier :: Maybe GenericStatement
    ,   dec_cancel_trigger ::  Maybe GenericScript
    ,   dec_cancel_effect ::  Maybe GenericScript

    ,   dec_days_mission_timeout :: Maybe HOI4DecisionDays
    ,   dec_activation :: Maybe GenericScript
    ,   dec_selectable_mission :: Bool
    ,   dec_timeout_effect :: Maybe GenericScript
    ,   dec_cancel_if_not_visible :: Bool

    ,   dec_targets :: Maybe GenericScript
    ,   dec_target_array :: Maybe Text
    ,   dec_targets_dynamic :: Bool
    ,   dec_target_trigger :: Maybe GenericScript
    ,   dec_targeted_modifier :: Maybe GenericScript
    ,   dec_state_target :: Maybe Text
    ,   dec_ai_will_do :: Maybe AIWillDo -- ^ Factors affecting whether an AI
                                         --   will take the decision when available
    ,   dec_path :: FilePath -- ^ Source file
    ,   dec_cat :: Text -- ^ Category of the decision
    } deriving (Show)

data HOI4DynamicModifier = HOI4DynamicModifier
    {   dmodName :: Text
    ,   dmodLocName :: Maybe Text
    ,   dmodPath :: FilePath
    ,   dmodIcon :: Maybe Text
    ,   dmodEffects :: GenericScript        -- The modifier to apply when the triggered modifier is active
    ,   dmodEnable :: GenericScript      -- Whether the triggered modifier is active
    ,   dmodRemoveTrigger :: Maybe GenericScript        -- Whether the triggered modifier is removed
    } deriving (Show)

data HOI4Modifier = HOI4Modifier
    {   modName :: Text
    ,   modLocName :: Maybe Text
    ,   modPath :: FilePath
    ,   modIcon :: Maybe Text
    ,   modEffects :: GenericScript        -- The modifier to apply when the triggered modifier is active
    ,   modRemoveTrigger :: Maybe GenericScript        -- Whether the triggered modifier is removed
    } deriving (Show)

data HOI4OpinionModifier = HOI4OpinionModifier
    {   omodName :: Text
    ,   omodLocName :: Maybe Text
    ,   omodPath :: FilePath
    ,   omodValue :: Maybe Double
    ,   omodMax :: Maybe Double
    ,   omodMin :: Maybe Double
    ,   omodDecay :: Maybe Double
    ,   omodDays :: Maybe Double
    ,   omodMonths :: Maybe Double
    ,   omodYears :: Maybe Double
    ,   omodTrade :: Maybe Bool
    ,   omodTarget :: Maybe Bool
    } deriving (Show)

--data HOI4NationalFocusIcon
--    = HOI4NationalFocusIconSimple Text
--    | HOI4NationalFocusIconScript GenericScript
--    deriving Show

data HOI4NationalFocus = HOI4NationalFocus
    {   nf_id :: Text
    ,   nf_name_loc :: Text
    ,   nf_name_desc :: Maybe Text
    ,   nf_text :: Maybe Text
    ,   nf_icon :: Text
    ,   nf_alt_icon :: Maybe Text
    ,   nf_icon_variants :: [(Text, GenericScript)] -- ^ Icons the focus shows in
                                 --   place of its usual one, each with the
                                 --   conditions the game shows it under
    ,   nf_name_variants :: [(Text, GenericScript)] -- ^ Names the focus goes by
                                 --   in place of its usual one, each with the
                                 --   conditions the game uses it under
    ,   nf_cost :: Double
    ,   nf_allow_branch  :: Maybe GenericScript
    ,   nf_prerequisite  :: [Maybe GenericScript]
    ,   nf_mutually_exclusive :: Maybe GenericScript
    ,   nf_available :: Maybe GenericScript
    ,   nf_bypass :: Maybe GenericScript
    ,   nf_cancel :: Maybe GenericScript
    ,   nf_cancelable :: Maybe Text
    ,   nf_historical_ai :: Maybe Text
    ,   nf_available_if_capitulated :: Maybe Text
    ,   nf_cancel_if_invalid :: Maybe Text
    ,   nf_continue_if_invalid :: Maybe Text
    ,   nf_will_lead_to_war_with :: Maybe Text
    ,   nf_search_filters :: Maybe Text
    ,   nf_select_effect :: Maybe GenericScript
    ,   nf_ai_will_do :: Maybe Text
    ,   nf_completion_reward :: Maybe GenericScript
    ,   nf_complete_tooltip :: Maybe GenericScript
    ,   nf_joint_complete_origin :: Maybe GenericScript
    ,   nf_joint_complete_member :: Maybe GenericScript
    ,   nf_joint_trigger :: Maybe GenericScript
    ,   nf_path :: FilePath -- ^ Source file
    ,   nf_ordinal :: Int -- ^ Where it sits in the order of its own file
    ,   nf_country :: Maybe Text -- ^ The country whose tree it stands in, where
                                 --   it stands in one country's alone
    } deriving (Show)

-- | One of the texts a scripted localization can come to. The game reads them
-- in the order they are written and takes the first whose conditions hold; one
-- that names no conditions holds always, and is the text it settles on when
-- nothing else applies.
data HOI4ScriptedLocText = HOI4ScriptedLocText
    {   sloc_key :: Text -- ^ The localization key holding the text itself
    ,   sloc_trigger :: Maybe GenericScript -- ^ What has to hold for this text
                                 --   to be the one used
    } deriving (Show)

data HOI4CountryHistory = HOI4CountryHistory
    {   chTag :: Text
    ,   chRulingTag :: Text
    ,   chCosmeticTag :: Maybe Text -- ^ The name the country starts out under,
                                 --   where its history gives it one instead of
                                 --   the name its tag alone would give
    } deriving (Show)

data HOI4Advisor = HOI4Advisor
    {   adv_advisor_slot :: Text
    ,   adv_cha_id     :: Text
    ,   adv_cha_name     :: Text
    ,   adv_idea_token   :: Text
    ,   adv_cha_portrait    :: Maybe Text
    ,   adv_traits       :: Maybe [Text]
    ,   adv_allowed      :: Maybe GenericScript
    ,   adv_visible      :: Maybe GenericScript
    ,   adv_available    :: Maybe GenericScript
    ,   adv_on_add       :: Maybe GenericScript
    ,   adv_on_remove    :: Maybe GenericScript
    ,   adv_modifier     :: Maybe GenericStatement
    ,   adv_research_bonus :: Maybe GenericStatement
    ,   adv_cost        :: Maybe Double
    ,   adv_can_be_fired :: Bool
    ,   adv_path         :: FilePath -- ^ Source file
    } deriving (Show)

data HOI4Character = HOI4Character
    {   cha_id          :: Text
    ,   cha_name        :: Text
    ,   cha_loc_name    :: Text
    ,   cha_portrait    :: Maybe Text
--    ,   chaId :: Maybe Int -- ^ legacy character id system is sometimes still used,
                         --   negative numbers count as not being there
    ,   cha_leader_traits :: Maybe [Text]
    ,   cha_leader_ideology :: Maybe Text
    ,   cha_advisor :: Maybe [HOI4Advisor]
    -- | The military posts the character is written for, innermost first. A
    -- character has one as a rule, and a handful are written for two.
    ,   cha_unit_roles :: [Text]
    ,   cha_path         :: FilePath -- ^ Source file
    } deriving (Show)

data HOI4CountryLeaderTrait = HOI4CountryLeaderTrait
    {   clt_id :: Text
    ,   clt_name :: Text
    ,   clt_loc_name :: Maybe Text
    ,   clt_path :: FilePath
    ,   clt_targeted_modifier :: Maybe GenericScript
    ,   clt_equipment_bonus :: Maybe GenericStatement
    ,   clt_hidden_modifier :: Maybe GenericStatement
    ,   clt_modifier :: Maybe GenericScript
    ,   clt_cp_cap :: Maybe Text
    } deriving (Show)

data HOI4UnitLeaderTrait = HOI4UnitLeaderTrait
    {   ult_id :: Text
    ,   ult_loc_name :: Maybe Text
    ,   ult_path :: FilePath
    ,   ult_modifier :: Maybe GenericStatement
    ,   ult_trait_xp_factor :: Maybe GenericStatement
    ,   ult_non_shared_modifier :: Maybe GenericStatement
    ,   ult_corps_commander_modifier :: Maybe GenericStatement
    ,   ult_field_marshal_modifier :: Maybe GenericStatement
    ,   ult_sub_unit_modifiers :: Maybe GenericStatement
    ,   ult_attack_skill :: Maybe Double
    ,   ult_defense_skill :: Maybe Double
    ,   ult_planning_skill :: Maybe Double
    ,   ult_logistics_skill :: Maybe Double
    ,   ult_maneuvering_skill :: Maybe Double
    ,   ult_coordination_skill :: Maybe Double
    } deriving (Show)

data HOI4BopRange = HOI4BopRange
    {   bop_id :: Text
    ,   bop_on_activate :: Maybe GenericScript
    ,   bop_on_deactivate :: Maybe GenericScript
    ,   bop_path :: FilePath
    } deriving (Show)

data HOI4Technology = HOI4Technology
    {   tech_id :: Text
    ,   tech_loc :: Text
    ,   tech_desc :: Maybe Text
    ,   tech_icon :: Text
    ,   tech_dependecies :: Maybe [Text]
    ,   tech_equipment :: Maybe [Text]
    ,   tech_modules :: Maybe [Text]
    ,   tech_units :: Maybe [Text]
    ,   tech_globalmod :: Maybe [GenericStatement] -- ^ global modifiers
    ,   tech_unitmod :: Maybe [GenericStatement] -- ^ modifiers for units
    ,   tech_catmod :: Maybe [GenericStatement] -- ^ modifiers for categories
    ,   tech_buildings :: Maybe [GenericScript]
    ,   tech_cost :: Double
    ,   tech_start_year :: Int
    ,   tech_doctrine :: Bool
    ,   tech_on_complete_limit :: Maybe GenericScript
    ,   tech_on_complete :: Maybe GenericScript
    ,   tech_sortrest :: Maybe [GenericStatement]
    ,   tech_filepath :: FilePath
    } deriving (Show)

-- | A building, as far as anything outside the construction interface needs it:
-- the modifiers it gives the state it stands in, which the game will show for a
-- building the script has just granted.
data HOI4Building = HOI4Building
    {   bld_id :: Text
    ,   bld_state_modifiers :: Maybe GenericStatement
    ,   bld_filepath :: FilePath
    } deriving (Show)

------------------------------
-- Shared lower level types --
------------------------------

-- | Scopes
data HOI4Scope
    = HOI4NoScope
    | HOI4Country
    | HOI4ScopeState
    | HOI4UnitLeader
    | HOI4Operative
    | HOI4ScopeCharacter
    | HOI4Division
    | HOI4From -- ^ Usually country or state, varies by context
    | HOI4Misc -- ^ custom for the parser, is used for var and event_target,
               --   because the scope depends on what is loaded into the var
    | HOI4Custom -- ^ custom for the parser, is usually defined by the code
                 --   example for ace_killed_by_ace events in on_actions PREV = enemy ace
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | What a scope pronoun stands for, where context pins it down. A pronoun in
-- script means whatever the game put there as it runs; most of the time
-- nothing outside the game can say what that is, but a decision knows its
-- taker and its targets, and an event can know who fires it, so within their
-- scripts the pronouns can be given as what they mean.
data HOI4ScopeVal
    = ScopeValTag Text -- ^ One country in particular, by tag
    | ScopeValState Int -- ^ One state in particular, by id
    | ScopeValRole HOI4Scope Text -- ^ Known only by role, e.g. \"Target Country\"
    deriving (Show)

-- | The scope type of what the pronoun stands for.
scopeValType :: HOI4ScopeVal -> HOI4Scope
scopeValType (ScopeValTag _) = HOI4Country
scopeValType (ScopeValState _) = HOI4ScopeState
scopeValType (ScopeValRole s _) = s

-- | The country tag the pronoun stands for, when it stands for one country.
scopeValTag :: HOI4ScopeVal -> Maybe Text
scopeValTag (ScopeValTag tag) = Just tag
scopeValTag _ = Nothing

-- | The one country an @allowed@-style trigger block confines a feature to,
-- where it does. Only a lone tag among the block's own statements counts:
-- anything under an @OR@ leaves a choice open, and no feature demands two
-- different tags at once.
allowedSoleTag :: Maybe GenericScript -> Maybe Text
allowedSoleTag mscr = case mapMaybe tagOf (concat mscr) of
    [tag] -> Just tag
    _ -> Nothing
    where
        tagOf [pdx| tag = $tag |] = Just tag
        tagOf [pdx| original_tag = $tag |] = Just tag
        tagOf _ = Nothing

-- | The one country a decision category can appear for, where its gates
-- confine it to one.
catTakerTag :: HOI4Decisioncat -> Maybe Text
catTakerTag cat = allowedSoleTag (decc_allowed cat) <|> allowedSoleTag (decc_visible cat)

-- | The one country that can take the decision, where its own gates or its
-- category's confine it to one. Many decisions are gated on their category
-- alone, so the category's gates speak for its decisions.
decTakerTag :: HashMap Text HOI4Decisioncat -> HOI4Decision -> Maybe Text
decTakerTag cats dec =
        allowedSoleTag (dec_allowed dec)
    <|> allowedSoleTag (dec_visible dec)
    <|> (catTakerTag =<< HM.lookup (dec_cat dec) cats)

-- | What FROM stands for in a targeted decision's scripts: the one entry of
-- @targets@ where there is only one, and its role otherwise, with
-- @state_target@ telling states from countries. No list at all still makes a
-- targeted decision -- everything the target trigger lets through is a
-- candidate -- and an untargeted decision leaves FROM unsaid.
decTargetIdent :: HOI4Decision -> Maybe HOI4ScopeVal
decTargetIdent dec = case (dec_targets dec, dec_target_array dec, dec_state_target dec) of
    (Just array, marray, Just _) -> Just $ case mapMaybe stateOf array of
        [n] | isNothing marray -> ScopeValState n
        _ -> stateRole
    (Just array, marray, Nothing) -> Just $ case mapMaybe tagOf array of
        [tag] | isNothing marray -> ScopeValTag tag
        _ -> countryRole
    (Nothing, Just _, Just _) -> Just stateRole
    (Nothing, Just _, Nothing) -> Just countryRole
    (Nothing, Nothing, Just _) -> Just stateRole
    (Nothing, Nothing, Nothing)
        | isJust (dec_target_trigger dec) -> Just countryRole
        | otherwise -> Nothing
    where
        countryRole = ScopeValRole HOI4Country "Target Country"
        stateRole = ScopeValRole HOI4ScopeState "Target State"
        -- Quiet counterparts of the extractors in 'HOI4.Decisions.ppdecision',
        -- which do the complaining about shapes we don't know; a shape unknown
        -- here only leaves FROM unsaid.
        tagOf (StatementBare (GenericLhs e [])) = Just e
        tagOf _ = Nothing
        stateOf (StatementBare (IntLhs e)) = Just e
        stateOf [pdx| state = !e |] = Just e
        stateOf _ = Nothing

-- | Run an action knowing what the pronouns mean in the given decision's
-- scripts: ROOT, and THIS at the top, are the taker, and FROM is the target.
withDecisionIdents :: (HOI4Info g, Monad m) => HOI4Decision -> PPT g m a -> PPT g m a
withDecisionIdents dec action = do
    cats <- getDecisioncats
    let rootIdent = ScopeValTag <$> decTakerTag cats dec
    withRootIdent rootIdent $ withThisIdent rootIdent $
        withFromIdent (decTargetIdent dec) action

-- | Run an action knowing what the pronouns mean in the given decision
-- category's own scripts and text: ROOT, and THIS at the top, are the one
-- country it can appear for, where known.
withCategoryIdents :: (HOI4Info g, Monad m) => HOI4Decisioncat -> PPT g m a -> PPT g m a
withCategoryIdents cat action =
    let rootIdent = ScopeValTag <$> catTakerTag cat
    in withRootIdent rootIdent $ withThisIdent rootIdent action

-- | Run an action knowing what the pronouns mean in the given national
-- focus's scripts and text: ROOT, and THIS at the top, are the country whose
-- tree it belongs to, where it belongs to one.
withFocusIdents :: (HOI4Info g, Monad m) => HOI4NationalFocus -> PPT g m a -> PPT g m a
withFocusIdents nf action =
    let rootIdent = ScopeValTag <$> nf_country nf
    in withRootIdent rootIdent $ withThisIdent rootIdent action

-- | The one country that fires the given event, where every source firing it
-- is known to belong to that same country. Inside the event this is who FROM
-- is, so an event with a single sender can name it wherever it says FROM. A
-- source belonging to no one country in particular -- an on_action, another
-- event's option -- keeps the sender unknown.
eventFirerTag :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
eventFirerTag eid = do
    eventTriggers <- getEventTriggers
    case HM.lookupDefault [] eid eventTriggers of
        [] -> return Nothing
        srcs -> do
            mtags <- traverse sourceFirerTag srcs
            return $ case mtags of
                (mtag@(Just _) : rest) | all (== mtag) rest -> mtag
                _ -> Nothing

-- | The country whose script a source stands in, where the source pins it
-- down to one: a decision only one country may take, or a focus in one
-- country's tree.
sourceFirerTag :: (HOI4Info g, Monad m) => HOI4Source -> PPT g m (Maybe Text)
sourceFirerTag src = case src of
    HOI4SrcDecComplete did _ -> decFirer did
    HOI4SrcDecRemove did _ -> decFirer did
    HOI4SrcDecCancel did _ -> decFirer did
    HOI4SrcDecTimeout did _ -> decFirer did
    HOI4SrcNFComplete fid _ _ -> nfFirer fid
    HOI4SrcNFSelect fid _ _ -> nfFirer fid
    _ -> return Nothing
    where
        decFirer did = do
            decs <- getDecisions
            cats <- getDecisioncats
            return $ decTakerTag cats =<< HM.lookup did decs
        nfFirer fid = do
            nfs <- getNationalFocus
            return $ nf_country =<< HM.lookup fid nfs

-- | AI decision factors.
data AIWillDo = AIWillDo
    {   awd_base :: Maybe Double
    ,   awd_modifiers :: [AIModifier]
    } deriving (Show)
-- | Modifiers for AI decision factors.
data AIModifier = AIModifier
    {   aim_factor :: Maybe Double
    ,   aim_add :: Maybe Double
    ,   aim_triggers :: GenericScript
    } deriving (Show)
-- | Empty decision factor.
newAIWillDo :: AIWillDo
newAIWillDo = AIWillDo Nothing []
-- | Empty modifier.
newAIModifier :: AIModifier
newAIModifier = AIModifier Nothing Nothing []

-- | Parse an @ai_will_do@ clause.
aiWillDo :: GenericScript -> AIWillDo
aiWillDo = foldl' aiWillDoAddSection newAIWillDo
aiWillDoAddSection :: AIWillDo -> GenericStatement -> AIWillDo
aiWillDoAddSection awd [pdx| $left = %right |] = case T.toLower left of
    "base" -> maybe awd
        (\fac -> awd { awd_base = Just fac })
        (floatRhs right)
    "factor" -> maybe awd
        (\fac -> awd { awd_base = Just fac })
        (floatRhs right)
    "modifier" -> case right of
        CompoundRhs scr -> awd { awd_modifiers = awd_modifiers awd ++ [awdModifier scr] }
        _               -> awd
    _ -> awd
aiWillDoAddSection awd _ = awd

-- | Parse a @modifier@ subclause for an @ai_will_do@ clause.
awdModifier :: GenericScript -> AIModifier
awdModifier = foldl' awdModifierAddSection newAIModifier
awdModifierAddSection :: AIModifier -> GenericStatement -> AIModifier
awdModifierAddSection aim stmt@[pdx| $left = %right |] = case T.toLower left of
    "factor" -> maybe aim
        (\fac -> aim { aim_factor = Just fac })
        (floatRhs right)
    "add" -> maybe aim
        (\add -> aim { aim_add = Just add })
        (floatRhs right)
    _ -> -- the rest of the statements are just the conditions.
        aim { aim_triggers = aim_triggers aim ++ [stmt] }
awdModifierAddSection aim stmt@[pdx| $_left > %_right |] = aim { aim_triggers = aim_triggers aim ++ [stmt] }
awdModifierAddSection aim stmt@[pdx| $_left < %_right |] = aim { aim_triggers = aim_triggers aim ++ [stmt] }
awdModifierAddSection aim _ = aim
