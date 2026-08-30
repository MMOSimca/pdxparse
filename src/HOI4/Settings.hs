{-|
Module      : HOI4.Settings
Description : Interface for Hearts of Iron IV backend
-}
module HOI4.Settings (
        HOI4 (..)
    ,   module HOI4.Types
    ) where

import Debug.Trace (trace)

import Control.Monad (join, when, forM, filterM, void, unless)
import Control.Monad.Trans (MonadIO (..), liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT (..), modify, gets)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Maybe (listToMaybe)

import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), isExtensionOf)
import System.IO (hPutStrLn, stderr)

import Abstract -- everything
import FileIO (buildPath, readScript)
import SettingsTypes ( PPT, Settings (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , safeIndex, safeLast)
import HOI4.Types -- everything
import HOI4.Localization
import Yaml (LocEntry (..))

-- Handlers
import HOI4.Decisions (parseHOI4Decisioncats, writeHOI4DecisionCats
                      ,parseHOI4Decisions, writeHOI4Decisions)
import HOI4.EventSources -- everything
import HOI4.Ideas (parseHOI4Ideas
    --, writeHOI4Ideas
    )
import HOI4.Modifiers (
                      parseHOI4OpinionModifiers, writeHOI4OpinionModifiers, writeHOI4OpinionModifiers'
                    , parseHOI4DynamicModifiers, writeHOI4DynamicModifiers
                    , parseHOI4Modifiers)
import HOI4.NationalFocus(parseHOI4NationalFocuses, writeHOI4NationalFocuses)
import HOI4.Events (parseHOI4Events, writeHOI4Events)
import HOI4.CharactersAndTraits (parseHOI4Characters, parseHOI4CountryLeaderTraits, parseHOI4UnitLeaderTraits)

import HOI4.TechAndEquipment (parseHOI4TechnologiesPath, writeHOI4Technologies, parseHOI4UnitTags, parseHOI4Units)
import HOI4.Misc (parseHOI4CountryHistory
                 , parseHOI4Terrain, parseHOI4Ideology
                 , parseHOI4Effects, parseHOI4Triggers
                 , parseHOI4BopRanges, parseHOI4ModifierDefinitions
                 , parseHOI4Buildings, parseHOI4MioNames
                 , parseHOI4ScriptedLoc
                 , parseHOI4ScriptConstants
                 , parseHOI4LocKeys)

-- | Temporary (?) fix for CHI and PRC both localizing to "China"
-- The wiki keeps "China" for CHI and names PRC after the state it holds at the
-- start, "Chinese Soviet Republic", so the clash is broken on PRC's side.
-- Can be extended/removed as necessary
fixLocalization :: Settings -> Settings
fixLocalization s =
    let
        lan  = language s
        l10n = gameL10n s
        l10nForLan = HM.findWithDefault HM.empty lan l10n
        findKey key = content $ HM.findWithDefault (LocEntry 0 key) key l10nForLan
        prcLoc = findKey "PRC"
        prcPartyLoc = findKey "PRC_communism"
        -- A key with no entry comes back as itself, so fall back to the tag
        -- rather than putting "PRC_communism" on the page as a country name.
        newPrcLoc = if prcPartyLoc == "PRC_communism"
                        then prcLoc <> " (PRC)"
                        else prcPartyLoc
        newL10n = HM.insert "PRC" (LocEntry 0 newPrcLoc) l10nForLan
    in
        if prcLoc == findKey "CHI" then
            trace ("Note: Applying localization fix for CHI/PRC: " ++ show prcLoc ++ " -> " ++ show newPrcLoc) $
                s { gameL10n = HM.insert lan newL10n l10n }
        else
            trace "Warning: fixLocalization hack for CHI/PRC in HOI4/Settings.hs no longer needed!" s

-- | HOI4 game type. This is only interesting for its instances.
data HOI4 = HOI4
instance IsGame HOI4 where
    readScripts  = readHOI4Scripts
    parseScripts = parseHOI4Scripts
    writeScripts = writeHOI4Scripts
    data GameData HOI4 = HOI4D { hoi4d :: HOI4Data }
    data GameState HOI4 = HOI4S { hoi4s :: HOI4State }
    runWithInitState HOI4 settings st =
        void (runReaderT
                (runStateT st (HOI4D $ HOI4Data {
                    hoi4settings = fixLocalization settings
                ,   hoi4events = HM.empty
                ,   hoi4eventScripts = HM.empty
                ,   hoi4decisioncatScripts = HM.empty
                ,   hoi4decisioncats = HM.empty
                ,   hoi4decisions = HM.empty
                ,   hoi4decisionScripts = HM.empty
                ,   hoi4ideas = HM.empty
                ,   hoi4ideaScripts = HM.empty
                ,   hoi4opmods = HM.empty
                ,   hoi4opmodScripts = HM.empty
                ,   hoi4eventTriggers = HM.empty
                ,   hoi4decisionTriggers = HM.empty
                ,   hoi4onactionsScripts = HM.empty
                ,   hoi4dynamicmodifiers = HM.empty
                ,   hoi4dynamicmodifierScripts = HM.empty
                ,   hoi4modifiers = HM.empty
                ,   hoi4modifierScripts = HM.empty
                ,   hoi4nationalfocusScripts = HM.empty
                ,   hoi4nationalfocus = HM.empty
                ,   hoi4countryHistory = HM.empty
                ,   hoi4initialvariables = HM.empty
                ,   hoi4countryHistoryScripts = HM.empty
                ,   hoi4characterScripts = HM.empty
                ,   hoi4characters = HM.empty
                ,   hoi4countryleadertraitScripts = HM.empty
                ,   hoi4countryleadertraits = HM.empty
                ,   hoi4unitleadertraitScripts = HM.empty
                ,   hoi4unitleadertraits = HM.empty
                ,   hoi4terrainScripts = HM.empty
                ,   hoi4terrain = []
                ,   hoi4unittagScripts = HM.empty
                ,   hoi4unittag = []
                ,   hoi4unitScripts = HM.empty
                ,   hoi4unit = []
                ,   hoi4ideologyScripts = HM.empty
                ,   hoi4ideology = HM.empty
                ,   hoi4chartoken = HM.empty

                ,   hoi4scriptedeffectScripts = HM.empty
                ,   hoi4scriptedeffects = HM.empty
                ,   hoi4scriptedtriggerScripts = HM.empty
                ,   hoi4scriptedtriggers = HM.empty
                ,   hoi4modifierdefinitionScripts = HM.empty
                ,   hoi4modifierdefinitions = HM.empty
                ,   hoi4bopScripts = HM.empty
                ,   hoi4bops = HM.empty
                ,   hoi4techScripts = HM.empty
                ,   hoi4techs = HM.empty
                ,   hoi4buildingScripts = HM.empty
                ,   hoi4buildings = HM.empty
                ,   hoi4mioScripts = HM.empty
                ,   hoi4mionames = HM.empty
                ,   hoi4mioincludes = HM.empty
                ,   hoi4scriptedlocScripts = HM.empty
                ,   hoi4scriptedloc = HM.empty
                ,   hoi4scriptconstantScripts = HM.empty
                ,   hoi4scriptconstants = HM.empty
                ,   hoi4extraScripts = HM.empty
                ,   hoi4lockeys = []
                ,   hoi4modkeys = []
                }))
                (HOI4S $ HOI4State {
                    hoi4currentFile = Nothing
                ,   hoi4currentIndent = Nothing
                ,   hoi4scopeStack = []
                ,   hoi4IsInEffect = False
                ,   hoi4expandedBlocks = []
                ,   hoi4currentCharacter = Nothing
                ,   hoi4rootIdent = Nothing
                ,   hoi4fromIdent = Nothing
                ,   hoi4identStack = []
                }))
    type Scope HOI4 = HOI4Scope
    -- What the new scope stands for is not known here, only its type; whoever
    -- does know calls 'withThisIdent' just inside to fill the unknown in.
    scope s = local $ \(HOI4S st) -> HOI4S $
        st { hoi4scopeStack = s : hoi4scopeStack st
           , hoi4identStack = Nothing : hoi4identStack st }
    getCurrentScope = asks $ listToMaybe . hoi4scopeStack . hoi4s
    getPrevScope = asks $ safeIndex 1 . hoi4scopeStack . hoi4s
    getPrevScopeCustom i = asks $ safeIndex i . hoi4scopeStack . hoi4s
    getRootScope = asks $ safeLast . hoi4scopeStack . hoi4s
    getScopeStack = asks $ hoi4scopeStack . hoi4s
    getIsInEffect = asks $ hoi4IsInEffect . hoi4s
    setIsInEffect b = local $ \(HOI4S st) -> HOI4S $ st { hoi4IsInEffect = b }
    getExpandedBlocks = asks $ hoi4expandedBlocks . hoi4s
    withExpandedBlock name = local $ \(HOI4S st) -> HOI4S $
        st { hoi4expandedBlocks = name : hoi4expandedBlocks st }
    getCurrentCharacter = asks $ hoi4currentCharacter . hoi4s
    withCurrentCharacter name = local $ \(HOI4S st) -> HOI4S $
        st { hoi4currentCharacter = Just name }

instance HOI4Info HOI4 where
    getRootIdent = asks $ hoi4rootIdent . hoi4s
    withRootIdent mv = local $ \(HOI4S st) -> HOI4S $ st { hoi4rootIdent = mv }
    getFromIdent = asks $ hoi4fromIdent . hoi4s
    withFromIdent mv = local $ \(HOI4S st) -> HOI4S $ st { hoi4fromIdent = mv }
    getThisIdent = asks $ join . listToMaybe . hoi4identStack . hoi4s
    withThisIdent mv = local $ \(HOI4S st) -> HOI4S $
        st { hoi4identStack = mv : drop 1 (hoi4identStack st) }
    getPrevIdent = asks $ join . safeIndex 1 . hoi4identStack . hoi4s
    getEventTitle eid = do
        HOI4D ed <- get
        let evts = hoi4events ed
            mevt = HM.lookup eid evts
        case mevt of
            Nothing -> return Nothing
            Just evt -> case hoi4evt_title evt of
                [] -> return Nothing
                [HOI4EvtTitleSimple key] -> getGameL10nIfPresent key
                _titles -> return Nothing
    getEventScripts = do
        HOI4D ed <- get
        return (hoi4eventScripts ed)
    setEventScripts scr = modify $ \(HOI4D ed) -> HOI4D $ ed {
            hoi4eventScripts = scr
        }
    getEvents = do
        HOI4D ed <- get
        return (hoi4events ed)
    getIdeaScripts = do
        HOI4D ed <- get
        return (hoi4ideaScripts ed)
    getIdeas = do
        HOI4D ed <- get
        return (hoi4ideas ed)
    getDecisioncatScripts = do
        HOI4D ed <- get
        return (hoi4decisioncatScripts ed)
    getDecisionScripts = do
        HOI4D ed <- get
        return (hoi4decisionScripts ed)
    getDecisioncats = do
        HOI4D ed <- get
        return (hoi4decisioncats ed)
    getDecisions = do
        HOI4D ed <- get
        return (hoi4decisions ed)
    getOpinionModifierScripts = do
        HOI4D ed <- get
        return (hoi4opmodScripts ed)
    getOpinionModifiers = do
        HOI4D ed <- get
        return (hoi4opmods ed)
    getEventTriggers = do
        HOI4D ed <- get
        return (hoi4eventTriggers ed)
    getDecisionTriggers = do
        HOI4D ed <- get
        return (hoi4decisionTriggers ed)
    getOnActionsScripts = do
        HOI4D ed <- get
        return (hoi4onactionsScripts ed)
    getDynamicModifierScripts = do
        HOI4D ed <- get
        return (hoi4dynamicmodifierScripts ed)
    getDynamicModifiers = do
        HOI4D ed <- get
        return (hoi4dynamicmodifiers ed)
    getModifierScripts = do
        HOI4D ed <- get
        return (hoi4modifierScripts ed)
    getModifiers = do
        HOI4D ed <- get
        return (hoi4modifiers ed)
    getNationalFocusScripts = do
        HOI4D ed <- get
        return (hoi4nationalfocusScripts ed)
    getNationalFocus = do
        HOI4D ed <- get
        return (hoi4nationalfocus ed)

    getCountryHistoryScripts = do
        HOI4D ed <- get
        return (hoi4countryHistoryScripts ed)
    getCountryHistory = do
        HOI4D ed <- get
        return (hoi4countryHistory ed)
    getInitialVariables = do
        HOI4D ed <- get
        return (hoi4initialvariables ed)
    getCharacterScripts = do
        HOI4D ed <- get
        return (hoi4characterScripts ed)
    getCharacters = do
        HOI4D ed <- get
        return (hoi4characters ed)
    getCountryLeaderTraitScripts = do
        HOI4D ed <- get
        return (hoi4countryleadertraitScripts ed)
    getCountryLeaderTraits = do
        HOI4D ed <- get
        return (hoi4countryleadertraits ed)
    getUnitLeaderTraitScripts = do
        HOI4D ed <- get
        return (hoi4unitleadertraitScripts ed)
    getUnitLeaderTraits = do
        HOI4D ed <- get
        return (hoi4unitleadertraits ed)
    getTerrainScripts = do
        HOI4D ed <- get
        return (hoi4terrainScripts ed)
    getTerrain = do
        HOI4D ed <- get
        return (hoi4terrain ed)
    getUnitTagScripts = do
        HOI4D ed <- get
        return (hoi4unittagScripts ed)
    getUnitTag = do
        HOI4D ed <- get
        return (hoi4unittag ed)
    getUnitScripts = do
        HOI4D ed <- get
        return (hoi4unitScripts ed)
    getUnit = do
        HOI4D ed <- get
        return (hoi4unit ed)
    getIdeologyScripts = do
        HOI4D ed <- get
        return (hoi4ideologyScripts ed)
    getIdeology = do
        HOI4D ed <- get
        return (hoi4ideology ed)
    getCharToken = do
        HOI4D ed <- get
        return (hoi4chartoken ed)
    getScriptedEffectScripts = do
        HOI4D ed <- get
        return (hoi4scriptedeffectScripts ed)
    getScriptedEffects = do
        HOI4D ed <- get
        return (hoi4scriptedeffects ed)
    getScriptedTriggerScripts = do
        HOI4D ed <- get
        return (hoi4scriptedtriggerScripts ed)
    getScriptedTriggers = do
        HOI4D ed <- get
        return (hoi4scriptedtriggers ed)
    getModifierDefintionScripts = do
        HOI4D ed <- get
        return (hoi4modifierdefinitionScripts ed)
    getModifierDefinitions = do
        HOI4D ed <- get
        return (hoi4modifierdefinitions ed)
    getBopScripts = do
        HOI4D ed <- get
        return (hoi4bopScripts ed)
    getBops = do
        HOI4D ed <- get
        return (hoi4bops ed)
    getTechnologyScripts = do
        HOI4D ed <- get
        return (hoi4techScripts ed)
    getTechnologies = do
        HOI4D ed <- get
        return (hoi4techs ed)
    getBuildingScripts = do
        HOI4D ed <- get
        return (hoi4buildingScripts ed)
    getBuildings = do
        HOI4D ed <- get
        return (hoi4buildings ed)
    getMioScripts = do
        HOI4D ed <- get
        return (hoi4mioScripts ed)
    getMioIncludes = do
        HOI4D ed <- get
        return (hoi4mioincludes ed)
    getMioNames = do
        HOI4D ed <- get
        return (hoi4mionames ed)
    getScriptedLocScripts = do
        HOI4D ed <- get
        return (hoi4scriptedlocScripts ed)
    getScriptedLoc = do
        HOI4D ed <- get
        return (hoi4scriptedloc ed)
    getScriptConstantScripts = do
        HOI4D ed <- get
        return (hoi4scriptconstantScripts ed)
    getScriptConstants = do
        HOI4D ed <- get
        return (hoi4scriptconstants ed)
    getExtraScripts = do
        HOI4D ed <- get
        return (hoi4extraScripts ed)
    getLocKeys = do
        HOI4D ed <- get
        return (hoi4lockeys ed)
    getModKeys = do
        HOI4D ed <- get
        return (hoi4modkeys ed)

instance IsGameData (GameData HOI4) where
    getSettings (HOI4D ed) = hoi4settings ed

instance IsGameState (GameState HOI4) where
    currentFile (HOI4S es) = hoi4currentFile es
    modifyCurrentFile cf (HOI4S es) = HOI4S $ es {
            hoi4currentFile = cf
        }
    currentIndent (HOI4S es) = hoi4currentIndent es
    modifyCurrentIndent ci (HOI4S es) = HOI4S $ es {
            hoi4currentIndent = ci
        }

-- | Read all scripts in a directory.
--
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readHOI4Scripts :: forall m. MonadIO m => PPT HOI4 m ()
readHOI4Scripts = do
    settings <- gets getSettings
    let readOneScript :: String -> String -> PPT HOI4 m (String, GenericScript)
        readOneScript category target = do
            content <- liftIO $ readScript settings target
            --traceM (show target)
            when (null content) $
                liftIO $ hPutStrLn stderr $
                    "Warning: " ++ target
                        ++ " contains no scripts - failed parse? Expected feature type "
                        ++ category
            return (target, content)

        readHOI4Script :: String -> PPT HOI4 m (HashMap String GenericScript)
        readHOI4Script category = do
            let sourceSubdir = case category of
                    "ideas" -> "common" </> "ideas"
                    "opinion_modifiers" -> "common" </> "opinion_modifiers"
                    "on_actions" -> "common" </> "on_actions"
                    "dynamic_modifiers" -> "common" </> "dynamic_modifiers"
                    "modifiers" -> "common" </> "modifiers"
                    "decisions" -> "common" </> "decisions"
                    "decisioncats" -> "common" </> "decisions" </> "categories"
                    "national_focus" -> "common" </> "national_focus"

                    "country_history" -> "history" </> "countries"
                    "characters" -> "common" </> "characters"
                    "country_leader_trait" -> "common" </> "country_leader"
                    "unit_leader_trait" -> "common" </> "unit_leader"
                    "terrain" -> "common" </> "terrain"
                    "unit_tags" -> "common" </> "unit_tags"
                    "units" -> "common" </> "units"
                    "ideology" -> "common" </> "ideologies"
                    "scripted_localisation" -> "common" </> "scripted_localisation"
                    "scripted_effect" -> "common" </> "scripted_effects"
                    "scripted_trigger" -> "common" </> "scripted_triggers"
                    "modifier_definitions" -> "common" </> "modifier_definitions"
                    "bop" -> "common" </> "bop"
                    "technologies" -> "common" </> "technologies"
                    "buildings" -> "common" </> "buildings"
                    "mio" -> "common" </> "military_industrial_organization" </> "organizations"
                    "script_constants" -> "common" </> "script_constants"
                    "special_projects" -> "common" </> "special_projects" </> "projects"
                    "operations" -> "common" </> "operations"
                    "raids" -> "common" </> "raids"
                    "resistance_compliance_modifiers" -> "common" </> "resistance_compliance_modifiers"
                    _          -> category
                sourceDir = buildPath settings sourceSubdir
            direxist <- liftIO $ doesDirectoryExist sourceDir
            if direxist
            then do
                files <- liftIO (filterM (doesFileExist . buildPath settings . (sourceSubdir </>))
                                    =<< filterM (pure . isExtensionOf ".txt")
                                     =<< getDirectoryContents sourceDir)
                results <- forM files $ \filename -> readOneScript category (sourceSubdir </> filename)
                return $ foldl (flip (uncurry HM.insert)) HM.empty results
            else return $ trace ("WARNING: Unable to find " ++ show sourceDir) HM.empty
    ideasScripts <- readHOI4Script "ideas"
    decisioncats <- readHOI4Script "decisioncats"
    decisions <- readHOI4Script "decisions"
    events <- readHOI4Script "events"
    opinion_modifiers <- readHOI4Script "opinion_modifiers"
    on_actions <- readHOI4Script "on_actions"
    dynamic_modifiers <- readHOI4Script "dynamic_modifiers"
    modifiers <- readHOI4Script "modifiers"
    national_focus <- readHOI4Script "national_focus"

    country_history <- readHOI4Script "country_history"
    characterScripts <- readHOI4Script "characters"
    countryleadertraitScripts <- readHOI4Script "country_leader_trait"
    unitleadertraitScripts <- readHOI4Script "unit_leader_trait"

    terrainScripts <- readHOI4Script "terrain"
    unittagScripts <- readHOI4Script "unit_tags"
    unitScripts <- readHOI4Script "units"
    ideologyScripts <- readHOI4Script "ideology"

    scripted_localisation <- readHOI4Script "scripted_localisation"
    scripted_effects <- readHOI4Script "scripted_effect"
    scripted_triggers <- readHOI4Script "scripted_trigger"

    moddefs <- readHOI4Script "modifier_definitions"
    bopscript <- readHOI4Script "bop"
    techscript <- readHOI4Script "technologies"
    buildingscript <- readHOI4Script "buildings"
    mioscript <- readHOI4Script "mio"
    constantscript <- readHOI4Script "script_constants"
    -- Read only to be searched for fired events and activated decisions.
    extrascripts <- HM.fromList <$> forM
        ["special_projects", "operations", "raids", "resistance_compliance_modifiers"]
        (\cat -> (,) cat <$> readHOI4Script cat)
    lockeys <- gets (gameL10nKeys . getSettings)

    modify $ \(HOI4D s) -> HOI4D $ s {
            hoi4ideaScripts = ideasScripts
        ,   hoi4decisioncatScripts = decisioncats
        ,   hoi4decisionScripts = decisions
        ,   hoi4eventScripts = events
        ,   hoi4opmodScripts = opinion_modifiers
        ,   hoi4onactionsScripts = on_actions
        ,   hoi4dynamicmodifierScripts = dynamic_modifiers
        ,   hoi4modifierScripts = modifiers
        ,   hoi4countryHistoryScripts = country_history

        ,   hoi4nationalfocusScripts = national_focus
        ,   hoi4characterScripts = characterScripts
        ,   hoi4countryleadertraitScripts = countryleadertraitScripts
        ,   hoi4unitleadertraitScripts = unitleadertraitScripts

        ,   hoi4terrainScripts = terrainScripts
        ,   hoi4unittagScripts = unittagScripts
        ,   hoi4unitScripts = unitScripts
        ,   hoi4ideologyScripts = ideologyScripts

        ,   hoi4scriptedeffectScripts = scripted_effects
        ,   hoi4scriptedtriggerScripts = scripted_triggers

        ,   hoi4modifierdefinitionScripts = moddefs

        ,   hoi4bopScripts = bopscript
        ,   hoi4techScripts = techscript
        ,   hoi4buildingScripts = buildingscript
        ,   hoi4mioScripts = mioscript
        ,   hoi4scriptedlocScripts = scripted_localisation
        ,   hoi4scriptconstantScripts = constantscript
        ,   hoi4extraScripts = extrascripts
        ,   hoi4lockeys = lockeys
        }


-- | Interpret the script ASTs as usable data.
parseHOI4Scripts :: Monad m => PPT HOI4 m ()
parseHOI4Scripts = do
    -- The country history has to be read and stored before anything else, since
    -- it says which party rules each country at the start of the game and so
    -- which of its several names the game calls it by. Every localization
    -- lookup from here on asks for that name (see "HOI4.Localization"), and a
    -- parser that stored its text before the history was in hand would have
    -- stored it under the wrong one.
    (countryHistory, initialVariables) <- parseHOI4CountryHistory =<< getCountryHistoryScripts
    scriptedLoc <- parseHOI4ScriptedLoc =<< getScriptedLocScripts
    modify $ \(HOI4D s) -> HOI4D $
            s { hoi4countryHistory = countryHistory
            ,   hoi4initialvariables = initialVariables
            ,   hoi4scriptedloc = scriptedLoc
            }

    -- Need idea groups and modifiers before everything else
    ideas <- parseHOI4Ideas =<< getIdeaScripts
    opinionModifiers <- parseHOI4OpinionModifiers =<< getOpinionModifierScripts
    dynamicModifiers <- parseHOI4DynamicModifiers =<< getDynamicModifierScripts
    modifiers <- parseHOI4Modifiers =<< getModifierScripts
    decisioncats <- parseHOI4Decisioncats =<< getDecisioncatScripts
    decisions <- parseHOI4Decisions =<< getDecisionScripts
    events <- parseHOI4Events =<< getEventScripts
    on_actions <- getOnActionsScripts
    nationalFocus <- parseHOI4NationalFocuses =<< getNationalFocusScripts

    (characters, chartoken) <- parseHOI4Characters =<< getCharacterScripts
    countryleadertraits <- parseHOI4CountryLeaderTraits =<< getCountryLeaderTraitScripts
    unitleadertraits <- parseHOI4UnitLeaderTraits =<< getUnitLeaderTraitScripts
    terrain <- parseHOI4Terrain =<< getTerrainScripts
    unittag <- parseHOI4UnitTags =<< getUnitTagScripts
    unit <- parseHOI4Units =<< getUnitScripts
    ideology <- parseHOI4Ideology =<< getIdeologyScripts
    scriptedeffects <- parseHOI4Effects =<< getScriptedEffectScripts
    scriptedtriggers <- parseHOI4Triggers =<< getScriptedTriggerScripts
    moddef <- parseHOI4ModifierDefinitions =<< getModifierDefintionScripts
    bops <- parseHOI4BopRanges =<< getBopScripts
    techspathed <- parseHOI4TechnologiesPath =<< getTechnologyScripts
    buildings <- parseHOI4Buildings =<< getBuildingScripts
    (mionames, mioincludes) <- parseHOI4MioNames =<< getMioScripts
    constants <- parseHOI4ScriptConstants =<< getScriptConstantScripts
    modkeys <- parseHOI4LocKeys =<< getLocKeys

    extraScripts <- getExtraScripts
    let extra cat = concat (HM.elems (HM.lookupDefault HM.empty cat extraScripts))
    let te1 = findTriggeredEventsInEvents HM.empty (HM.elems events)
        te2 = findTriggeredEventsInDecisions te1 (HM.elems decisions)
        te3 = findTriggeredEventsInOnActions te2 (concat (HM.elems on_actions))
        te4 = findTriggeredEventsInNationalFocus te3 (HM.elems nationalFocus)
        te5 = findTriggeredEventsInIdeas te4 (HM.elems ideas)
        te6 = findTriggeredEventsInCharacters te5 (HM.elems chartoken)
        te7 = findTriggeredEventsInScriptedEffects te6 (HM.elems scriptedeffects)
        te8 = findTriggeredEventsInBops te7 (HM.elems bops)
        te9 = findTriggeredEventsInGenericScripts HOI4SrcSpecialProject te8 (extra "special_projects")
        te10 = findTriggeredEventsInGenericScripts HOI4SrcOperation te9 (extra "operations")
        te11 = findTriggeredEventsInGenericScripts HOI4SrcRaid te10 (extra "raids")
        te12 = findTriggeredEventsInGenericScripts HOI4SrcComplianceMod te11 (extra "resistance_compliance_modifiers")
    let td1 = findActivatedDecisionsInEvents HM.empty (HM.elems events)
        td2 = findActivatedDecisionsInDecisions td1 (HM.elems decisions)
        td3 = findActivatedDecisionsInOnActions td2 (concat (HM.elems on_actions))
        td4 = findActivatedDecisionsInNationalFocus td3 (HM.elems nationalFocus)
        td5 = findActivatedDecisionsInIdeas td4 (HM.elems ideas)
        td6 = findActivatedDecisionsInCharacters td5 (HM.elems chartoken)
        td7 = findActivatedDecisionsInScriptedEffects td6 (HM.elems scriptedeffects)
        td8 = findActivatedDecisionsInBops td7 (HM.elems bops)
        td9 = findActivatedDecisionsInGenericScripts HOI4SrcSpecialProject td8 (extra "special_projects")
        td10 = findActivatedDecisionsInGenericScripts HOI4SrcOperation td9 (extra "operations")
        td11 = findActivatedDecisionsInGenericScripts HOI4SrcRaid td10 (extra "raids")
        td12 = findActivatedDecisionsInGenericScripts HOI4SrcComplianceMod td11 (extra "resistance_compliance_modifiers")
    modify $ \(HOI4D s) -> HOI4D $
            s { hoi4events = events
            ,   hoi4decisioncats = decisioncats
            ,   hoi4decisions = decisions
            ,   hoi4ideas = ideas
            ,   hoi4opmods = opinionModifiers
            ,   hoi4nationalfocus = nationalFocus
            ,   hoi4eventTriggers = te12
            ,   hoi4decisionTriggers = td12
            ,   hoi4dynamicmodifiers = dynamicModifiers
            ,   hoi4modifiers = modifiers

            ,   hoi4characters = characters
            ,   hoi4countryleadertraits = countryleadertraits
            ,   hoi4unitleadertraits = unitleadertraits
            ,   hoi4chartoken = chartoken

            ,   hoi4terrain = terrain
            ,   hoi4unittag = unittag
            ,   hoi4unit = unit
            ,   hoi4ideology = ideology

            ,   hoi4scriptedeffects = scriptedeffects
            ,   hoi4scriptedtriggers = scriptedtriggers

            ,   hoi4modifierdefinitions = moddef
            ,   hoi4bops = bops
            ,   hoi4techs = techspathed
            ,   hoi4buildings = buildings
            ,   hoi4mionames = mionames
            ,   hoi4mioincludes = mioincludes
            ,   hoi4scriptconstants = constants
            ,   hoi4modkeys = modkeys
            }

-- | Output the game data as wiki text.
writeHOI4Scripts :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Scripts = do
--        liftIO $ putStrLn "Writing ideas."
--        writeHOI4Ideas
        liftIO $ putStrLn "Writing events."
        writeHOI4Events
        liftIO $ putStrLn "Writing decision categories."
        writeHOI4DecisionCats
        liftIO $ putStrLn "Writing decisions."
        writeHOI4Decisions
        liftIO $ putStrLn "Writing national focuses."
        writeHOI4NationalFocuses
        liftIO $ putStrLn "Writing technologies."
        writeHOI4Technologies
        liftIO $ putStrLn "Writing opinion modifiers."
        writeHOI4OpinionModifiers
        liftIO $ putStrLn "Writing dynamic modifiers."
        writeHOI4DynamicModifiers
