{-
Module      : HOI4.Decisions
Description : Feature handler for Hearts of Iron IV decisions
-}
module HOI4.Decisions (
        parseHOI4Decisioncats, writeHOI4DecisionCats,
        parseHOI4Decisions, writeHOI4Decisions
    ) where

import Debug.Trace (trace, traceM)

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM, (<=<))
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.State (gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.List (intersperse, foldl', intercalate, nubBy, sortBy)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import System.FilePath ((</>), takeFileName)

import Abstract -- everything
import qualified Doc
import FileIO ( Feature (..), Consolidation (..), ConsolidatedFeature (..)
              , naturalOrder, writeFeatures, writeFeaturesWith)
import HOI4.WikiPage ( CountryIndex, buildCountryIndex
                     , ppPageIntro, ppSectionHeader, boxWrapper, requiresDebug)
import HOI4.EventSources (ppSource)
import HOI4.Messages -- everything
import MessageTools (italicText, formatDays)
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions
                     , getGameInterface, getGameInterfaceNamed, getGameInterfaceIfPresent)
import HOI4.Common -- everything
import HOI4.Localization

-- | Empty decision category. Starts off Nothing/empty everywhere, except id and name
-- (which should get filled in immediately).
newDecisionCat :: Text -> Maybe Text -> Maybe Text -> FilePath -> HOI4Decisioncat
newDecisionCat id locid locdesc = HOI4Decisioncat id locid locdesc "decision_category_generic" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Take the decisions categories scripts from game data and parse them into decision
-- data structures.
parseHOI4Decisioncats :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Decisioncat)
parseHOI4Decisioncats scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseHOI4Decisioncat scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing decision category: " ++ T.unpack err
            return HM.empty
        Right deccatFilesOrErrors ->
            flip HM.traverseWithKey deccatFilesOrErrors $ \sourceFile edeccats ->
                fmap (mkDecCatMap . catMaybes) . forM edeccats $ \case
                    Left err -> do
                        traceM $ "Error parsing decision categories in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right ddeccat -> return ddeccat
                where mkDecCatMap :: [HOI4Decisioncat] -> HashMap Text HOI4Decisioncat
                      mkDecCatMap = HM.fromList . map (decc_name &&& id)

-- | Parse one decisioncategory script into a decision data structure.
parseHOI4Decisioncat :: (HOI4Info g, MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4Decisioncat))
parseHOI4Decisioncat (StatementBare _) = throwError "bare statement at top level"
parseHOI4Decisioncat [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] -> withCurrentFile $ \file -> do
            locid <- getGameL10nIfPresent id
            locdesc <- getGameL10nIfPresent (id <> "_desc")
            ddeccat <- hoistErrors $ foldM decisioncatAddSection
                                        (Just (newDecisionCat id locid locdesc file))
                                        parts
            case ddeccat of
                Left err -> return (Left err)
                Right Nothing -> return (Right Nothing)
                Right (Just deccat) -> withCurrentFile $ \_file ->
                    return (Right (Just deccat ))
        _ -> throwError "unrecognized form for decision category"
    _ -> throwError "unrecognized form for decision category"
parseHOI4Decisioncat _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for decisi= reon category in " <> T.pack file)

-- | Add a sub-clause of the decision categry script to the data structure.
decisioncatAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4Decisioncat -> GenericStatement -> PPT g m (Maybe HOI4Decisioncat)
decisioncatAddSection Nothing _ = return Nothing
decisioncatAddSection ddeccat stmt
    = return $ (`decisioncatAddSection'` stmt) <$> ddeccat
    where
        decisioncatAddSection' decc stmt = case stmt of
            [pdx| icon           = $txt  |] -> decc { decc_icon = txt }
            [pdx| visible        = %rhs  |] -> case rhs of
                CompoundRhs [] -> decc -- empty, treat as if it wasn't there
                CompoundRhs scr -> decc { decc_visible = Just scr } -- can check from and root if target_root_trigger is true (or allowed if it's not present)
                _ -> trace "bade decision category allowed" decc
            [pdx| available      = %rhs  |] -> case rhs of
                CompoundRhs [] -> decc -- empty, treat as if it wasn't there
                CompoundRhs scr -> decc { decc_available = Just scr } -- checks visible, if it's false the decision is greyed out but still visible
                _ -> trace "bade decision category allowed" decc
            [pdx| picture        = $txt  |] -> decc { decc_picture = Just txt }
            [pdx| custom_icon    = %_    |] -> decc
            [pdx| visibility_type = %_   |] -> decc
            [pdx| priority       = %_    |] -> decc
            [pdx| allowed        = %rhs  |] -> case rhs of
                CompoundRhs [] -> decc -- empty, treat as if it wasn't there
                CompoundRhs scr -> decc { decc_allowed = Just scr } -- Checks only once on start/load an is used to restrict which countries have/not have it
                _ -> trace "bade decision category allowed" decc
            [pdx| visible_when_empty = %_ |] -> decc
            [pdx| scripted_gui   = %_    |] -> decc
            [pdx| highlight_states = %_  |] -> decc
            [pdx| on_map_area    = %_    |] -> decc
            [pdx| $other = %_ |] -> trace ("unknown decision category section: " ++ T.unpack other) decc
            _ -> trace "unrecognised form for decision category section" decc

-- | Present the parsed decision categoriess as wiki text and write them to the
-- appropriate files.
writeHOI4DecisionCats :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4DecisionCats = do
    decisionCats <- getDecisioncats
    let pathedDecisionCats :: [Feature HOI4Decisioncat]
        pathedDecisionCats = map (\decc -> Feature {
                                        featurePath = Just $ decc_path decc
                                    ,   featureId = Just (decc_name decc) <> Just ".txt"
                                    ,   theFeature = Right decc })
                              (HM.elems decisionCats)
    writeFeatures "decisions"
                  pathedDecisionCats
                  (scope HOI4Country . ppdecisioncat)

-- | Present a parsed decision category.
ppdecisioncat :: forall g m. (HOI4Info g, MonadError Text m) => HOI4Decisioncat -> PPT g m Doc
ppdecisioncat decc = setCurrentFile (decc_path decc) $ withCategoryIdents decc $ do
    version <- gets (gameVersion . getSettings)
    decc_text_loc <- fmap wikifyLocColours <$> getGameL10nIfPresent (decc_name decc <> "_desc")
    let deccArg :: Text -> (HOI4Decisioncat -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        deccArg fieldname field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        ["| ", Doc.strictText fieldname, " = "
                        ,PP.line
                        ,content_pp'd
                        ,PP.line])
                (field decc)
    allow_pp'd  <- deccArg "allowed" decc_allowed ppScript
    visible_pp'd  <- deccArg "visible" decc_visible ppScript
    available_pp'd  <- deccArg "available" decc_available ppScript
    let name = decc_name decc
        nameD = Doc.strictText name
    name_loc <- wikifyLocColours <$> getGameL10n name
    let icon = decc_icon decc
        deccpicture = decc_picture decc
    icon_pp <- do
        micon <- getGameInterfaceIfPresent ("GFX_decision_category_" <> decc_name decc)
        case micon of
            Nothing -> let iconcat = if not $ "GFX_decision_category_" `T.isPrefixOf` icon then "GFX_decision_category_" <> icon else icon in
                    getGameInterface "decision_category_generic" iconcat
            Just icond -> return icond
    picture_pp <- do
        case deccpicture of
            Nothing -> return mempty
            Just picd -> do
                let piccat = if not $ "GFX_decision_category_" `T.isPrefixOf` picd then "GFX_decision_category_" <> picd else picd
                mpic <- getGameInterfaceIfPresent piccat
                maybe (return mempty) (\p -> return $ mconcat ["<!-- picture: ", Doc.strictText p, " -->", PP.line]) mpic
    return . mconcat $
        ["== [[File:", Doc.strictText icon_pp, ".png|22px]]" , "<!-- ", nameD, " --> ", Doc.strictText name_loc," ==", PP.line]++

        [maybe mempty
               (\txt -> mconcat [picture_pp, Doc.strictText $ italicText $ Doc.nl2br txt, PP.line, PP.line])
               decc_text_loc
        ] ++
        allow_pp'd ++
        visible_pp'd ++
        available_pp'd

-- | Empty decision. Starts off Nothing/empty everywhere, except id and name
-- (which should get filled in immediately).
newDecision :: HOI4Decision
newDecision = HOI4Decision undefined undefined Nothing Nothing Nothing Nothing Nothing Nothing False Nothing Nothing False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False Nothing False Nothing Nothing False Nothing Nothing Nothing Nothing undefined undefined

-- | Take the decisions scripts from game data and parse them into decision
-- data structures.
parseHOI4Decisions :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Decision)
parseHOI4Decisions scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ concat <$> mapM parseHOI4DecisionGroup scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing decisions: " ++ T.unpack err
            return HM.empty
        Right decFilesOrErrors ->
            flip HM.traverseWithKey decFilesOrErrors $ \sourceFile edecs ->
                fmap (mkDecMap . catMaybes) . forM edecs $ \case
                    Left err -> do
                        traceM $ "Error parsing decisions in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right ddec -> return ddec
                where mkDecMap :: [HOI4Decision] -> HashMap Text HOI4Decision
                      mkDecMap = HM.fromList . map (dec_name &&& id)

-- | Parse one file's decision groups scripts into decision data structures.
parseHOI4DecisionGroup :: (HOI4Info g, Monad m) =>
    GenericStatement -> PPT g (ExceptT Text m) [Either Text (Maybe HOI4Decision)]
parseHOI4DecisionGroup (StatementBare _) = throwError "bare statement at top level"
parseHOI4DecisionGroup [pdx| $left = @scr |]
    = forM scr $ \stmt -> (Right <$> parseHOI4Decision stmt left)
                            `catchError` (return . Left)
parseHOI4DecisionGroup [pdx| %check = %_ |] = case check of
    AtLhs _ -> return [Right Nothing]
    _-> throwError "unrecognized form for decision block (LHS)"
parseHOI4DecisionGroup _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for decision in " <> T.pack file)

-- | Parse one decision script into a decision data structure.
parseHOI4Decision :: (HOI4Info g, Monad m) =>
    GenericStatement -> Text -> PPT g (ExceptT Text m) (Maybe HOI4Decision)
parseHOI4Decision [pdx| $decName = %rhs |] category = case rhs of
    CompoundRhs parts -> do
        decName_loc <- wikifyLocColours <$> getGameL10n decName
        decDesc <- getGameL10nIfPresent (decName <> "_desc")
        withCurrentFile $ \sourcePath ->
            foldM decisionAddSection
                  (Just (newDecision { dec_name = decName
                              , dec_name_loc = decName_loc
                              , dec_desc = decDesc
                              , dec_path = sourcePath </> T.unpack category -- so decision are divided into maps for the cateogry, should I loc or not?
                              , dec_cat = category}))
                  parts
    _ -> throwError "unrecognized form for decision (RHS)"
parseHOI4Decision [pdx| %check = %_ |] _ = case check of
    AtLhs _ -> return Nothing
    _-> throwError "unrecognized form for decision block (LHS)"
parseHOI4Decision _ _ = throwError "unrecognized form for decision (LHS)"

-- | Add a sub-clause of the decision script to the data structure.
decisionAddSection :: (HOI4Info g, MonadError Text m) =>
    Maybe HOI4Decision -> GenericStatement -> PPT g m (Maybe HOI4Decision)
decisionAddSection Nothing _ = return Nothing
-- desc needs localization, so it can't be handled in the pure code below
decisionAddSection dec [pdx| desc = %rhs |] = case rhs of
    GenericRhs txt [] -> do
        mloc <- getGameL10nIfPresent txt
        return $ (\d -> d { dec_desc = maybe (dec_desc d) Just mloc }) <$> dec
    -- compound desc (text with triggers); keep the default localization
    _ -> return dec
decisionAddSection dec stmt
    = return $ (`decisionAddSection'` stmt) <$> dec
    where -- the QQ pdx patternmatching takes to long to compile with this many patterns so using case of here
        decisionAddSection' dec stmt@[pdx| $lhs = %rhs |] = case T.toLower lhs of
            "icon" -> case rhs of
                GenericRhs txt _ ->
                    dec { dec_icon = Just (HOI4DecisionIconSimple txt) }
                CompoundRhs scr -> dec { dec_icon = Just (HOI4DecisionIconScript scr) }
                _ -> trace "DEBUG: bad decisions icon" dec
            "allowed" -> case rhs of -- Checks only once on start/load an is used to restrict which countries have/not have it
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_allowed = Just scr }
                _ -> trace "DEBUG: bad decisions allowed" dec
            "complete_effect" -> case rhs of -- effect when selecting decision, or when mission starts
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_complete_effect = Just scr }
                _ -> trace "DEBUG: bad decisions complete_effect" dec
            "ai_will_do" -> case rhs of
                CompoundRhs scr -> dec { dec_ai_will_do = Just (aiWillDo scr) }
                _ -> trace "DEBUG: bad decisions ai_will_do" dec
            "target_root_trigger" -> case rhs of -- can only check root for targeted decisions if allowed is true
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_target_root_trigger = Just scr }
                _ -> trace "DEBUG: bad decisions target_root_trigger" dec
            "visible" -> case rhs of -- can check from and root if target_root_trigger is true (or allowed if it's not present)
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_visible = Just scr }
                _ -> trace "DEBUG: bad decisions visible" dec
            "available" -> case rhs of -- checks visible, if it's false the decision is greyed out but still visible
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_available = Just scr }
                _ -> trace "DEBUG: bad decisions available" dec
            "priority" -> dec
            "highlight_states" -> dec
            "days_re_enable" -> case rhs of -- number of days it takes for decision to reapear after completion
                (floatRhs -> num) -> dec { dec_days_re_enable = num }
                --_ -> trace "DEBUG: bad decisions days_re_enable" dec
            "fire_only_once"  -> case rhs of --bool, standard false
                GenericRhs "yes" [] -> dec { dec_fire_only_once = True }
                -- no is the default, so I don't think this is ever used
                GenericRhs "no" [] -> dec { dec_fire_only_once = False }
                _ -> trace "DEBUG: bad decisions fire_only_once" dec
            "cost" -> case rhs of --var or num
                GenericRhs txt [] -> dec { dec_cost = Just (HOI4DecisionCostVariable txt) }
                GenericRhs _var [txt] -> dec { dec_cost = Just (HOI4DecisionCostVariable txt) }
                (floatRhs -> Just num)  -> dec { dec_cost = Just (HOI4DecisionCostSimple num) }
                _ -> trace "DEBUG: bad decisions costt" dec
            "custom_cost_trigger" -> case rhs of
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_custom_cost_trigger = Just scr }
                _ -> trace "DEBUG: bad decisions custom_cost_trigger" dec
            "custom_cost_text" -> case rhs of
                GenericRhs txt _ -> -- localizable
                    dec { dec_custom_cost_text = Just txt }
                _ -> trace "DEBUG: bad decisions custom_cost_text" dec
            "days_remove" -> case rhs of --number of days it takes to finish decision
                (floatRhs -> num) -> dec { dec_days_remove = num }
                --_ -> trace "DEBUG: bad decisions days_remove" dec
            "remove_effect" -> case rhs of -- effect when decision completes
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_remove_effect = Just scr }
                _ -> trace "DEBUG: bad decisions remove_effect" dec
            "cancel_trigger" -> case rhs of -- trigger for canceling missions
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_cancel_trigger = Just scr }
                _ -> trace "DEBUG: bad decisions cancel_trigger" dec
            "cancel_effect" -> case rhs of -- effect when mission is canceled
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_cancel_effect = Just scr }
                _ -> trace "DEBUG: bad decisions cancel_effect" dec
            "war_with_on_remove" -> dec -- used to inform if a decison declares war when finished
            "war_with_on_complete" -> dec -- used to inform if a decison declares war when selected
            "war_with_on_timeout" -> dec -- used to inform if a decison declares war when selected
            "fixed_random_seed" -> dec --bool, standard True
            "days_mission_timeout" -> case rhs of -- how long the mission takes to finish, and turns decision into mission
                (floatRhs -> num)  -> dec { dec_days_mission_timeout = num }
                --_ -> trace "DEBUG: bad decisions days_mission_timeout" dec
            "activation" -> case rhs of -- checks for if a mission starts
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_activation = Just scr }
                _ -> trace "DEBUG: bad decisions activation" dec
            "selectable_mission" -> case rhs of --bool, standard false
                GenericRhs "yes" [] -> dec { dec_selectable_mission = True }
                -- no is the default, so I don't think this is ever used
                GenericRhs "no" [] -> dec { dec_selectable_mission = False }
                _ -> trace "DEBUG: bad decisions selectable_mission" dec
            "timeout_effect" -> case rhs of -- effect for mission completing
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_timeout_effect = Just scr }
                _ -> trace "DEBUG: bad decisions trimeout_effect" dec
            "is_good" ->  case rhs of --bool, standard false, says wether finishing the mission is good or bad
                GenericRhs "yes" [] -> dec { dec_is_good = True }
                GenericRhs "no" [] -> dec { dec_is_good = False }
                _ -> trace "DEBUG: bad decisions is_good" dec
            "targets" -> case rhs of -- weirdo array , checks countries for which decision can be targeted to, turn decisions into targeted decision
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_targets = Just scr }
                _ -> trace "DEBUG: bad decisions targets" dec
            "target_array" -> case rhs of -- uses variables to create targets for decision, turn decisions into targeted decision
                GenericRhs txt _ -> dec { dec_target_array = Just txt }
                _ -> trace "DEBUG: bad decisions target array" dec
            "targets_dynamic" -> case rhs of --bool, standard false , makes targets also check for orignal_tag
                GenericRhs "yes" [] -> dec { dec_targets_dynamic = True }
                -- no is the default, so I don't think this is ever used
                GenericRhs "no" [] -> dec { dec_targets_dynamic = False }
                _ -> trace "DEBUG: bad decisions targets_dynamic" dec
            "target_trigger" -> case rhs of -- alternate to visible for targeted decision
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_target_trigger = Just scr }
                _ -> trace "DEBUG: bad decisions target_trigger" dec
            "war_with_target_on_complete" -> dec --bool, standard false
            "war_with_target_on_remove" -> dec --bool, standard false
            "war_with_target_on_timeout" -> dec --bool, standard false
            "state_target" -> case rhs of --bool, standard false , targeted decison uses targets for states
                GenericRhs "no" [] -> dec
                -- no is the default, so I don't think this is ever used
                GenericRhs trgt [] -> dec { dec_state_target = Just trgt }
                _ -> trace "DEBUG: bad decisions state_target" dec
            "on_map_mode" -> dec
            "modifier" -> case rhs of -- effects that apply when decision is active (timer/mission?)
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs _scr -> dec { dec_modifier = Just stmt }
                _ -> trace "DEBUG: bad decisions modifier" dec
            "targeted_modifier" -> case rhs of -- effects for country/state targeted and duration?
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs _scr -> let oldstmt = fromMaybe [] (dec_targeted_modifier dec) in
                    dec { dec_targeted_modifier = Just (oldstmt ++ [stmt]) }
                _ -> trace "DEBUG: bad decisions targeted_modifier" dec
            "cancel_if_not_visible" -> case rhs of -- cancels mission if visible is false
                GenericRhs "yes" [] -> dec { dec_cancel_if_not_visible = True }
                -- no is the default, so I don't think this is ever used
                GenericRhs "no" [] -> dec { dec_cancel_if_not_visible = False }
                _ -> trace "DEBUG: bad decisions cancel_if_not_visible" dec
            "name" -> case rhs of -- is used over the localized id
                GenericRhs txt _ -> dec {dec_name = txt}
                -- literal display name (e.g. the DEBUG balance-of-power decisions);
                -- keep dec_name as the id since it's used as the output filename
                StringRhs txt -> dec {dec_name_loc = txt}
                _ -> trace "DEBUG: bad decisions name" dec
            "ai_hint_pp_cost" -> dec
            "cosmetic_tag" -> dec -- no clue
            "cosmetic_ideology" -> dec -- no clue
            "remove_trigger" -> case rhs of -- used to cancel timed decision
                CompoundRhs [] -> dec -- empty, treat as if it wasn't there
                CompoundRhs scr -> dec { dec_remove_trigger = Just scr }
                _ -> trace "DEBUG: bad decisions remove_trigger" dec
            "target_non_existing" -> dec -- no clue
            "power_balance" -> dec -- no clue, only seen in debug so far
            other -> trace ("unknown decision section: " ++ show other ++ "  " ++ show stmt) dec
        decisionAddSection' dec _stmt = trace "unrecognised form for decision section" dec

-- | Present the parsed decisions as wiki text and write them to the
-- appropriate files. Also write one consolidated file per decision category
-- folder, with a wiki header before each decision so the page gets a usable
-- table of contents.
writeHOI4Decisions :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Decisions = do
    decisions <- getDecisions
    countries <- buildCountryIndex
    let pathedDecisions :: [Feature HOI4Decision]
        pathedDecisions = map (\dec -> Feature {
                                        featurePath = Just $ dec_path dec
                                    ,   featureId = Just (dec_name dec) <> Just ".txt"
                                    ,   theFeature = Right dec })
                              (HM.elems decisions)
    writeFeaturesWith "decisions"
                  pathedDecisions
                  (scope HOI4Country . ppdecision)
                  (Just (ConsolidateInParent (ppDecisionsPage countries)))

-- | Present one decision file's decisions as a single wiki page, a section
-- per category, since each category is written to a folder of its own.
-- Decisions only visible in debug mode are left off the page, their
-- categories with them if that empties them; a file with nothing else in it
-- makes no page at all.
ppDecisionsPage :: (HOI4Info g, Monad m) =>
    CountryIndex -> FilePath -> [ConsolidatedFeature HOI4Decision] -> PPT g m (Maybe Doc)
ppDecisionsPage countries srcPath cfs = do
    cats <- getDecisioncats
    case filter (not . decisionRequiresDebug cats . cfFeature) cfs of
        [] -> return Nothing
        shown -> do
            intro <- ppPageIntro countries "decisions" srcPath
            let byCategory = M.toAscList $
                    M.map (sortBy (\a b -> naturalOrder (cfId a) (cfId b))) $
                    M.fromListWith (++) [(T.pack (takeFileName (cfDir cf)), [cf]) | cf <- shown]
            sections <- mapM ppCategorySection byCategory
            return . Just . mconcat $ intersperse PP.line (intro : sections)

-- | Whether the decision can only be seen with the game in debug mode. All
-- four of its own trigger blocks gate whether it shows, and its category's do
-- too: a decision of a category that never appears is never seen either.
decisionRequiresDebug :: HashMap Text HOI4Decisioncat -> HOI4Decision -> Bool
decisionRequiresDebug cats dec =
    any (requiresDebug . ($ dec))
        [dec_allowed, dec_target_root_trigger, dec_visible, dec_target_trigger]
    || maybe False catRequiresDebug (HM.lookup (dec_cat dec) cats)
    where
        catRequiresDebug cat = any (requiresDebug . ($ cat))
            [decc_allowed, decc_visible]

-- | Present one category's decisions: a heading naming the category, then its
-- decisions wrapped so their boxes flow together. The category's name is
-- read as its own gates read the pronouns, so a name written for one country
-- names it.
ppCategorySection :: (HOI4Info g, Monad m) =>
    (Text, [ConsolidatedFeature HOI4Decision]) -> PPT g m Doc
ppCategorySection (catId, cfs) = do
    cats <- getDecisioncats
    mloc <- maybe id withCategoryIdents (HM.lookup catId cats) $
        getGameL10nIfPresent catId
    let header = ppSectionHeader (fromMaybe catId mloc) (Just catId)
    return $ header <> PP.line <> boxWrapper (map cfDoc cfs)

-- | Present a parsed decision.
ppdecision :: forall g m. (HOI4Info g, MonadError Text m) => HOI4Decision -> PPT g m Doc
ppdecision dec = setCurrentFile (dec_path dec) $ withDecisionIdents dec $ do
    version <- gets (gameVersion . getSettings)
    -- The description was localized at parse time, before what the pronouns
    -- mean here was known, so what they name is filled in now -- and only
    -- where it is pinned down: an ambiguous pronoun keeps its brackets rather
    -- than have a role's wording spliced into running text.
    dec_text_loc <- traverse (fmap wikifyLocColours . fillLocScopes) (dec_desc dec)
    let decArg :: Text -> (HOI4Decision -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        decArg fieldname field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        ["| ", Doc.strictText fieldname, " = "
                        ,PP.line
                        ,content_pp'd
                        ,PP.line])
                (field dec)
    let decNoArg :: (HOI4Decision -> Maybe a) -> (a -> PPT g m Doc) -> Doc -> PPT g m [Doc]
        decNoArg field fmt opttext
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        [opttext, content_pp'd
                        ,PP.line])
                (field dec)
    -- Script the wiki's decision template has no parameter for. Passing it as
    -- one anyway only has the template drop it, so it is written into the
    -- page's source as a note, where an editor can still find it.
    let decNote :: Text -> (HOI4Decision -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        decNote label field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        ["<!-- ", Doc.strictText label, ":", PP.line
                        ,content_pp'd, PP.line
                        ,"-->", PP.line])
                (field dec)
    targets <- case (dec_targets dec, dec_target_array dec, dec_state_target dec) of
        (Just array, _, Just trgt) -> do
            let targetlist = mapMaybe extractTargetsStates array
            targetlistloc <- traverse getStateLoc targetlist
            let targetdoc = [Doc.ppString (intercalate ", " $ map T.unpack targetlistloc)]
            trgtlmt <- if trgt == "yes" then return $ Doc.strictText "" else do
                trgtloc <- getGameL10n trgt
                return $ Doc.strictText ("Limited to " <> trgtloc <> "<!-- Requires editing --> states ")
            return $ ["| targets = " <> trgtlmt] ++ targetdoc ++ [PP.line]
        (Just array, _, Nothing) -> do
            let targetlist = mapMaybe extractTargets array
            targetlistloc <- traverse (flagText (Just HOI4Country)) targetlist
            let targetdoc = [Doc.ppString (intercalate ", " $ map T.unpack targetlistloc)]
            return $ ["| targets = "] ++ targetdoc ++ [PP.line]
        (_, Just array, Just trgt) -> do
            trgtlmt <- if trgt == "yes" then return $ Doc.strictText "" else do
                trgtloc <- getGameL10n trgt
                return $ Doc.strictText ("Limited to " <> trgtloc <> "<!-- Requires editing --> states ")
            return ["| targets = ", trgtlmt,Doc.strictText array, "<!-- requires editing -->", PP.line]
        (_, Just array, Nothing) -> return ["| targets = ",Doc.strictText array, PP.line]
        _ -> return [""]
    ------
    allow_pp'd <- decNoArg dec_allowed ppScript "<!-- allowed -->"
    targetRootTrigger_pp'd <- decNoArg dec_target_root_trigger ppScript "<!-- target_root_trigger -->" -- checks ROOT
    visible_pp'd <- decNoArg dec_visible ppScript "<!-- visible -->"
    targetTrigger_pp'd <- decNoArg dec_target_trigger ppScript "<!-- target_trigger -->" --checks FROM and ROOT and makes decision visible if true
    available_pp'd <- decArg "available" dec_available ppScript
    -- Removes a timed decision early, ending its modifier and triggering its
    -- remove_effect. The template has no parameter for it, so it is noted.
    removeTrigger_note <- decNote "remove_trigger" dec_remove_trigger ppScript
    cancelTrigger_pp'd <- decArg "cancel_trigger" dec_cancel_trigger ppScript -- cancels missions, triggers canceleffect
    effect_pp'd <- setIsInEffect True (decArg "select_effect" dec_complete_effect ppScript)
    removeEffect_pp'd <- setIsInEffect True (decArg "remove_effect" dec_remove_effect ppScript)
    cancelEffect_pp'd <- setIsInEffect True (decArg "cancel_effect" dec_cancel_effect ppScript)
    timeoutEffect_pp'd <- setIsInEffect True (decArg "timeout_effect" dec_timeout_effect ppScript)
    mawd_pp'd   <- setIsInEffect True (mapM (imsg2doc <=< ppAiWillDo) (dec_ai_will_do dec))
    let name = dec_name dec
        nameD = Doc.strictText name
        cost_pp = case dec_cost dec of
            Just (HOI4DecisionCostSimple num) -> Just $ T.pack $ show num
            Just (HOI4DecisionCostVariable txt) -> Just txt
            _ -> Nothing
        isFireOnlyOnce = dec_fire_only_once dec
        isGood = dec_is_good dec
        isSelectableMission = dec_selectable_mission dec
        cancelIfNotVisible = dec_cancel_if_not_visible dec
        targetsDynamic = dec_targets_dynamic dec
    -- A cost that isn't political power. The template's own cost parameter
    -- writes a political power icon after whatever it is given, so anything
    -- else has to go through custom_cost, which replaces the whole phrase.
    custom_cost_loc_pp'd <- case dec_custom_cost_text dec of
            Just custom_cost_text -> do
                custom_cost_text_loc <- wikifyLocColours <$> getGameL10n custom_cost_text
                return ["| custom_cost = ", Doc.strictText custom_cost_text_loc, PP.line]
            _ -> return []
    -- What the custom cost actually checks and takes. The template says only
    -- what the cost is called, so this is noted rather than passed.
    custom_cost_trigger_note <- decNote "custom_cost_trigger" dec_custom_cost_trigger ppScript
    activation_pp'd <- decNoArg dec_activation ppScript "<!-- activation -->"
    modifier_pp'd <- setIsInEffect True (decNoArg dec_modifier ppStatement "")
    targetedModifier_pp'd <- setIsInEffect True (decNoArg dec_targeted_modifier ppScript "")
    name_loc <- wikifyLocColours <$> getGameL10n name
    icon_pp'd <- case dec_icon dec of
        Just (HOI4DecisionIconSimple txt) -> do
            let icond = if not $ "GFX_decision_" `T.isPrefixOf` txt then "GFX_decision_" <> txt else txt
            micon <- getGameInterfaceIfPresent icond
            case micon of
                Just icondd -> return $ "| decision_icon = " <> icondd <> "\n"
                Nothing -> return mempty
        Just (HOI4DecisionIconScript _) -> return "| decision_icon = <!-- Check script -->\n"
        _ -> return mempty
    let days_remove = dec_days_remove dec
        days_re_enable = dec_days_re_enable dec
        days_mission_timeout = dec_days_mission_timeout dec
    ppActivatedBy_pp'd <- ppActivatedBy (dec_name dec)
    return . mconcat $
        ["<section begin=", nameD, "/>", PP.line
        ,"{{Decision", PP.line
        ,"| version = ", Doc.strictText version, PP.line
        ,"| collapse = no", PP.line
        ,"| decision_id = ", nameD, PP.line
        ,"| decision_name = ", Doc.strictText name_loc, PP.line
        , Doc.strictText icon_pp'd
        ,maybe mempty
               (\txt -> mconcat ["| decision_text = ", Doc.strictText $ Doc.nl2br txt, PP.line])
               dec_text_loc
        ] ++
        custom_cost_loc_pp'd ++
        [maybe mempty (\txt -> if txt /= "0" then mconcat ["| cost = ", Doc.strictText txt, PP.line] else mconcat [])
               cost_pp
        ,maybe mempty
               (\num -> mconcat ["| cooldown = ", Doc.strictText $ formatDays $ fromIntegral num, PP.line])
               days_re_enable
        ,maybe mempty
               (\num -> mconcat ["| days_mission_timeout = ", Doc.strictText $ formatDays $ fromIntegral num, PP.line])
               days_mission_timeout
        ,maybe mempty
               (\num -> mconcat ["| days_remove = ", Doc.strictText $ formatDays $ fromIntegral num, PP.line])
               days_remove] ++
        ( if isGood then
            ["| is_good = yes", PP.line]
        else []) ++
        ( if isFireOnlyOnce then
            ["| fire_only_once = yes", PP.line]
        else []) ++
        ( if isSelectableMission then
            ["| selectable_mission = yes", PP.line]
        else []) ++
        ( if cancelIfNotVisible then
            ["| cancel_if_not_visible = yes", PP.line]
        else []) ++
        (if not $ all null [allow_pp'd
            ,targetRootTrigger_pp'd
            ,visible_pp'd
            ,activation_pp'd
            ,targetTrigger_pp'd] then
            ["| visible =", PP.line]
        else []) ++
        allow_pp'd ++
        targetRootTrigger_pp'd ++
        visible_pp'd ++
        activation_pp'd ++
        targetTrigger_pp'd ++

        available_pp'd ++
        ppActivatedBy_pp'd ++
        cancelTrigger_pp'd ++
        targets ++
        effect_pp'd ++
        removeEffect_pp'd ++
        cancelEffect_pp'd ++
        timeoutEffect_pp'd ++

        (if not $ all null [modifier_pp'd, targetedModifier_pp'd] then
            ["| modifier =", PP.line]
        else []) ++
        modifier_pp'd ++
        targetedModifier_pp'd ++
        -- Everything the template has no parameter of its own for goes into
        -- the one comment it takes, which can only be given once.
        (let notes = custom_cost_trigger_note ++ removeTrigger_note
                        ++ (if targetsDynamic then ["<!-- targets_dynamic = yes -->", PP.line] else [])
                        ++ maybe [] (\awd_pp'd ->
                                ["<!-- AI decision factors:", PP.line
                                ,awd_pp'd, PP.line, "-->", PP.line]) mawd_pp'd
         in if null notes then [] else ["| comment = ", PP.line] ++ notes) ++
        ["}}", PP.line
        ,"<section end=", nameD, "/>", PP.line
        ]
    where
        extractTargets (StatementBare (GenericLhs e [])) = Just e
        extractTargets stmt = trace ("Unknown in targets array statement: " ++ show stmt) Nothing
        extractTargetsStates (StatementBare (IntLhs e)) = Just e
        extractTargetsStates [pdx| state = !e |] = Just e
        extractTargetsStates stmt = trace ("Unknown in targets states array statement: " ++ show stmt) Nothing


ppActivatedBy :: (HOI4Info g, Monad m) => Text -> PPT g m [Doc]
ppActivatedBy decisionId = do
    decisionTriggers <- getDecisionTriggers
    let mtriggers = HM.lookup decisionId decisionTriggers
    case mtriggers of
        Just triggers -> do
            -- The same source can activate a decision from several places in
            -- its script; one mention is enough.
            ts <- nubBy (\a b -> Doc.doc2text a == Doc.doc2text b) <$> mapM ppSource triggers
            -- FIXME: This is a bit ugly, but we only want a list if there's more than one trigger
            let ts' = if length ts < 2 then
                    ts
                else
                    map (\d -> Doc.strictText $ "* " <> Doc.doc2text d) ts
            return [mconcat $ ["| activated_by = "] ++ [PP.line] ++ intersperse PP.line ts' ++ [PP.line]]
        _ -> return [Doc.strictText ""]


