{-|
Module      : HOI4.EventSources
Description : Shared machinery for finding and presenting the game content
              that fires events and activates decisions

Events and decisions both want to list what can set them off, and both draw
on the same kinds of sources: event effects, decision effects, on_actions,
national focuses, ideas, advisors, scripted effects, balance-of-power ranges,
special projects, intelligence operations, raids, and resistance/compliance
modifiers. This module holds the single implementation of both the search
(the @find*@ functions) and the wiki-text presentation ('ppSource') for these
sources, keyed off the shared 'HOI4Source' type.
-}
module HOI4.EventSources (
        ppSource
    ,   formatWeight
    -- * Finding events fired by game content
    ,   findTriggeredEventsInEvents
    ,   findTriggeredEventsInDecisions
    ,   findTriggeredEventsInOnActions
    ,   findTriggeredEventsInNationalFocus
    ,   findTriggeredEventsInIdeas
    ,   findTriggeredEventsInCharacters
    ,   findTriggeredEventsInScriptedEffects
    ,   findTriggeredEventsInBops
    ,   findTriggeredEventsInGenericScripts
    -- * Finding decisions activated by game content
    ,   findActivatedDecisionsInEvents
    ,   findActivatedDecisionsInDecisions
    ,   findActivatedDecisionsInOnActions
    ,   findActivatedDecisionsInNationalFocus
    ,   findActivatedDecisionsInIdeas
    ,   findActivatedDecisionsInCharacters
    ,   findActivatedDecisionsInScriptedEffects
    ,   findActivatedDecisionsInBops
    ,   findActivatedDecisionsInGenericScripts
    ) where

import Debug.Trace (trace)

import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text (Doc)

import Abstract -- everything
import qualified Doc
import HOI4.Common (ppEventLoc, iquotes't)
import HOI4.Localization
import HOI4.Messages (wikifyLocColours)
import HOI4.Types
import QQ (pdx)
import SettingsTypes ( PPT
                     , getGameInterface, getGameInterfaceNamed, getGameInterfaceIfPresent)

-- | A map from event or decision id to the sources known to set it off.
type HOI4SourceMap = HashMap Text [HOI4Source]

formatWeight :: HOI4SourceWeight -> Text
formatWeight Nothing = ""
formatWeight (Just (n, d)) = T.pack (" (Base weight: " ++ show n ++ "/" ++ show d ++ ")")

-- | A decision's name with its pronouns filled in the way that decision reads
-- them, where its script pins them down. The name was localized before that
-- was known, and it is written for its own decision whatever page quotes it,
-- so an ambiguous pronoun keeps its brackets.
decNameLoc :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m Text
decNameLoc did loc = do
    decs <- getDecisions
    case HM.lookup did decs of
        Just dec -> withDecisionIdents dec (fillLocScopes loc)
        Nothing -> return loc

-- | Present one source as wiki text for a "triggered by"/"activated by" list.
ppSource :: (HOI4Info g, Monad m) => HOI4Source -> PPT g m Doc
ppSource (HOI4SrcOption eventId optionId) = do
    eventLoc <- ppEventLoc eventId
    -- The option's words are the event's own: its FROM is whoever fires that
    -- event, and whatever the pronouns mean on the page quoting it has no say.
    mfirer <- eventFirerTag eventId
    optLoc <- wikifyLocColours <$>
        (withRootIdent Nothing $ withFromIdent (ScopeValTag <$> mfirer) $
            getGameL10n optionId)
    return $ Doc.strictText $ mconcat [ "The event "
        , eventLoc
        , " option "
        , iquotes't optLoc
        ]
ppSource (HOI4SrcImmediate eventId) = do
    eventLoc <- ppEventLoc eventId
    return $ Doc.strictText $ mconcat [ "As an immediate effect of the "
        , eventLoc
        , " event"
        ]
ppSource (HOI4SrcAfter eventId) = do
    eventLoc <- ppEventLoc eventId
    return $ Doc.strictText $ mconcat [ "After choosing any option of the "
        , eventLoc
        , " event"
        ]
ppSource (HOI4SrcDecComplete id loc) = do
    locF <- decNameLoc id loc
    return $ Doc.strictText $ mconcat ["Taking the decision "
        , "<!-- "
        , id
        , " -->"
        , iquotes't locF
        ]
ppSource (HOI4SrcDecRemove id loc) = do
    locF <- decNameLoc id loc
    return $ Doc.strictText $ mconcat ["Finishing the decision "
        , "<!-- "
        , id
        , " -->"
        , iquotes't locF
        ]
ppSource (HOI4SrcDecCancel id loc) = do
    locF <- decNameLoc id loc
    return $ Doc.strictText $ mconcat ["Triggering the cancel trigger on the decision "
        , "<!-- "
        , id
        , " -->"
        , iquotes't locF
        ]
ppSource (HOI4SrcDecTimeout id loc) = do
    locF <- decNameLoc id loc
    return $ Doc.strictText $ mconcat ["Running out the timer on the decision "
        , "<!-- "
        , id
        , " -->"
        , iquotes't locF
        ]
ppSource (HOI4SrcOnAction act weight) = do
    actn <- actionName act
    return $ Doc.strictText $ actn <> formatWeight weight
ppSource (HOI4SrcNFComplete id loc icon) = do
    iconnf <- nfIcon id icon
    return $ Doc.strictText $ mconcat ["Completing the national focus "
        , iconnf
        , " <!-- "
        , id
        , " -->"
        , iquotes't loc
        ]
ppSource (HOI4SrcNFSelect id loc icon) = do
    iconnf <- nfIcon id icon
    return $ Doc.strictText $ mconcat ["Selecting the national focus "
        , iconnf
        , " <!-- "
        , id
        , " -->"
        , iquotes't loc
        ]
ppSource (HOI4SrcIdeaOnAdd id loc icon categ) = do
    iconnf <- ideaIcon id icon
    catloc <- getGameL10n categ
    return $ Doc.strictText $ mconcat ["When the "
        , catloc
        , " "
        , iconnf
        , " <!-- "
        , id
        , " -->"
        , iquotes't loc
        , " is added"
        ]
ppSource (HOI4SrcIdeaOnRemove id loc icon categ) = do
    iconnf <- ideaIcon id icon
    catloc <- getGameL10n categ
    return $ Doc.strictText $ mconcat ["When the "
        , catloc
        , " "
        , iconnf
        , " <!-- "
        , id
        , " -->"
        , iquotes't loc
        , " is removed"
        ]
ppSource (HOI4SrcCharacterOnAdd idtoken id name) = do
    loc <- advisorLoc idtoken name
    return $ Doc.strictText $ mconcat ["When the advisor "
        , " <!-- "
        , id
        , " "
        , idtoken
        , " -->"
        , iquotes't loc
        , " is added"
        ]
ppSource (HOI4SrcCharacterOnRemove idtoken id name) = do
    loc <- advisorLoc idtoken name
    return $ Doc.strictText $ mconcat ["When the advisor "
        , " <!-- "
        , id
        , " "
        , idtoken
        , " -->"
        , iquotes't loc
        , " is removed"
        ]
ppSource (HOI4SrcScriptedEffect id _weight) =
    return $ Doc.strictText $ mconcat ["When scripted effect "
        , iquotes't id
        , " is activated"
        ]
ppSource (HOI4SrcBopOnActivate id) = do
    loc <- getGameL10n id
    return $ Doc.strictText $ mconcat ["When reaching the "
        , "<!-- "
        , id
        , " -->"
        , iquotes't loc
        , " balance of power range"
        ]
ppSource (HOI4SrcBopOnDeactivate id) = do
    loc <- getGameL10n id
    return $ Doc.strictText $ mconcat ["When leaving the "
        , "<!-- "
        , id
        , " -->"
        , iquotes't loc
        , " balance of power range"
        ]
ppSource (HOI4SrcSpecialProject id) = do
    loc <- getGameL10n id
    return $ Doc.strictText $ mconcat ["From the special project "
        , "<!-- "
        , id
        , " -->"
        , iquotes't loc
        ]
ppSource (HOI4SrcOperation id) = do
    loc <- getGameL10n id
    return $ Doc.strictText $ mconcat ["From the operation "
        , "<!-- "
        , id
        , " -->"
        , iquotes't loc
        ]
ppSource (HOI4SrcRaid id) = do
    loc <- getGameL10n id
    return $ Doc.strictText $ mconcat ["From the raid "
        , "<!-- "
        , id
        , " -->"
        , iquotes't loc
        ]
ppSource (HOI4SrcComplianceMod id) = do
    loc <- getGameL10n id
    return $ Doc.strictText $ mconcat ["When the occupation modifier "
        , "<!-- "
        , id
        , " -->"
        , iquotes't loc
        , " takes effect"
        ]

-- | Icon for a national focus source.
nfIcon :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m Text
nfIcon id icon = do
    iconname <- do
        micon <- getGameInterfaceIfPresent ("GFX_focus_" <> id)
        case micon of
            Nothing -> getGameInterface "goal_unknown" icon
            Just idicon -> return idicon
    return $ "[[File:" <> iconname <> ".png|28px]]"

-- | Icon for an idea source.
ideaIcon :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m Text
ideaIcon id icon = do
    iconname <- do
        micon <- getGameInterfaceIfPresent ("GFX_idea_" <> id)
        case micon of
            Nothing -> getGameInterfaceNamed icon
            Just idicon -> return idicon
    return $ "[[File:" <> iconname <> ".png|28px]]"

-- | Name of an advisor source, preferring the character name over the token.
advisorLoc :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m Text
advisorLoc idtoken name = do
    mloc <- getGameL10nIfPresent name
    case mloc of
        Just nloc -> return nloc
        _ -> getGameL10n idtoken

-- | Human-readable name of an on_action hook.
actionName :: (HOI4Info g, Monad m) => Text -> PPT g m Text
actionName n
    | "on_monthly_" `T.isPrefixOf` n = tagAction "on_monthly_" "On every month for "
    | "on_weekly_" `T.isPrefixOf` n = tagAction "on_weekly_" "On every week for "
    | "on_daily_" `T.isPrefixOf` n = tagAction "on_daily_" "On every day for "
    | otherwise =
        return $ HM.findWithDefault ("<pre>" <> n <> "</pre>") n actionNameTable
    where
        tagAction prefix msg = do
            let tag = case T.stripPrefix prefix n of
                    Just nc -> nc
                    _ -> "<!-- Check game Script -->"
                actmsg = "<!-- " <> n <> " -->" <> msg
            tagloc <- flagText (Just HOI4Country) tag
            return $ actmsg <> tagloc

-- | Every on_action hook the game defines, with wiki text for each. The
-- commented id keeps the source greppable on the wiki.
actionNameTable :: HashMap Text Text
actionNameTable = HM.fromList $ map (\(n, t) -> (n, "<!-- " <> n <> " -->" <> t))
    [("on_ace_killed","On ace killed")
    ,("on_ace_killed_by_ace","On ace killed by enemy ace")
    ,("on_ace_killed_on_accident","On ace killed in accident")
    ,("on_ace_killed_other_ace","On ace kills enemy ace")
    ,("on_ace_promoted","On ace promoted")
    ,("on_aces_killed_each_other","On aces killed each other")
    ,("on_activated_active_decryption_bonuses","On activating decryption bonuses")
    ,("on_add_history","On history entry added")
    ,("on_annex", "On nation annexed")
    ,("on_army_leader_daily","On every day for army leader")
    ,("on_army_leader_lost_combat","On army leader loses combat")
    ,("on_army_leader_promoted","On army leader promoted")
    ,("on_army_leader_won_combat","On army leader wins combat")
    ,("on_assume_faction_leadership","On assuming faction leadership")
    ,("on_before_peace_conference_start","Before a peace conference starts")
    ,("on_border_war_lost","On lost border conflict war")
    ,("on_call_allies","On calling allies to war")
    ,("on_capitulation","On nation capitulation")
    ,("on_capitulation_immediate","On nation capitulation (immediate)")
    ,("on_civil_war_end","On civil war end")
    ,("on_civil_war_end_before_annexation","On civil war end before annexation")
    ,("on_coup_succeeded","On coup succeeded")
    ,("on_create_faction","On faction created")
    ,("on_daily","On every day")
    ,("on_declare_war","On declared war")
    ,("on_deployed_leader_defeated","On deployed leader defeated")
    ,("on_exile_government_reinstated","On exiled government reinstated")
    ,("on_faction_formed","On faction formed")
    ,("on_force_government","On government forced upon nation")
    ,("on_fully_decrypted_cipher","On fully decrypting a cipher")
    ,("on_government_change","On government changed")
    ,("on_government_exiled","On government exiled")
    ,("on_host_changed_from_capitulation","On exile host capitulating")
    ,("on_join_allies","On joining allies in war")
    ,("on_join_faction","On faction joined")
    ,("on_justifying_wargoal_pulse","On justifying wargoal")
    ,("on_leave_faction","On faction left")
    ,("on_liberate","On nation liberated")
    ,("on_mio_design_team_assigned_to_tech","On MIO design team assigned to research")
    ,("on_mio_design_team_assigned_to_variant","On MIO design team assigned to equipment variant")
    ,("on_mio_industrial_manufacturer_assigned","On MIO industrial manufacturer assigned")
    ,("on_mio_industrial_manufacturer_unassigned","On MIO industrial manufacturer unassigned")
    ,("on_mio_size_increased","On MIO size increased")
    ,("on_mio_tech_research_cancelled","On MIO research cancelled")
    ,("on_mio_tech_research_completed","On MIO research completed")
    ,("on_monthly","On every month")
    ,("on_naval_invasion","On naval invasion")
    ,("on_new_term_election","On new term election")
    ,("on_non_ace_killed_other_ace","On non-ace kills enemy ace")
    ,("on_nuke_drop","On nuke dropped")
    ,("on_offer_join_faction","On nation invited to faction")
    ,("on_operation_completed","On operation completed")
    ,("on_operative_captured","On operative captured")
    ,("on_operative_created","On operative created")
    ,("on_operative_death","On operative death")
    ,("on_operative_detected_during_operation","On operative detected during operation")
    ,("on_operative_on_mission_spotted","On operative spotted on mission")
    ,("on_operative_recruited","On operative recruited")
    ,("on_paradrop","On paradrop")
    ,("on_peace","On peace")
    ,("on_peaceconference_ended","On peace conference ended")
    ,("on_peaceconference_started","On peace conference started")
    ,("on_pride_of_the_fleet_sunk","On pride of the fleet sunk")
    ,("on_project_completion","On special project completed")
    ,("on_puppet","On nation puppeted")
    ,("on_recall_volunteers","On volunteers recalled")
    ,("on_release_as_free","On nation released as free nation")
    ,("on_release_as_puppet","On nation released as puppet")
    ,("on_ruling_party_change","On ruling party change")
    ,("on_ruling_party_change_immediate","On ruling party change (immediate)")
    ,("on_send_volunteers","On volunteers sent")
    ,("on_startup", "On startup")
    ,("on_state_control_changed","On state control changed")
    ,("on_subject_annex","On subject nation annexed")
    ,("on_subject_annexed","On subject nation annexed")
    ,("on_subject_autonomy_level_change","On subject autonomy level change")
    ,("on_subject_free","On subject nation freed")
    ,("on_uncapitulation","On nation no longer capitulated")
    ,("on_unit_leader_created","On army leader created")
    ,("on_unit_leader_level_up","On unit leader level up")
    ,("on_unit_leader_promote_from_ranks_green","On unit leader promoted from green ranks")
    ,("on_unit_leader_promote_from_ranks_veteran","On unit leader promoted from veteran ranks")
    ,("on_units_paradropped_in_state","On units paradropped in state")
    ,("on_war","On war started")
    ,("on_war_relation_added","On nation joined war")
    ,("on_wargoal_expire","On wargoal expired")
    ,("on_weekly","On every week")
    ]

--------------------------------------
-- Searching scripts for event ids  --
--------------------------------------

-- | Find events fired anywhere in a statement, with the weight they are
-- fired with where one applies (random_events blocks).
evtFindInStmt :: GenericStatement -> [(HOI4SourceWeight, Text)]
evtFindInStmt stmt@[pdx| $lhs = @scr |] | lhs `elem` eventEffects =
    maybe (trace ("Unrecognized event trigger: " ++ show stmt) [])
        (\triggeredId -> [(Nothing, triggeredId)])
        (getId scr)
    where
        getId :: [GenericStatement] -> Maybe Text
        getId [] = Nothing
        getId (stmt@[pdx| id = ?!id |] : _) = case id of
            Just (Left n) -> Just $ T.pack (show (n :: Int))
            Just (Right t) -> Just t
            _ -> trace ("Invalid event id statement: " ++ show stmt) Nothing
        getId (_ : ss) = getId ss
evtFindInStmt [pdx| $lhs = $id |]
    | lhs `elem` eventEffects || lhs `elem` ["on_win", "on_lose", "on_cancel"] =
        [(Nothing, id)]
evtFindInStmt [pdx| events = @scr |] = mapMaybe extractEvent scr
    where
        extractEvent :: GenericStatement -> Maybe (HOI4SourceWeight, Text)
        extractEvent (StatementBare (GenericLhs e [])) = Just (Nothing, e)
        extractEvent (StatementBare (IntLhs e)) = Just (Nothing, T.pack (show e))
        extractEvent stmt = trace ("Unknown in events statement: " ++ show stmt) Nothing
evtFindInStmt [pdx| random_events = @scr |] =
    let evts = mapMaybe extractRandomEvent scr
        total = sum $ map fst evts
    in map (\t -> (Just (fst t, total), snd t)) evts
    where
        extractRandomEvent :: GenericStatement -> Maybe (Integer, Text)
        extractRandomEvent stmt@[pdx| !weight = ?!id |] = case id of
            Just (Left n) -> Just (fromIntegral weight, T.pack (show (n :: Int)))
            Just (Right t) -> Just (fromIntegral weight, t)
            _ -> trace ("Invalid event id in random_events: " ++ show stmt) Nothing
        extractRandomEvent stmt = trace ("Unknown in random_events statement: " ++ show stmt) Nothing
evtFindInStmt [pdx| %lhs = @scr |] = concatMap evtFindInStmt scr
evtFindInStmt _ = []

-- | The effects that fire an event.
eventEffects :: [Text]
eventEffects = ["country_event", "news_event", "unit_leader_event", "state_event", "operative_leader_event"]

-- | Find decisions and missions activated anywhere in a statement.
decFindInStmt :: GenericStatement -> [(HOI4SourceWeight, Text)]
decFindInStmt [pdx| $lhs = $id |]
    | lhs == "activate_mission" || lhs == "activate_decision" = [(Nothing, id)]
decFindInStmt [pdx| activate_targeted_decision = @scr |] = mapMaybe getDec scr
    where
        getDec :: GenericStatement -> Maybe (HOI4SourceWeight, Text)
        getDec [pdx| decision = $id |] = Just (Nothing, id)
        getDec _ = Nothing
decFindInStmt [pdx| %_lhs = @scr |] = concatMap decFindInStmt scr
decFindInStmt _ = []

-- | How to search statements: 'evtFindInStmt' when building the event
-- triggers table, 'decFindInStmt' when building the decision triggers table.
type StmtFinder = GenericStatement -> [(HOI4SourceWeight, Text)]

findInStmts :: StmtFinder -> [GenericStatement] -> [(HOI4SourceWeight, Text)]
findInStmts = concatMap

addSource :: (HOI4SourceWeight -> HOI4Source) -> [(HOI4SourceWeight, Text)] -> [(Text, HOI4Source)]
addSource es = map (\t -> (snd t, es (fst t)))

findInOptions :: StmtFinder -> Text -> [HOI4Option] -> [(Text, HOI4Source)]
findInOptions finder eventId = concatMap (\o ->
    (\optName -> addSource (const (HOI4SrcOption eventId optName)) (maybe [] (findInStmts finder) (hoi4opt_effects o)))
    (fromMaybe "(Un-named option)" (hoi4opt_name o))
    )

addTriggers :: HOI4SourceMap -> [(Text, HOI4Source)] -> HOI4SourceMap
addTriggers hm l = foldl' ins hm l
    where
        ins :: HOI4SourceMap -> (Text, HOI4Source) -> HOI4SourceMap
        ins hm (k, v) = HM.alter (\case
            Just l -> Just $ l ++ [v]
            Nothing -> Just [v]) k hm

----------------------------------------
-- Search functions per feature kind  --
----------------------------------------

findSourcesInEvents :: StmtFinder -> HOI4SourceMap -> [HOI4Event] -> HOI4SourceMap
findSourcesInEvents finder hm evts = addTriggers hm (concatMap findInEvent evts)
    where
        findInEvent :: HOI4Event -> [(Text, HOI4Source)]
        findInEvent evt@HOI4Event{hoi4evt_id = Just eventId} =
            (case hoi4evt_options evt of
                Just opts -> findInOptions finder eventId opts
                _ -> []) ++
            addSource (const (HOI4SrcImmediate eventId)) (maybe [] (findInStmts finder) (hoi4evt_immediate evt)) ++
            addSource (const (HOI4SrcAfter eventId)) (maybe [] (findInStmts finder) (hoi4evt_after evt))
        findInEvent _ = []

findSourcesInDecisions :: StmtFinder -> HOI4SourceMap -> [HOI4Decision] -> HOI4SourceMap
findSourcesInDecisions finder hm ds = addTriggers hm (concatMap findInDecision ds)
    where
        findInDecision :: HOI4Decision -> [(Text, HOI4Source)]
        findInDecision d =
            addSource (const (HOI4SrcDecComplete (dec_name d) (dec_name_loc d))) (maybe [] (findInStmts finder) (dec_complete_effect d)) ++
            addSource (const (HOI4SrcDecRemove (dec_name d) (dec_name_loc d))) (maybe [] (findInStmts finder) (dec_remove_effect d)) ++
            addSource (const (HOI4SrcDecCancel (dec_name d) (dec_name_loc d))) (maybe [] (findInStmts finder) (dec_cancel_effect d)) ++
            addSource (const (HOI4SrcDecTimeout (dec_name d) (dec_name_loc d))) (maybe [] (findInStmts finder) (dec_timeout_effect d))

findSourcesInOnActions :: StmtFinder -> HOI4SourceMap -> [GenericStatement] -> HOI4SourceMap
findSourcesInOnActions finder hm scr = foldl' findInAction hm scr
    where
        findInAction :: HOI4SourceMap -> GenericStatement -> HOI4SourceMap
        findInAction hm [pdx| on_actions = @stmts |] = foldl' findInAction hm stmts
        findInAction hm [pdx| $lhs = @scr |] = addTriggers hm (addSource (HOI4SrcOnAction lhs) (findInStmts finder scr))
        findInAction hm stmt = trace ("Unknown on_actions statement: " ++ show stmt) hm

findSourcesInNationalFocus :: StmtFinder -> HOI4SourceMap -> [HOI4NationalFocus] -> HOI4SourceMap
findSourcesInNationalFocus finder hm nf = addTriggers hm (concatMap findInFocus nf)
    where
        findInFocus :: HOI4NationalFocus -> [(Text, HOI4Source)]
        findInFocus f =
            addSource (const (HOI4SrcNFComplete (nf_id f) (nf_name_loc f) (nf_icon f))) (maybe [] (findInStmts finder) (nf_completion_reward f)) ++
            addSource (const (HOI4SrcNFSelect (nf_id f) (nf_name_loc f) (nf_icon f))) (maybe [] (findInStmts finder) (nf_select_effect f))

findSourcesInIdeas :: StmtFinder -> HOI4SourceMap -> [HOI4Idea] -> HOI4SourceMap
findSourcesInIdeas finder hm ideas = addTriggers hm (concatMap findInIdea ideas)
    where
        findInIdea :: HOI4Idea -> [(Text, HOI4Source)]
        findInIdea idea =
            addSource (const (HOI4SrcIdeaOnAdd (id_id idea) (id_name_loc idea) (id_picture idea) (id_category idea))) (maybe [] (findInStmts finder) (id_on_add idea)) ++
            addSource (const (HOI4SrcIdeaOnRemove (id_id idea) (id_name_loc idea) (id_picture idea) (id_category idea))) (maybe [] (findInStmts finder) (id_on_remove idea))

findSourcesInCharacters :: StmtFinder -> HOI4SourceMap -> [HOI4Advisor] -> HOI4SourceMap
findSourcesInCharacters finder hm hChars = addTriggers hm (concatMap findInCharacter hChars)
    where
        findInCharacter :: HOI4Advisor -> [(Text, HOI4Source)]
        findInCharacter hChar =
            addSource (const (HOI4SrcCharacterOnAdd (adv_idea_token hChar) (adv_cha_id hChar) (adv_cha_name hChar))) (maybe [] (findInStmts finder) (adv_on_add hChar)) ++
            addSource (const (HOI4SrcCharacterOnRemove (adv_idea_token hChar) (adv_cha_id hChar) (adv_cha_name hChar))) (maybe [] (findInStmts finder) (adv_on_remove hChar))

findSourcesInScriptedEffects :: StmtFinder -> HOI4SourceMap -> [GenericStatement] -> HOI4SourceMap
findSourcesInScriptedEffects finder hm scr = foldl' findInScriptEffect hm scr
    where
        findInScriptEffect :: HOI4SourceMap -> GenericStatement -> HOI4SourceMap
        findInScriptEffect hm [pdx| $lhs = @scr |] = addTriggers hm (addSource (HOI4SrcScriptedEffect lhs) (findInStmts finder scr))
        findInScriptEffect hm stmt = trace ("Unknown scripted effect statement: " ++ show stmt) hm

findSourcesInBops :: StmtFinder -> HOI4SourceMap -> [HOI4BopRange] -> HOI4SourceMap
findSourcesInBops finder hm hBops = addTriggers hm (concatMap findInBop hBops)
    where
        findInBop :: HOI4BopRange -> [(Text, HOI4Source)]
        findInBop hBop =
            addSource (const (HOI4SrcBopOnActivate (bop_id hBop))) (maybe [] (findInStmts finder) (bop_on_activate hBop)) ++
            addSource (const (HOI4SrcBopOnDeactivate (bop_id hBop))) (maybe [] (findInStmts finder) (bop_on_deactivate hBop))

-- | Search a feature whose script files consist of top-level blocks keyed by
-- feature id: special projects, operations, raids (whose blocks sit inside a
-- @types@ wrapper), and resistance/compliance modifiers.
findSourcesInGenericScripts :: StmtFinder -> (Text -> HOI4Source) -> HOI4SourceMap -> [GenericStatement] -> HOI4SourceMap
findSourcesInGenericScripts finder mkSource hm scr = foldl' findInBlock hm scr
    where
        findInBlock :: HOI4SourceMap -> GenericStatement -> HOI4SourceMap
        findInBlock hm [pdx| types = @stmts |] = foldl' findInBlock hm stmts
        findInBlock hm [pdx| $id = @scr |] = addTriggers hm (addSource (const (mkSource id)) (findInStmts finder scr))
        findInBlock hm _ = hm

------------------------------
-- Exported finder aliases  --
------------------------------

findTriggeredEventsInEvents :: HOI4EventTriggers -> [HOI4Event] -> HOI4EventTriggers
findTriggeredEventsInEvents = findSourcesInEvents evtFindInStmt

findTriggeredEventsInDecisions :: HOI4EventTriggers -> [HOI4Decision] -> HOI4EventTriggers
findTriggeredEventsInDecisions = findSourcesInDecisions evtFindInStmt

findTriggeredEventsInOnActions :: HOI4EventTriggers -> [GenericStatement] -> HOI4EventTriggers
findTriggeredEventsInOnActions = findSourcesInOnActions evtFindInStmt

findTriggeredEventsInNationalFocus :: HOI4EventTriggers -> [HOI4NationalFocus] -> HOI4EventTriggers
findTriggeredEventsInNationalFocus = findSourcesInNationalFocus evtFindInStmt

findTriggeredEventsInIdeas :: HOI4EventTriggers -> [HOI4Idea] -> HOI4EventTriggers
findTriggeredEventsInIdeas = findSourcesInIdeas evtFindInStmt

findTriggeredEventsInCharacters :: HOI4EventTriggers -> [HOI4Advisor] -> HOI4EventTriggers
findTriggeredEventsInCharacters = findSourcesInCharacters evtFindInStmt

findTriggeredEventsInScriptedEffects :: HOI4EventTriggers -> [GenericStatement] -> HOI4EventTriggers
findTriggeredEventsInScriptedEffects = findSourcesInScriptedEffects evtFindInStmt

findTriggeredEventsInBops :: HOI4EventTriggers -> [HOI4BopRange] -> HOI4EventTriggers
findTriggeredEventsInBops = findSourcesInBops evtFindInStmt

findTriggeredEventsInGenericScripts :: (Text -> HOI4Source) -> HOI4EventTriggers -> [GenericStatement] -> HOI4EventTriggers
findTriggeredEventsInGenericScripts = findSourcesInGenericScripts evtFindInStmt

findActivatedDecisionsInEvents :: HOI4DecisionTriggers -> [HOI4Event] -> HOI4DecisionTriggers
findActivatedDecisionsInEvents = findSourcesInEvents decFindInStmt

findActivatedDecisionsInDecisions :: HOI4DecisionTriggers -> [HOI4Decision] -> HOI4DecisionTriggers
findActivatedDecisionsInDecisions = findSourcesInDecisions decFindInStmt

findActivatedDecisionsInOnActions :: HOI4DecisionTriggers -> [GenericStatement] -> HOI4DecisionTriggers
findActivatedDecisionsInOnActions = findSourcesInOnActions decFindInStmt

findActivatedDecisionsInNationalFocus :: HOI4DecisionTriggers -> [HOI4NationalFocus] -> HOI4DecisionTriggers
findActivatedDecisionsInNationalFocus = findSourcesInNationalFocus decFindInStmt

findActivatedDecisionsInIdeas :: HOI4DecisionTriggers -> [HOI4Idea] -> HOI4DecisionTriggers
findActivatedDecisionsInIdeas = findSourcesInIdeas decFindInStmt

findActivatedDecisionsInCharacters :: HOI4DecisionTriggers -> [HOI4Advisor] -> HOI4DecisionTriggers
findActivatedDecisionsInCharacters = findSourcesInCharacters decFindInStmt

findActivatedDecisionsInScriptedEffects :: HOI4DecisionTriggers -> [GenericStatement] -> HOI4DecisionTriggers
findActivatedDecisionsInScriptedEffects = findSourcesInScriptedEffects decFindInStmt

findActivatedDecisionsInBops :: HOI4DecisionTriggers -> [HOI4BopRange] -> HOI4DecisionTriggers
findActivatedDecisionsInBops = findSourcesInBops decFindInStmt

findActivatedDecisionsInGenericScripts :: (Text -> HOI4Source) -> HOI4DecisionTriggers -> [GenericStatement] -> HOI4DecisionTriggers
findActivatedDecisionsInGenericScripts = findSourcesInGenericScripts decFindInStmt
