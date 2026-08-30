{-|
Module      : HOI4.Events
Description : Feature handler for Hearts of Iron IV events
-}
module HOI4.Events (
        parseHOI4Events
    ,   writeHOI4Events
    ) where

import Debug.Trace (traceM)

import Control.Arrow ((&&&))
import Control.Monad (forM, foldM, when, (<=<))
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Char (isDigit)
import Data.List (intersperse, foldl', nubBy, sortBy)
import Data.Maybe (isJust, isNothing, fromMaybe, fromJust, catMaybes)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import HOI4.Common -- everything
import HOI4.EventSources (ppSource)
import HOI4.Localization
import HOI4.WikiPage ( CountryIndex, buildCountryIndex
                     , ppPageIntro, ppSectionHeader, boxWrapper, requiresDebug)
import FileIO ( Feature (..), Consolidation (..), ConsolidatedFeature (..)
              , naturalOrder, writeFeaturesWith)
import HOI4.Messages (imsg2doc, wikifyLocColours)
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..)
                     , IsGame (..), IsGameData (..)
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions )

-- | Empty event value. Starts off Nothing/empty everywhere.
newHOI4Event :: HOI4Scope -> FilePath -> HOI4Event
newHOI4Event escope = HOI4Event Nothing [] [] escope Nothing Nothing Nothing Nothing Nothing False False False Nothing Nothing Nothing
-- | Empty event option vaule. Starts off Nothing everywhere.
newHOI4Option :: HOI4Option
newHOI4Option = HOI4Option Nothing Nothing Nothing Nothing

-- | Take the event scripts from game data and parse them into event data
-- structures.
parseHOI4Events :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Event)
parseHOI4Events scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseHOI4Event scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing events: " ++ T.unpack err
            return HM.empty
        Right eventsFilesOrErrors ->
            flip HM.traverseWithKey eventsFilesOrErrors $ \sourceFile eevts -> do
                fmap (mkEvtMap . catMaybes) . forM eevts $ \case
                    Left err -> do
                        traceM $ "Error parsing events in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right mevt -> return mevt
                where mkEvtMap :: [HOI4Event] -> HashMap Text HOI4Event
                      mkEvtMap = HM.fromList . map (fromJust . hoi4evt_id &&& id)
                        -- Events returned from parseEvent are guaranteed to have an id.

-- | Present the parsed events as wiki text and write them to the appropriate
-- files. Also write one consolidated file per event folder, with a wiki
-- header before each event so the page gets a usable table of contents.
writeHOI4Events :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Events = do
    events <- getEvents
    countries <- buildCountryIndex
    let pathedEvents :: [Feature HOI4Event]
        pathedEvents = map (\evt -> Feature {
                                    featurePath = Just (hoi4evt_path evt)
                                ,   featureId = hoi4evt_id evt <> Just ".txt"
                                ,   theFeature = Right evt })
                            (HM.elems events)
    writeFeaturesWith "events"
                  pathedEvents
                  (\e -> scope (hoi4evt_scope e) $ ppEvent e)
                  (Just (ConsolidateHere (ppEventsPage countries)))

-- | Present one event file's events as a single wiki page. Events are
-- gathered into sections by their ids, so that a file of a hundred events
-- reads as a dozen headings rather than a hundred. Events that can only fire
-- in debug mode are left off the page, their sections with them if that
-- empties them; a file with nothing else in it makes no page at all.
ppEventsPage :: (HOI4Info g, Monad m) =>
    CountryIndex -> FilePath -> [ConsolidatedFeature HOI4Event] -> PPT g m (Maybe Doc)
ppEventsPage countries srcPath cfs =
    case filter (not . requiresDebug . hoi4evt_trigger . cfFeature) cfs of
        [] -> return Nothing
        shown -> do
            intro <- ppPageIntro countries "events" srcPath
            sections <- mapM ppEventSection (groupEvents shown)
            return . Just . mconcat $ intersperse PP.line (intro : sections)

-- | Present one section of an events page: its heading, then the events under
-- it wrapped so their boxes flow together.
ppEventSection :: (HOI4Info g, Monad m) =>
    (EventSection, [ConsolidatedFeature HOI4Event]) -> PPT g m Doc
ppEventSection (_, []) = return mempty
ppEventSection (_, cfs@(cf:rest)) = do
    header <- case rest of
        -- A section holding one event is named for the event, which reads
        -- better than a range of one. The title is read the way the event
        -- itself reads it, so a title naming its sender names them here too.
        [] -> do
            let eid = eventId cf
            mtitle <- case [key | HOI4EvtTitleSimple key <- hoi4evt_title (cfFeature cf)] of
                (key:_) -> withEventIdents eid (getGameL10nIfPresent key)
                _ -> return Nothing
            return $ ppSectionHeader (fromMaybe eid mtitle) (Just eid)
        _ -> return $ ppSectionHeader
                (eventId cf <> " – " <> eventId (last cfs)) Nothing
    return $ header <> PP.line <> boxWrapper (map cfDoc cfs)

-- | The section an event belongs to. Events whose ids share a namespace and
-- whose numbers fall in the same run of ten are listed together, so that
-- @foo.1@ to @foo.10@ make one section and @foo.1001@ to @foo.1010@ another.
-- An id with no number of its own -- @foo_skoda_priority@ -- stands alone.
data EventSection = EventSection Text Integer deriving (Eq, Ord)

eventSection :: Text -> EventSection
eventSection eid = case T.breakOnEnd "." eid of
    (namespace, number)
        | not (T.null namespace), not (T.null number), T.all isDigit number
            -> EventSection (T.dropEnd 1 namespace)
                            ((read (T.unpack number) - 1) `div` 10)
    _ -> EventSection eid 0

-- | An event's id, as its own file was named.
eventId :: ConsolidatedFeature HOI4Event -> Text
eventId = fromMaybe "(unknown)" . hoi4evt_id . cfFeature

-- | Gather a file's events into their sections, the sections in id order and
-- the events within each in id order too.
groupEvents :: [ConsolidatedFeature HOI4Event]
    -> [(EventSection, [ConsolidatedFeature HOI4Event])]
groupEvents cfs = M.toAscList $ M.map (sortBy (\a b -> naturalOrder (eventId a) (eventId b))) $
    M.fromListWith (++) [(eventSection (eventId cf), [cf]) | cf <- cfs]

-- | Parse a statement in an events file. Some statements aren't events; for
-- those, and for any obvious errors, return Right Nothing.
parseHOI4Event :: (HOI4Info g, MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4Event))
parseHOI4Event (StatementBare lhs) = withCurrentFile $ \f ->
        throwError $ T.pack (f ++ ": bare statement at top level: " ++ show lhs)
parseHOI4Event stmt@[pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs etype _ ->
            let mescope = case T.toLower etype of
                    "country_event" -> Just HOI4Country
                    "unit_leader_event" -> Just HOI4Country
                    "operative_leader_event" -> Just HOI4Operative
                    "state_event" -> Just HOI4ScopeState
                    "news_event" -> Just HOI4Country -- ?
                    "event" -> Just HOI4Country -- ?
                    _ -> Nothing
            in case mescope of
                Nothing -> throwError $ "unrecognized event type " <> etype
                Just escope -> withCurrentFile $ \file -> do
                    mevt <- hoistErrors (foldM eventAddSection (Just (newHOI4Event escope file)) parts)
                    case mevt of
                        Left err -> return (Left err)
                        Right Nothing -> return (Right Nothing)
                        Right (Just evt) ->
                            if isJust (hoi4evt_id evt)
                            then return (Right (Just evt))
                            else return (Left $ "error parsing events in " <> T.pack file
                                         <> ": missing event id")

    _ -> return (Right Nothing)
parseHOI4Event _ = throwError "operator other than ="

-- | Intermediate structure for interpreting event title blocks.
data EvtTitleI = EvtTitleI {
        eti_text :: Maybe Text
    ,   eti_trigger :: Maybe GenericScript
    }
-- | Interpret the @title@ section of an event. This can be either a
-- localization key or a conditional title block. (TODO: document the
-- format here)
evtTitle :: MonadError Text m => Maybe Text -> GenericScript -> m HOI4EvtTitle
evtTitle meid scr = case foldl' evtTitle' (EvtTitleI Nothing Nothing) scr of
        EvtTitleI (Just t) Nothing -- title = { text = foo }
            -> return $ HOI4EvtTitleSimple t
        EvtTitleI Nothing (Just trig) -- title = { trigger = { .. } } (invalid)
            -> return $ HOI4EvtTitleCompound scr
        EvtTitleI (Just t) (Just trig) -- title = { trigger = { .. } text = foo }
                                      -- e.g. pirate.1
            -> return $ HOI4EvtTitleConditional trig t
        EvtTitleI Nothing Nothing -- title = { switch { .. = { text = foo } } }
                                 -- e.g. action.39
            -> throwError $ "bad title: no trigger nor text" <> case meid of
                Just eid -> " in event " <> eid
                Nothing -> ""
    where
        evtTitle' ed [pdx| trigger = @trig |] = ed { eti_trigger = Just trig }
        evtTitle' ed [pdx| text = ?txt |] = ed { eti_text = Just txt }
        evtTitle' ed [pdx| title = ?txt |] = ed { eti_text = Just txt }
        evtTitle' ed [pdx| show_sound = %_ |] = ed
        evtTitle' ed [pdx| $label = %_ |]
            = error ("unrecognized title section " ++ T.unpack label
                     ++ " in " ++ maybe "(unknown)" T.unpack meid)
        evtTitle' ed stmt
            = error ("unrecognized title section in " ++ maybe "(unknown)" T.unpack meid
                    ++ ": " ++ show stmt)


-- | Intermediate structure for interpreting event description blocks.
data EvtDescI = EvtDescI {
        edi_text :: Maybe Text
    ,   edi_trigger :: Maybe GenericScript
    }
-- | Interpret the @desc@ section of an event. This can be either a
-- localization key or a conditional description block. (TODO: document the
-- format here)
evtDesc :: MonadError Text m => Maybe Text -> GenericScript -> m HOI4EvtDesc
evtDesc meid scr = case foldl' evtDesc' (EvtDescI Nothing Nothing) scr of
        EvtDescI (Just t) Nothing -- desc = { text = foo }
            -> return $ HOI4EvtDescSimple t
        EvtDescI Nothing (Just trig) -- desc = { trigger = { .. } } (invalid)
            -> return $ HOI4EvtDescCompound scr
        EvtDescI (Just t) (Just trig) -- desc = { trigger = { .. } text = foo }
                                      -- e.g. pirate.1
            -> return $ HOI4EvtDescConditional trig t
        EvtDescI Nothing Nothing -- desc = { switch { .. = { text = foo } } }
                                 -- e.g. action.39
            -> throwError $ "bad desc: no trigger nor text" <> case meid of
                Just eid -> " in event " <> eid
                Nothing -> ""
    where
        evtDesc' ed [pdx| trigger = @trig |] = ed { edi_trigger = Just trig }
        evtDesc' ed [pdx| text = ?txt |] = ed { edi_text = Just txt }
        evtDesc' ed [pdx| desc = ?txt |] = ed { edi_text = Just txt }
        evtDesc' ed [pdx| show_sound = %_ |] = ed
        evtDesc' ed [pdx| $label = %_ |]
            = error ("unrecognized desc section " ++ T.unpack label
                     ++ " in " ++ maybe "(unknown)" T.unpack meid)
        evtDesc' ed stmt
            = error ("unrecognized desc section in " ++ maybe "(unknown)" T.unpack meid
                    ++ ": " ++ show stmt)

-- | Interpret one section of an event. If understood, add it to the event
-- data. If not understood, throw an exception.
eventAddSection :: (HOI4Info g, MonadError Text m) =>
    Maybe HOI4Event -> GenericStatement -> PPT g m (Maybe HOI4Event)
eventAddSection Nothing _ = return Nothing
eventAddSection mevt stmt = sequence (eventAddSection' <$> mevt <*> pure stmt) where
    eventAddSection' evt stmt@[pdx| id = %rhs |]
        = case (textRhs rhs, floatRhs rhs) of
            (Just tid, _) -> return evt { hoi4evt_id = Just tid }
            (_, Just nid) -> return evt { hoi4evt_id = Just (T.pack $ show (nid::Int)) }
            _ -> withCurrentFile $ \file ->
                throwError $ "bad id in " <> T.pack file <> ": " <> T.pack (show rhs)
    eventAddSection' evt stmt@[pdx| title = %rhs |] =
        let oldtitles = hoi4evt_title evt in case rhs of
            (textRhs -> Just title) -> return evt { hoi4evt_title = oldtitles ++ [HOI4EvtTitleSimple title] }
            CompoundRhs scr -> do
                let meid = hoi4evt_id evt
                title <- evtTitle meid scr
                return evt { hoi4evt_title = oldtitles ++ [title] }
            _ -> throwError $ "bad title" <> maybe "" (" in event " <>) (hoi4evt_id evt)
    eventAddSection' evt stmt@[pdx| desc = %rhs |] =
        let olddescs = hoi4evt_desc evt in case rhs of
            (textRhs -> Just desc) -> return evt { hoi4evt_desc = olddescs ++ [HOI4EvtDescSimple desc] }
            CompoundRhs scr -> do
                let meid = hoi4evt_id evt
                desc <- evtDesc meid scr
                return evt { hoi4evt_desc = olddescs ++ [desc] }
            _ -> throwError $ "bad desc" <>  maybe "" (" in event " <>) (hoi4evt_id evt)
    eventAddSection' evt stmt@[pdx| $picture = %_ |] | T.toLower picture == "picture" = return evt
--  picture has conditions like desc. Ignore for now since we don't actually use it
--    eventAddSection' evt stmt@[pdx| picture = %rhs |] = case textRhs rhs of
--        Just pic -> return evt { hoi4evt_picture = Just pic }
--        _ -> throwError "bad picture"
    eventAddSection' evt stmt@[pdx| goto = %rhs |] = return evt
    eventAddSection' evt stmt@[pdx| trigger = %rhs |] = case rhs of
        CompoundRhs [] -> return evt
        CompoundRhs trigger_script -> case trigger_script of
            [] -> return evt -- empty, treat as if it wasn't there
            _ -> return evt { hoi4evt_trigger = Just trigger_script }
        _ -> throwError "bad event trigger"
    eventAddSection' evt stmt@[pdx| is_triggered_only = %rhs |] = case rhs of
        GenericRhs "yes" [] -> return evt { hoi4evt_is_triggered_only = Just True }
        -- no is the default, so I don't think this is ever used
        GenericRhs "no" [] -> return evt { hoi4evt_is_triggered_only = Just False }
        _ -> throwError "bad trigger"
    eventAddSection' evt stmt@[pdx| mean_time_to_happen = %rhs |] = case rhs of
        CompoundRhs [] -> return evt
        CompoundRhs mtth -> return evt { hoi4evt_mean_time_to_happen = Just mtth }
        _ -> throwError "bad MTTH"
    eventAddSection' evt stmt@[pdx| immediate = %rhs |] = case rhs of
        CompoundRhs [] -> return evt
        CompoundRhs immediate -> return evt { hoi4evt_immediate = Just immediate }
        _ -> throwError "bad immediate section"
    eventAddSection' evt stmt@[pdx| after = %rhs |] = case rhs of
        CompoundRhs [] -> return evt
        CompoundRhs after -> return evt { hoi4evt_after = Just after }
        _ -> throwError "bad after section"
    eventAddSection' evt stmt@[pdx| option = %rhs |] =  case rhs of
        CompoundRhs option -> do
            newHOI4Options <- addHOI4Option (hoi4evt_options evt) option
            return evt { hoi4evt_options = newHOI4Options }
        _ -> throwError "bad option"
    eventAddSection' evt stmt@[pdx| fire_only_once = %rhs |]
        | GenericRhs "yes" [] <- rhs = return evt { hoi4evt_fire_only_once = True }
        | GenericRhs "no"  [] <- rhs = return evt { hoi4evt_fire_only_once = False }
    eventAddSection' evt stmt@[pdx| major = %rhs |] = case rhs of
        GenericRhs "yes" [] -> return evt { hoi4evt_major = True }
        -- No is the default
        GenericRhs "no"  [] -> return evt { hoi4evt_major = False }
        _ -> throwError "bad major"
    eventAddSection' evt stmt@[pdx| show_major = %rhs |] = case rhs of
        CompoundRhs show_major_script -> case show_major_script of
            [] -> return evt -- empty, treat as if it wasn't there
            _ -> return evt { hoi4evt_show_major = Just show_major_script }
        _ -> throwError "bad event show_major"
    eventAddSection' evt stmt@[pdx| $hidden = %rhs |] | T.toLower hidden == "hidden" = case rhs of
        GenericRhs "yes" [] -> return evt { hoi4evt_hide_window = True }
        -- No is the default
        GenericRhs "no"  [] -> return evt { hoi4evt_hide_window = False }
        _ -> throwError "bad hidden"
    eventAddSection' evt stmt@[pdx| fire_for_sender = %rhs |] = case rhs of
        -- Yes is the default, so I don't think this is ever used
        GenericRhs "yes" [] -> return evt { hoi4evt_fire_for_sender = Just False }
        GenericRhs "no" [] -> return evt { hoi4evt_fire_for_sender = Just True }
        _ -> throwError "bad fire_for_sender"
    eventAddSection' evt stmt@[pdx| timeout_days = %_ |] = return evt
    eventAddSection' evt stmt@[pdx| minor_flavor = %_ |] = return evt -- unknown effect
    eventAddSection' evt stmt@[pdx| $label = %_ |] =
        withCurrentFile $ \file ->
            throwError $ "unrecognized event section in " <> T.pack file <> ": " <> label
    eventAddSection' evt stmt =
        withCurrentFile $ \file ->
            throwError $ "unrecognized event section in " <> T.pack file <> ": " <> T.pack (show stmt)

-- | Interpret an option block and append it to the list of options.
addHOI4Option :: (IsGame g, Monad m) => Maybe [HOI4Option] -> GenericScript -> PPT g m (Maybe [HOI4Option])
addHOI4Option Nothing opt = addHOI4Option (Just []) opt
addHOI4Option (Just opts) opt = do
    optn <- foldM optionAddStatement newHOI4Option opt
    return $ Just (opts ++ [optn])

-- | Interpret one section of an option block and add it to the option data.
optionAddStatement :: (IsGame g, Monad m) => HOI4Option -> GenericStatement -> PPT g m HOI4Option
optionAddStatement opt stmt@[pdx| name = ?name |]
    = return $ opt { hoi4opt_name = Just name }
optionAddStatement opt stmt@[pdx| ai_chance = @ai_chance |]
    = return $ opt { hoi4opt_ai_chance = Just (aiWillDo ai_chance) } -- hope can re-use the aiWilldo script
optionAddStatement opt stmt@[pdx| trigger = %rhs |] = case rhs of
    CompoundRhs [] -> return opt
    CompoundRhs trigger_script -> return $ opt { hoi4opt_trigger = Just trigger_script }
    _ -> return opt
optionAddStatement opt stmt = do
    -- Not a GenericLhs - presumably an effect.
    effects_pp'd <- setIsInEffect True (optionAddEffect (hoi4opt_effects opt) stmt)
    return $ opt { hoi4opt_effects = effects_pp'd }

-- | Append an effect to the effects of an option.
optionAddEffect :: Monad m => Maybe GenericScript -> GenericStatement -> PPT g m (Maybe GenericScript)
optionAddEffect Nothing stmt = optionAddEffect (Just []) stmt
optionAddEffect (Just effs) stmt = return $ Just (effs ++ [stmt])


-- | Present an event's title block.
ppTitles :: (HOI4Info g, Monad m) => Bool {- ^ Is this a hidden event? -}
                                -> [HOI4EvtTitle] -> Text -> PPT g m Doc
ppTitles _ [] eid = return $ "| event_name = " <> Doc.strictText eid
ppTitles False [HOI4EvtTitleSimple key] eid = ("| event_name = " <>) . Doc.strictText . Doc.nl2br . wikifyLocColours <$> getGameL10n key
ppTitles True [HOI4EvtTitleSimple key]eid  = ("| event_name = (Hidden) " <>) . Doc.strictText . Doc.nl2br . wikifyLocColours <$> getGameL10n key
ppTitles True _ eid = return "| event_name = (This event is hidden and has no title.)"
-- The wiki's event template has no parameter for a conditional title, so the
-- name it shows has to be one title rather than the conditions between them:
-- the one the event falls back on when no condition holds, or failing that the
-- first it can be given. The conditions are still written out below it.
ppTitles _ titles eid = do
    defaultTitle <- case [key | HOI4EvtTitleSimple key <- titles]
                        ++ [key | HOI4EvtTitleConditional _ key <- titles] of
        (key:_) -> Doc.nl2br . wikifyLocColours <$> getGameL10n key
        [] -> return eid
    conditions <- PP.vsep <$> mapM ppTitle titles
    return $ "| event_name = " <> Doc.strictText defaultTitle <> PP.line
        <> "| cond_event_name = yes" <> PP.line
        <> "| cond_name = " <> conditions
  where
    ppTitle (HOI4EvtTitleSimple key) = ("Otherwise:<br>:" <>) <$> fmtTitle key
    ppTitle (HOI4EvtTitleConditional scr key) = mconcat <$> sequenceA
        [pure "The following title is used if:", pure PP.line
        ,imsg2doc =<< ppMany scr, pure PP.line
        ,pure ":", fmtTitle key
        ]
    ppTitle (HOI4EvtTitleCompound scr) =
        imsg2doc =<< ppMany scr
    fmtTitle key = flip fmap (getGameL10nIfPresent key) $ \case
        Nothing -> Doc.strictText key
        Just txt -> "''" <> Doc.strictText (Doc.nl2br (wikifyLocColours txt)) <> "''"

-- | Present an event's description block.
ppDescs :: (HOI4Info g, Monad m) => Bool {- ^ Is this a hidden event? -}
                                -> [HOI4EvtDesc] -> PPT g m Doc
ppDescs True _ = return "| cond_event_text = (This event is hidden and has no description.)"
ppDescs _ [] = return "| event_text = (No description)"
ppDescs _ [HOI4EvtDescSimple key] = ("| event_text = " <>) . Doc.strictText . Doc.nl2br . wikifyLocColours <$> getGameL10n key
ppDescs _ descs = (("| cond_event_text = yes" <> PP.line <> "| event_text = ") <>) . PP.vsep <$> mapM ppDesc descs where
    ppDesc (HOI4EvtDescSimple key) = ("Otherwise:<br>:" <>) <$> fmtDesc key
    ppDesc (HOI4EvtDescConditional scr key) = mconcat <$> sequenceA
        [pure "The following description is used if:", pure PP.line
        ,imsg2doc =<< ppMany scr, pure PP.line
        ,pure ":", fmtDesc key
        ]
    ppDesc (HOI4EvtDescCompound scr) =
        imsg2doc =<< ppMany scr
    fmtDesc key = flip fmap (getGameL10nIfPresent key) $ \case
        Nothing -> Doc.strictText key
        Just txt -> "''" <> Doc.strictText (Doc.nl2br (wikifyLocColours txt)) <> "''"




ppTriggeredBy :: (HOI4Info g, Monad m) => Text -> [Doc] -> PPT g m Doc
ppTriggeredBy eventId trig = do
    eventTriggers <- getEventTriggers
    let mtriggers = HM.lookup eventId eventTriggers
    case mtriggers of
        Just triggers -> do
            -- The same source can fire an event from several places in its
            -- script; one mention is enough.
            ts <- nubBy (\a b -> Doc.doc2text a == Doc.doc2text b) <$> mapM ppSource triggers
            -- FIXME: This is a bit ugly, but we only want a list if there's more than one trigger
            let ts' = if length ts < 2 then
                    ts
                else
                    map (\d -> Doc.strictText $ "* " <> Doc.doc2text d) ts
            return $ mconcat $ PP.line : intersperse PP.line ts'
        _ -> if null trig then return $ Doc.strictText "(No triggers)" else return $ Doc.strictText "(Triggers in event)"

-- | Pretty-print an event. If some essential parts are missing from the data,
-- throw an exception.
-- | Run an action knowing what the pronouns mean inside the given event:
-- FROM is whoever fired it, known by name where everything that fires it
-- belongs to a single country. ROOT is the recipient, whom nothing outside
-- the game can name.
withEventIdents :: (HOI4Info g, Monad m) => Text -> PPT g m a -> PPT g m a
withEventIdents eid action = do
    mfirer <- eventFirerTag eid
    withFromIdent (ScopeValTag <$> mfirer) action

ppEvent :: forall g m. (HOI4Info g, MonadError Text m) =>
    HOI4Event -> PPT g m Doc
ppEvent evt = maybe
    (throwError "hoi4evt_id missing")
    (\eid -> setCurrentFile (hoi4evt_path evt) $ withEventIdents eid $ do
        -- Valid event
        version <- gets (gameVersion . getSettings)
        (conditional, options_pp'd) <- case hoi4evt_options evt of
            Just opts -> ppoptions (hoi4evt_hide_window evt) eid opts
            _ -> fixForNoOptions eid  --BC: less ugly fix for having no options for an event
        titleLoc <- ppTitles (hoi4evt_hide_window evt) (hoi4evt_title evt) eid -- get localisation of title
        descLoc <- ppDescs (hoi4evt_hide_window evt) (hoi4evt_desc evt)
        let evtArg :: Text -> (HOI4Event -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
            evtArg fieldname field fmt
                = maybe (return [])
                    (\field_content -> do
                        content_pp'd <- fmt field_content
                        return
                            ["| ", Doc.strictText fieldname, " = "
                            ,PP.line
                            ,content_pp'd
                            ,PP.line])
                    (field evt)
            isTriggeredOnly = fromMaybe False $ hoi4evt_is_triggered_only evt
            isFireOnlyOnce = hoi4evt_fire_only_once evt
            isMajor = hoi4evt_major evt
            isFireForSender = fromMaybe False $ hoi4evt_fire_for_sender evt
            evtId = Doc.strictText eid
        trigger_pp'd <- evtArg "trigger" hoi4evt_trigger ppScript
        showmajor_pp'd <- evtArg "trigger" hoi4evt_show_major ppScript
        mmtth_pp'd <- mapM (ppMtth isTriggeredOnly) (hoi4evt_mean_time_to_happen evt)
        immediate_pp'd <- setIsInEffect True (evtArg "immediate" hoi4evt_immediate ppScript)
        after_pp'd <- setIsInEffect True (evtArg "after" hoi4evt_after ppScript)
        triggered_pp <- ppTriggeredBy eid trigger_pp'd
        -- Keep track of incomplete events
        when (not isTriggeredOnly && isNothing mmtth_pp'd && null trigger_pp'd) $
            -- TODO: use logging instead of trace
            traceM ("warning: is_triggered_only, trigger, and mean_time_to_happen missing for event id " ++ T.unpack eid)
        when (isTriggeredOnly && Doc.doc2text triggered_pp == "(No triggers)" && isNothing mmtth_pp'd && null trigger_pp'd) $
            -- TODO: use logging instead of trace
            traceM ("warning: Event is is_triggered_only but no triggers or mean_time_to_happen found for event id " ++ T.unpack eid)
        when (isFireOnlyOnce && isMajor) $
            -- TODO: use logging instead of trace
            traceM ("warning: Event is fire_only_once and major " ++ T.unpack eid)
        return . mconcat $
            ["<section begin=", evtId, "/>", PP.line
            ,"{{Event", PP.line
            ,"| version = ", Doc.strictText version, PP.line
            ,"| event_id = ", evtId, PP.line
            ,titleLoc, PP.line
            ,descLoc, PP.line
            ] ++
            ( if isFireOnlyOnce then
                ["| fire_only_once = yes", PP.line]
            else []) ++
            ( if isMajor then
                ["| major = yes", PP.line]
            else []) ++
            ( if isFireForSender then
                ["| fire_for_sender = no", PP.line]
            else []) ++
            -- For triggered only events, mean_time_to_happen is not
            -- really mtth but instead describes weight modifiers, for
            -- scripts that trigger them with a probability based on a
            -- weight (e.g. on_bi_yearly_pulse).
            (if isTriggeredOnly then
                ["| triggered only = ", triggered_pp, PP.line
                ]
                ++ maybe [] (:[PP.line]) mmtth_pp'd
            else []) ++
            trigger_pp'd ++
            -- mean_time_to_happen is only really mtth if it's *not*
            -- triggered only.
            (if isTriggeredOnly then [] else case mmtth_pp'd of
                Nothing -> if not $ null trigger_pp'd then [] else ["| triggered only =", PP.line
                        ,"* Unknown (Missing MTTH and is_triggered_only)", PP.line]
                Just mtth_pp'd ->
                    ["| mtth = ", PP.line
                    ,mtth_pp'd, PP.line]) ++
            immediate_pp'd ++
            (if conditional then ["| option conditions = yes", PP.line] else []) ++
            -- option_conditions = no (not implemented yet)
            ["| options = ", options_pp'd, PP.line] ++
            after_pp'd ++
            ["| collapse = no", PP.line
            ,"}}", PP.line
            ,"<section end=", evtId, "/>", PP.line
            ])
    (hoi4evt_id evt)

fixForNoOptions :: Monad m => Text -> m (Bool, Doc)
fixForNoOptions eid = do --BC: less ugly fix for having no options for an event
    let evtId = Doc.strictText eid
        message = mconcat ["(no options for event ", evtId, ")"]
    return (False, message)
-- | Present the options of an event.
ppoptions :: (HOI4Info g, MonadError Text m) =>
    Bool -> Text -> [HOI4Option] -> PPT g m (Bool, Doc)
ppoptions hidden evtid opts = do
    let triggered = any (isJust . hoi4opt_trigger) opts
    options_pp'd <- mapM (ppoption evtid hidden triggered) opts
    return (triggered, mconcat . (PP.line:) . intersperse PP.line $ options_pp'd)

-- | Present a single event option.
ppoption :: (HOI4Info g, MonadError Text m) =>
    Text -> Bool -> Bool -> HOI4Option -> PPT g m Doc
ppoption evtid hidden triggered opt = do
    optNameLoc <- fmap wikifyLocColours <$> (getGameL10n `mapM` hoi4opt_name opt)
    case optNameLoc of
        -- NB: some options have no effect, e.g. start of Peasants' War.
        Just name_loc -> ok name_loc
        Nothing -> if hidden
            then ok "(Dummy option for hidden event)"
            else ok "(Dummy option for possibly AI or invisible actions)"
                --old thing-throwError $ "some required option sections missing in " <> evtid <> " - dumping: " <> T.pack (show opt)
    where
        ok name_loc = let mtrigger = hoi4opt_trigger opt in do
            mawd_pp'd   <- mapM (imsg2doc <=< ppAiWillDo) (hoi4opt_ai_chance opt)
            effects_pp'd <- setIsInEffect True (ppScript (fromMaybe [] (hoi4opt_effects opt)))
            mtrigger_pp'd <- sequence (ppScript <$> mtrigger)
            return . mconcat $
                ["{{Option",PP.line
                ,"| option_text = ", Doc.strictText name_loc, PP.line
                ,"| effect =", PP.line, effects_pp'd, PP.line]
                ++ (if triggered then
                        maybe
                            ["| trigger = always", PP.line] -- no trigger
                        (\trigger_pp'd ->
                            ["| trigger = ", PP.line -- trigger
                            ,trigger_pp'd, PP.line]
                        ) mtrigger_pp'd
                    else [])
                ++
                maybe [] (\awd_pp'd ->
                    ["| aichance = ", PP.line
                    ,awd_pp'd, PP.line]) mawd_pp'd ++
                -- 1 = no
                ["}}"
                ]

