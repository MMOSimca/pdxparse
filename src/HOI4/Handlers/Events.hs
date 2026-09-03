{-|
Module      : HOI4.Handlers.Events
Description : Events, decisions and national focuses

Handlers for firing an event, naming a decision, and the state of a
national focus.
-}
module HOI4.Handlers.Events (
        triggerEvent
    ,   focusProgress
    ,   handleFocus
    ,   focusUncomplete
    ,   loadFocusTree
    ,   addHistoryEntry
    ,   locandid
    ,   unlockDecisionTooltip
    ,   reduceFocusCompletionCost
    ) where

import Data.Char (chr)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative ((<|>))
import Control.Monad (foldM)

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (typewriterText, plainPc, reducedNum, formatHours)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, concatMapM, getGameInterface, getGameInterfaceIfPresent)
import StatementUtils -- everything

import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything
import HOI4.WikiTables (focusPages, focusPageSplits, focusTagPages)

import HOI4.Handlers.Core (getbaretraits, msgToPP, noloc, preMessage, preStatement)
import HOI4.Handlers.Generic (textAtom, withNonlocAtom)

-- Events

data TriggerEvent = TriggerEvent
        { e_id :: Maybe Text
        , e_title_loc :: Maybe Text
        , e_days :: Maybe Double
        , e_months :: Maybe Double
        , e_hours :: Maybe Double
        , e_random :: Maybe Double
        , e_random_days :: Maybe Double
        , e_random_hours :: Maybe Double
        , e_for_controller :: Bool
        }

newTriggerEvent :: TriggerEvent
newTriggerEvent = TriggerEvent Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False

-- | Handler for effects that fire an event. The Bool says whether ROOT inside
-- the event being fired is the scope it is fired in -- true of country and
-- state events, while a news event's ROOT is each country viewing it.
triggerEvent :: forall g m. (HOI4Info g, Monad m) => Bool -> ScriptMessage -> StatementHandler g m
triggerEvent rootIsRecipient evtType stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_trigger_event =<< foldM addLine newTriggerEvent scr
    where
        addLine :: TriggerEvent -> GenericStatement -> PPT g m TriggerEvent
        addLine evt [pdx| id = ?!eeid |]
            | Just eid <- either (\n -> T.pack (show (n::Int))) id <$> eeid
            = do
                mevt_t <- firedEventContext rootIsRecipient (getEventTitle eid)
                return evt { e_id = Just eid, e_title_loc = mevt_t }
        addLine evt [pdx| days = %rhs |]
            = return evt { e_days = floatRhs rhs }
        addLine evt [pdx| months = %rhs |]
            = return evt { e_months = floatRhs rhs }
        addLine evt [pdx| hours = %rhs |]
            = return evt { e_hours = floatRhs rhs }
        addLine evt [pdx| random = %rhs |]
            = return evt { e_random = floatRhs rhs }
        addLine evt [pdx| random_days = %rhs |]
            = return evt { e_random_days = floatRhs rhs }
        addLine evt [pdx| random_hours = %rhs |]
            = return evt { e_random_hours = floatRhs rhs }
        -- A state event is fired for the state's owner unless script says to
        -- fire it for the controller instead.
        addLine evt [pdx| trigger_for = $whom |]
            | T.toLower whom == "controller" = return evt { e_for_controller = True }
            | T.toLower whom == "owner" = return evt
        -- Points the fired event's FROM.FROM at a country of the script's
        -- choosing. That only moves a pronoun inside the event being fired;
        -- nothing about the firing itself changes.
        addLine evt [pdx| set_from_from = %_ |] = return evt
        addLine evt [pdx| set_from = %_ |] = return evt
        addLine evt stmt = warn (UnknownSection "trigger event" stmt) (return evt)
        pp_trigger_event :: TriggerEvent -> PPT g m ScriptMessage
        pp_trigger_event evt = do
            evtType_msgt <- messageText evtType
            -- Script may delay the event in months; the wiki counts a month
            -- as 30 days, as the game's tooltips do.
            let evtType_t = evtType_msgt
                    <> (if e_for_controller evt then " (for its controller)" else "")
            case e_id evt of
                Just msgid ->
                    let loc = fromMaybe msgid (e_title_loc evt)
                        time = (fromMaybe 0 (e_days evt) + fromMaybe 0 (e_months evt) * 30) * 24 + fromMaybe 0 (e_hours evt)
                        timernd = time + fromMaybe 0 (e_random_days evt) * 24 + fromMaybe 0 (e_random evt) + fromMaybe 0 (e_hours evt)
                        tottimer = formatHours time <> if timernd /= time then " to " <> formatHours timernd else ""
                    in if time > 0 then
                        return $ MsgTriggerEventTime evtType_t msgid loc tottimer
                    else
                        return $ MsgTriggerEvent evtType_t msgid loc
                _ -> return $ preMessage stmt
triggerEvent rootIsRecipient evtType stmt@[pdx| %_ = ?!rid |]
    = msgToPP =<< pp_trigger_event =<< addLine newTriggerEvent rid
    where
        addLine :: TriggerEvent -> Maybe (Either Int Text) -> PPT g m TriggerEvent
        addLine evt eeid
            | Just eid <- either (\n -> T.pack (show (n::Int))) id <$> eeid
            = do
                mevt_t <- firedEventContext rootIsRecipient (getEventTitle eid)
                return evt { e_id = Just eid, e_title_loc = mevt_t }
        addLine evt _ = return evt
        pp_trigger_event :: TriggerEvent -> PPT g m ScriptMessage
        pp_trigger_event evt = do
            evtType_t <- messageText evtType
            case e_id evt of
                Just msgid -> do
                    let loc = fromMaybe msgid (e_title_loc evt)
                    return $ MsgTriggerEvent evtType_t msgid loc
                _ -> return $ preMessage stmt
triggerEvent _ _ stmt = preStatement stmt

-- | Read something of the event being fired -- its title -- with the
-- pronouns meaning what they do inside that event: its ROOT is the scope the
-- effect fires it in, where that is who ROOT is for its type of event, and
-- its FROM is whoever is firing -- the ROOT of the script in hand. What the
-- pronouns meant out here must not leak into text that is not written for
-- here, so both are set even when the answer is that nothing is known.
firedEventContext :: (HOI4Info g, Monad m) => Bool -> PPT g m a -> PPT g m a
firedEventContext rootIsRecipient action = do
    firer <- getRootIdent
    recip <- if rootIsRecipient then getThisIdent else return Nothing
    withRootIdent recip $ withFromIdent firer action

---------------
-- has focus --
---------------

-- | The icon, key and localized name of the national focus given, or 'Nothing'
-- for a focus we know nothing about.
focusIconKeyLoc :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe (Text, Text, Text))
focusIconKeyLoc nf = do
    nfs <- getNationalFocus
    case HM.lookup nf nfs of
        -- Not a focus of a tree we read: one built at run time, or one whose
        -- tree we do not parse. The name it is written under is the key the
        -- game localizes it by, so the line can still be written without the
        -- entry; only where there is no localization either is it beyond us.
        Nothing -> do
            mloc <- getGameL10nIfPresent nf
            case mloc of
                Nothing -> return Nothing -- unknown national focus
                Just nf_loc -> do
                    nfIcon <- getGameInterface "goal_unknown" ("GFX_focus_" <> nf)
                    return $ Just (nfIcon, nf, nf_loc)
        Just nnf -> do
            let nfKey = nf_id nnf
            nfIcon <- do
                micon <- getGameInterfaceIfPresent ("GFX_focus_" <> nfKey)
                case micon of
                    Nothing -> getGameInterface "goal_unknown" (nf_icon nnf)
                    Just idicon -> return idicon
            nf_loc <- getGameL10n nfKey
            return $ Just (nfIcon, nfKey, nf_loc)

focusProgress :: (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
focusProgress msg stmt@[pdx| $lhs = @compa |] = do
    let nf = fromMaybe "<!-- Check Script -->" (getfoc compa)
        compare = fromMaybe "<!-- Check Script -->" (getcomp compa)
    mfoc <- focusIconKeyLoc nf
    case mfoc of
        Nothing -> preStatement stmt
        Just (nfIcon, nfKey, nf_loc) -> msgToPP (msg nfIcon nfKey nf_loc compare)
    where
        getfoc :: [GenericStatement] -> Maybe Text
        getfoc [] = Nothing
        getfoc (stmt@[pdx| focus = $id |] : _) = Just id
        getfoc (_ : ss) = getfoc ss
        getcomp :: [GenericStatement] -> Maybe Text
        getcomp [] = Nothing
        getcomp (stmt@[pdx| progress > !num |] : _)
            = Just $ "more than " <> Doc.doc2text (reducedNum plainPc num)
        getcomp (stmt@[pdx| progress < !num |] : _)
            = Just $ "less than " <> Doc.doc2text (reducedNum plainPc num)
        getcomp (_ : ss) = getcomp ss
focusProgress _ stmt = preStatement stmt

handleFocus :: (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
handleFocus msg stmt@[pdx| $lhs = $nf |] = do
    mfoc <- focusIconKeyLoc nf
    case mfoc of
        Nothing -> preStatement stmt -- unknown national focus
        Just (nfIcon, nfKey, nf_loc) -> msgToPP (msg nfIcon nfKey nf_loc)
-- | Some effects take the focus inside a block, along with fields that say how
-- the game is to tell the player about it. The focus is the only part of that
-- worth reading on the wiki.
handleFocus msg stmt@[pdx| %_ = @scr |] =
    case [inner | inner@[pdx| focus = %_ |] <- scr] of
        (focstmt : _) -> handleFocus msg focstmt
        [] -> preStatement stmt
handleFocus _ stmt = preStatement stmt

data UncFoc = UncFoc
        {   uf_focus :: Text
        ,   uf_uncomplete_children :: Bool
        }

newUF :: UncFoc
newUF = UncFoc "<!-- Check Game Script -->" False

focusUncomplete :: (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> Bool -> ScriptMessage)
        -> StatementHandler g m
focusUncomplete msg stmt@[pdx| $lhs = @scr |] = do
    msgToPP =<< ppuf (foldl' addLine newUF scr)
    where
        addLine :: UncFoc -> GenericStatement -> UncFoc
        addLine uf [pdx| focus = ?what |] = uf { uf_focus =  what }
        addLine uf [pdx| uncomplete_children = %rhs |]
            | GenericRhs "yes" [] <- rhs = uf { uf_uncomplete_children = True }
            | GenericRhs "no"  [] <- rhs = uf { uf_uncomplete_children = False }
        addLine uf [pdx| refund_political_power = %_ |] = uf
        addLine uf scr = warn (UnknownSection "uncomplete_national_focus" scr) uf

        ppuf uf = do
            mfoc <- focusIconKeyLoc (uf_focus uf)
            case mfoc of
                Nothing -> return $ preMessage stmt -- unknown national focus
                Just (nfIcon, nfKey, nf_loc) ->
                    return $ msg nfIcon nfKey nf_loc (uf_uncomplete_children uf)
focusUncomplete _ stmt = preStatement stmt

---------------------------------
-- Handler for load_focus_tree --
---------------------------------

loadFocusTree :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
loadFocusTree stmt@[pdx| %_ = $txt |] = withNonlocAtom MsgLoadFocusTree stmt
loadFocusTree stmt@[pdx| %_ = @scr |] = case extractStmt (matchLhsText "keep_completed") scr of
    (Just keep,_)-> textAtom "tree" "keep_completed" MsgLoadFocusTreeKeep noloc stmt
    _-> case extractStmt (matchLhsText "tree") scr of
        (Just tree,_) -> withNonlocAtom MsgLoadFocusTree tree
        _-> preStatement stmt
loadFocusTree stmt = preStatement stmt

-----------------------------------
-- handler for add_history_entry --
-----------------------------------

addHistoryEntry :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addHistoryEntry stmt@[pdx| %_ = @scr |] =
    case fst (extractStmt (matchLhsText "key") scr) of
        Just [pdx| %_ = ?key |] -> msgToPP . MsgAddHistoryEntry =<< getGameL10n key
        _ -> preStatement stmt
addHistoryEntry stmt = preStatement stmt

locandid :: (Monad m, HOI4Info g) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
locandid msg [pdx| %_ = ?key |] = do
    -- The name is the other decision's: its FROM is that decision's own
    -- target, and it is drawn for whoever the script in hand works it for --
    -- the current scope.
    decs <- getDecisions
    mthis <- getThisIdent
    loc <- withRootIdent mthis $
        withFromIdent (decTargetIdent =<< HM.lookup key decs) $
            getGameL10n key
    msgToPP $ msg loc key
locandid _ stmt = preStatement stmt

-- unlock decision tooltip

unlockDecisionTooltip :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
unlockDecisionTooltip stmt@[pdx| %_ = $_ |] = locandid MsgUnlockDecisionTooltip stmt
unlockDecisionTooltip stmt@[pdx| %_ = @scr |] = do
    let (mname, _) = extractStmt (matchLhsText "decision") scr
    case mname of
        Just stmt@[pdx| %_ = ?txt |] -> locandid MsgUnlockDecisionTooltip stmt
        _ -> preStatement stmt
unlockDecisionTooltip stmt = preStatement stmt

-------------------------------------
-- Handler for national focus costs --
-------------------------------------

-- | Handler for @reduce_focus_completion_cost@, which takes days off however many
-- focuses are named in it. The focuses are written as links to where each stands
-- in its tree, so that a reader can go and see which ones were sped up.
reduceFocusCompletionCost :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
reduceFocusCompletionCost stmt@[pdx| %_ = @scr |] = case cost of
    Nothing -> preStatement stmt
    Just days -> do
        basemsg <- msgToPP (MsgReduceFocusCompletionCost days)
        links <- indentUp (concatMapM focusLink focuses)
        return (basemsg ++ links)
    where
        (cost, focuses) = foldl' addLine (Nothing, []) scr
        addLine (c, fs) [pdx| cost = !n |] = (Just n, fs)
        addLine (c, fs) [pdx| focus = @arr |] = (c, fs ++ mapMaybe getbaretraits arr)
        addLine (c, fs) [pdx| focus = $theid |] = (c, fs ++ [theid])
        addLine acc stmt = warn (UnknownSection "reduce_focus_completion_cost" stmt) acc
reduceFocusCompletionCost stmt = preStatement stmt

-- | One focus written the way the wiki writes it: a template call naming the page
-- it is written up on and the focus itself. A focus on no page of the wiki falls
-- back to its icon and name written out, which is how focuses are named
-- everywhere else.
focusLink :: (HOI4Info g, Monad m) => Text -> PPT g m IndentedMessages
focusLink theid = do
    focuses <- getNationalFocus
    case HM.lookup theid focuses of
        Just nf | Just page <- focusPage focuses nf -> msgToPP (MsgFocusLink page theid)
        Just nf -> msgToPP (MsgFocusNamed (nf_icon nf) theid (nf_name_loc nf))
        Nothing -> msgToPP (MsgUnprocessed (typewriterText theid))

-- | The wiki page a focus is written up on. Which file script keeps a focus in
-- says which page it belongs to, bar three files the wiki writes up over two
-- pages each: Spain's two sides are told apart by the tag their ids carry, and
-- Germany's and the Soviet Union's halves each run in one stretch, so the focus
-- the second half opens with says where the break falls.
focusPage :: HashMap Text HOI4NationalFocus -> HOI4NationalFocus -> Maybe Text
focusPage focuses nf = byTag <|> bySplit <|> HM.lookup file focusPages
    where
        file = T.toLower (T.takeWhileEnd (\c -> c /= '/' && c /= (chr 92)) (T.pack (nf_path nf)))
        byTag = listToMaybe
            [ page | (tag, page) <- focusTagPages, tag `T.isPrefixOf` nf_id nf ]
        bySplit = do
            (before, marker, from) <- HM.lookup file focusPageSplits
            split <- HM.lookup marker focuses
            return (if nf_ordinal nf >= nf_ordinal split then from else before)
