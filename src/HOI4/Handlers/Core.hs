{-|
Module      : HOI4.Handlers.Core
Description : What every statement handler is built from

The pieces every handler shares: putting a message out at the current
indent, the @<pre>@ fallback for script that is not understood, the
compound-statement combinators that head a block of lines, the helpers that
turn a condition into readable prose, and the small readers of a statement's
parts that many handlers use.
-}
module HOI4.Handlers.Core (
        preStatement
    ,   plainStatement
    ,   preStatementText'
    ,   preMessage
    ,   plainMsg
    ,   plainMsg'
    ,   msgToPP
    ,   msgToPP'
    ,   headed
    ,   compound
    ,   compoundMessage
    ,   prioritize
    ,   compoundMessageScope
    ,   compoundMessageCondition
    ,   limitClause
    ,   isClause
    ,   isThirdPerson
    ,   compoundMessageNot
    ,   joinClauses
    ,   lowerFirst
    ,   compoundMessageExtractTag
    ,   compoundMessageExtract
    ,   compoundMessageExtractNum
    ,   compoundMessagePronoun
    ,   compoundMessageTagged
    ,   rhsAlways
    ,   rhsAlwaysYes
    ,   rhsYesOrScope
    ,   rhsIgnored
    ,   constantOrNumber
    ,   noloc
    ,   bareAtom
    ,   bareInt
    ,   thisContext
    ,   statedFrom
    ,   getbaretraits
    ,   sectionLink
    ,   tooltipText
    ) where

import Data.Char (isAlpha, isSpace, isUpper, toLower)
import Data.Foldable (fold)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Text.PrettyPrint.Leijen.Text as PP

import Debug.Trace

import Abstract -- everything
import Doc (Doc)
import qualified Doc -- everything
import MessageTools (typewriterText)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, IsGameState (..), GameState (..), scope, withCurrentIndent
                     , alsoIndent', withCurrentFile, unsnoc)
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany, ppOne)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

-- | Pretty-print a script statement, wrap it in a @<pre>@ element, and emit a
-- generic message for it at the current indentation level. This is the
-- fallback in case we haven't implemented that particular statement or we
-- failed to understand it.
--
-- Will now try to recurse into nested clauses as they break the wiki layout, and
-- it might be possible to "recover".
preStatement :: (HOI4Info g, Monad m) =>
    GenericStatement -> PPT g m IndentedMessages
preStatement [pdx| %lhs = @scr |] = do
    headerMsg <- plainMsg' $ "<pre>" <> Doc.doc2text (lhs2doc (const "") lhs) <> "</pre>"
    msgs <- ppMany scr
    return (headerMsg : msgs)
preStatement stmt = (:[]) <$> alsoIndent' (preMessage stmt)

-- | For pretty printing a simple statement without <pre>
plainStatement :: (HOI4Info g, Monad m) =>
    Text -> GenericStatement -> PPT g m IndentedMessages
plainStatement xtxt stmt =
    plainMsg $ xtxt <> typewriterText (Doc.doc2text (genericStatement2doc stmt))

-- | Pretty-print a statement and wrap it in a @<pre>@ element.
preStatementText :: GenericStatement -> Doc
preStatementText stmt = "<pre>" <> genericStatement2doc stmt <> "</pre>"

-- | 'Text' version of 'preStatementText'.
preStatementText' :: GenericStatement -> Text
preStatementText' = Doc.doc2text . preStatementText

-- | Pretty-print a script statement, wrap it in a @<pre>@ element, and emit a
-- generic message for it.
preMessage :: GenericStatement -> ScriptMessage
preMessage = MsgUnprocessed
            . TL.toStrict
            . PP.displayT
            . PP.renderPretty 0.8 80 -- Don't use 'Doc.doc2text', because it uses
                                     -- 'Doc.renderCompact' which is not what
                                     -- we want here.
            . preStatementText

-- | Create a generic message from a piece of text. The rendering function will
-- pass this through unaltered.
plainMsg :: (IsGameState (GameState g), Monad m) => Text -> PPT g m IndentedMessages
plainMsg msg = (:[]) <$> plainMsg' msg

plainMsg' :: (IsGameState (GameState g), Monad m) => Text -> PPT g m IndentedMessage
plainMsg' = alsoIndent' . MsgUnprocessed

msgToPP :: (IsGameState (GameState g), Monad m) => ScriptMessage -> PPT g m IndentedMessages
msgToPP msg = (:[]) <$> msgToPP' msg

msgToPP' :: (IsGameState (GameState g), Monad m) => ScriptMessage -> PPT g m IndentedMessage
msgToPP' = alsoIndent'

--------------------------------
-- General statement handlers --
--------------------------------

-- | Put a header over the lines of a block, dropping the header along with the
-- block where it would be left standing over nothing.
--
-- A block whose contents all come to nothing -- script's own book-keeping,
-- such as telling the game to draw the focus tree again -- would otherwise
-- leave a header over an empty list, which tells the reader nothing and reads
-- as though something had gone missing.
--
-- Handlers whose header names the thing being asked after do not use this: a
-- scope trigger with no conditions written under it asks whether there is
-- anything there at all -- whether any state lies in the region an
-- @any_state_in@ names -- and that question is the whole of what it says.
headed :: Int -> ScriptMessage -> IndentedMessages -> IndentedMessages
headed _ _ [] = []
headed i header body = (i, header) : body

-- | Generic handler for a simple compound statement. Usually you should use
-- 'compoundMessage' instead so the text can be localized.
compound :: (HOI4Info g, Monad m) =>
    Text -- ^ Text to use as the block header, without the trailing colon
    -> StatementHandler g m
compound header [pdx| %_ = @scr |]
    = withCurrentIndent $ \_ -> do -- force indent level at least 1
        headerMsg <- plainMsg (header <> ":")
        scriptMsgs <- ppMany scr
        return $ if null scriptMsgs then [] else headerMsg ++ scriptMsgs
compound _ stmt = preStatement stmt

-- | Generic handler for a simple compound statement.
compoundMessage :: (HOI4Info g, Monad m) =>
    ScriptMessage -- ^ Message to use as the block header
    -> StatementHandler g m
compoundMessage header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        script_pp'd <- ppMany scr
        return (headed i header script_pp'd)
compoundMessage _ stmt = preStatement stmt

-- | Handler for @prioritize@, which names the states a scope should pick from
-- first. Defined here rather than with the other handlers below because
-- 'compoundMessageScope' folds it into the header line it belongs to, and a
-- Template Haskell splice further down would otherwise put it out of scope.
prioritize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
prioritize stmt@[pdx| %_ = @arr |] = do
                let states = mapMaybe stateFromArray arr
                    stateFromArray (StatementBare (IntLhs e)) = Just e
                    stateFromArray stmt = warn (UnknownSection "prioritize array" stmt) Nothing
                -- The wiki has a template for a run of states, which names
                -- them all in the one go, so the ids go to it as they are
                -- rather than being localized one at a time. It wants a run
                -- though: given a single state it looks for a second and says
                -- so where the name belongs, and one state is the wiki's other
                -- template.
                msgToPP $ MsgPrioritize $ Doc.doc2text $ case states of
                    [one] -> template "state" [T.pack (show one)]
                    _ -> template "states" (map (T.pack . show) states)
prioritize stmt = preStatement stmt

-- | Generic handler for a compound statement that picks out one thing to work
-- with and narrows the choice with a @limit@ block, e.g. @random_owned_state@.
--
-- The conditions in the @limit@ are clauses about the thing being picked, so
-- where there are few enough of them to read on one line they are joined onto
-- the header as a relative clause, the way the wiki writes it, instead of
-- standing under a heading of their own that puts them two levels further in
-- than the effects they qualify. Only headers that name one thing at a time can
-- take the clause, which in HOI4 is every scope except the @all_@ triggers.
compoundMessageScope :: (HOI4Info g, Monad m) =>
    ScriptMessage -- ^ Message to use as the block header
    -> StatementHandler g m
compoundMessageScope header stmt@[pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        let (mlimit, unlimited) = extractStmt (matchLhsText "limit") scr
            (mprio, rest) = extractStmt (matchLhsText "prioritize") unlimited
        mclause <- maybe (return Nothing) limitClause mlimit
        case mclause of
            Nothing -> do
                script_pp'd <- ppMany scr
                return (headed i header script_pp'd)
            Just clause -> do
                headtext <- messageText header
                priomsgs <- maybe (return []) prioritize mprio
                priotexts <- map T.strip <$> traverse (messageText . snd) priomsgs
                script_pp'd <- ppMany rest
                let aside = if null priotexts then ""
                        else " (" <> T.intercalate ", " priotexts <> ")"
                    heading = T.dropWhileEnd (== ':') (T.stripEnd headtext)
                        <> aside <> " that " <> clause <> ":"
                return (headed i (MsgUnprocessed heading) script_pp'd)
compoundMessageScope _ stmt = preStatement stmt

-- | Generic handler for a compound statement whose @limit@ block is the
-- condition the rest of it runs under rather than a narrowing of something it
-- picks out: @if@ and @else_if@. The conditions read on from the header word
-- itself, so that "If:" with "Limited to:" under it and the condition under that
-- comes out as one line saying what the branch is for.
compoundMessageCondition :: (HOI4Info g, Monad m) =>
    ScriptMessage -- ^ Message to use as the block header
    -> StatementHandler g m
compoundMessageCondition header stmt@[pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        let (mlimit, rest) = extractStmt (matchLhsText "limit") scr
        mclause <- maybe (return Nothing) limitClause mlimit
        case mclause of
            Nothing -> do
                script_pp'd <- ppMany scr
                return (headed i header script_pp'd)
            Just clause -> do
                headtext <- messageText header
                script_pp'd <- ppMany rest
                let heading = T.dropWhileEnd (== ':') (T.stripEnd headtext)
                        <> " " <> clause <> ":"
                return (headed i (MsgUnprocessed heading) script_pp'd)
compoundMessageCondition _ stmt = preStatement stmt

-- | The most conditions that will be joined onto a header line rather than
-- listed under it. Past a handful the line is harder to read than the list.
limitClauseMax :: Int
limitClauseMax = 3

-- | The conditions of a @limit@ block written as one clause, if they will read
-- as one: a handful of conditions, none of them a block with conditions of its
-- own, and every one of them written as something the scope does or is.
limitClause :: (HOI4Info g, Monad m) => GenericStatement -> PPT g m (Maybe Text)
limitClause [pdx| %_ = @scr |] = do
    msgs <- setIsInEffect False (ppMany scr)
    texts <- map T.strip <$> traverse (messageText . snd) msgs
    let flat = case map fst msgs of
            [] -> False
            (i:is) -> all (i ==) is
    return $ if not flat || length texts > limitClauseMax || any (not . isClause) texts
        then Nothing
        else Just (joinClauses (dropRepeatedSubject (map lowerFirst texts)))
limitClause _ = return Nothing

-- | Whether a condition is written as a statement about the thing it applies to,
-- and so can be read on from a header. A condition we failed to make sense of
-- came out as a @<pre>@ and has to keep the line where it can be seen, and one
-- written as a label and a value -- @Scripted Trigger: …@ -- is not a statement
-- about anything.
isClause :: Text -> Bool
isClause t = not (T.null t) && not ("<pre>" `T.isInfixOf` t) && not labelled
    where
        labelled = case T.breakOn ": " t of
            (before, rest) -> not (T.null rest) && T.length before <= 30
                                && T.all (\c -> isAlpha c || c == ' ') before

-- | Whether a line reads as something the scope's subject does or is, rather than
-- as an order given to the reader. Effects are worded as orders, since they stand
-- under a heading that has already named who they are about -- "Set nationality to
-- our country" -- and putting a subject in front of one of those gives a sentence
-- that does not agree with itself.
--
-- English tells the two apart by the @-s@ of the third person, with the modal verbs
-- as the standing exception. The few orders that end in @-s@ of their own have to
-- be named. Anything unrecognised is taken for an order, which costs no more than
-- the heading it would have had anyway.
isThirdPerson :: Text -> Bool
isThirdPerson line = case T.toLower (T.takeWhile isAlpha line) of
    "" -> False
    word -> word `notElem` imperativesInS
            && ("s" `T.isSuffixOf` word || word `elem` modals)
    where
        modals = ["will", "would", "can", "could", "may", "might", "shall", "must"]
        imperativesInS = ["dismiss", "address", "press", "pass"]

-- | A condition said the other way about, if English will take a "not" into it
-- without the verb having to be rebuilt. That holds when the clause opens with a
-- verb that carries one -- "Is at war" becomes "Is ''not'' at war" -- and not
-- when it opens with a plain verb, which would have to be put back into the
-- infinitive behind a "does not".
--
-- "Has" is either of those depending on what comes after it, so it is looked at
-- more closely: standing in front of a participle it is an auxiliary and takes
-- the "not", and anywhere else it is the verb itself and is rebuilt.
--
-- A condition that already says "not" is left alone rather than given a second
-- one to read past.
negateClause :: Text -> Maybe Text
negateClause t
    | T.null rest = Nothing
    | "not" `elem` T.words (T.map (\c -> if isAlpha c then toLower c else ' ') t) = Nothing
    | lower `elem` ["has", "have", "had"] =
        Just $ if isParticiple rest
            then word <> " ''not''" <> rest
            else doForm <> " ''not'' have" <> rest
    | lower `elem` negatable = Just (word <> " ''not''" <> rest)
    | Just plain <- lookup lower thirdPerson =
        Just $ capsLike "Does" <> " ''not'' " <> plain <> rest
    | otherwise = Nothing
    where
        word = T.takeWhile isAlpha t
        rest = T.dropWhile isAlpha t
        lower = T.toLower word
        capsLike = if maybe False (isUpper . fst) (T.uncons word) then id else T.toLower
        doForm = capsLike (case lower of
            "have" -> "Do"
            "had" -> "Did"
            _ -> "Does")
        negatable =
            [ "is", "are", "was", "were"
            , "does", "do", "did", "can", "could", "will", "would"
            , "may", "might", "must", "shall" ]
        -- A condition written with the verb itself takes a "does not" and gives
        -- the verb back its plain form. Only the verbs a condition of ours is
        -- known to open with are listed: an ordinary word ending in an S would
        -- otherwise be read as one of these and negated as though it were.
        thirdPerson = [("gives", "give")]

-- | Whether a clause goes on with a past participle, an adverb in front of one
-- not counting. Regular participles are told by their @-ed@; the irregular ones
-- that turn up in a condition of ours are listed.
isParticiple :: Text -> Bool
isParticiple line = case T.toLower firstWord of
    "" -> False
    word
        | word `elem` adverbs || "ly" `T.isSuffixOf` word -> isParticiple after
        | otherwise -> (T.length word > 3 && "ed" `T.isSuffixOf` word)
                       || word `elem` irregulars
    where
        firstWord = T.takeWhile isAlpha (T.dropWhile (not . isAlpha) line)
        after = T.dropWhile isAlpha (T.dropWhile (not . isAlpha) line)
        adverbs = ["already", "ever", "still", "just", "yet", "also", "again", "now"]
        irregulars =
            [ "been", "become", "begun", "broken", "built", "cast", "chosen"
            , "come", "cut", "dealt", "done", "drawn", "driven", "fallen"
            , "felt", "fought", "found", "given", "gone", "grown", "held"
            , "hidden", "hit", "kept", "known", "laid", "left", "lent", "let"
            , "lost", "made", "meant", "met", "paid", "put", "read", "risen"
            , "run", "said", "seen", "sent", "set", "shown", "shut", "sold"
            , "spent", "split", "spoken", "spread", "stood", "sworn", "taken"
            , "taught", "thrown", "told", "torn", "understood", "won", "worn"
            , "written" ]

-- | Handler for @NOT@, which is otherwise written as a heading with the
-- conditions it rules out standing under it. A block holding a single condition
-- reads better said in one line, and a single line is also the form a header can
-- fold in, so it is written that way whenever English will negate the condition
-- inside without rebuilding it.
compoundMessageNot :: (HOI4Info g, Monad m) => StatementHandler g m
compoundMessageNot stmt@[pdx| %_ = @scr |] = case scr of
    [inner] -> do
        msgs <- ppOne inner
        case msgs of
            [(_, msg)] -> do
                text <- T.strip <$> messageText msg
                case negateClause text of
                    Just negated | isClause text -> msgToPP (MsgUnprocessed negated)
                    _ -> compoundMessage MsgNot stmt
            _ -> compoundMessage MsgNot stmt
    _ -> compoundMessage MsgNot stmt
compoundMessageNot stmt = compoundMessage MsgNot stmt

-- | Drop the subject of a clause where an earlier clause in the same list has
-- already named it. A character block folds to a line that opens with the
-- person's name in bold, and two of those joined together say the name twice --
-- "X is active in this country and X is not a Field Marshal" -- where English
-- would say it once and carry it over.
dropRepeatedSubject :: [Text] -> [Text]
dropRepeatedSubject = go []
    where
        go _ [] = []
        go seen (t:ts) = case subject t of
            Just (name, rest) | name `elem` seen -> rest : go seen ts
            Just (name, _) -> t : go (name : seen) ts
            Nothing -> t : go seen ts
        -- A subject is a name in bold at the head of the clause; the bold marks
        -- are what say where it ends.
        subject t = do
            afterOpen <- T.stripPrefix bold t
            let (name, rest) = T.breakOn bold afterOpen
            after <- T.stripPrefix bold rest
            trimmed <- T.stripPrefix " " after
            if T.null name then Nothing else Just (name, trimmed)
        bold = "'''"

-- | Join clauses into a list the way it would be written out: a serial comma
-- between all but the last two, which take an "and".
joinClauses :: [Text] -> Text
joinClauses ts = case unsnoc ts of
    Just (front@(_:_), end) -> T.intercalate ", " front <> " and " <> end
    _ -> fold ts

-- | Lower the first letter of a clause so that it reads on from whatever it is
-- being joined to. A first word in capitals is a tag or an abbreviation and is
-- left as it is.
--
-- Some clauses open with the name of a thing rather than with a verb: a mechanic
-- the game has named, like "Army Readiness", or a sentence the game itself wrote.
-- Lowering one of those spells the name wrong. A name is told from a verb by what
-- comes after it, since the second word of a name is capitalized as well; the
-- handful of words that open a clause of ours often, and are never the first word
-- of a name, are lowered whatever follows them.
lowerFirst :: Text -> Text
lowerFirst t
    | T.length firstWord > 1, T.all isUpper firstWord = t
    | opensAName = t
    | otherwise = case T.uncons t of
        Just (c, rest) -> T.cons (toLower c) rest
        Nothing -> t
    where
        firstWord = T.takeWhile isAlpha t
        secondWord = T.takeWhile isAlpha (T.dropWhile (not . isAlpha) (T.dropWhile isAlpha t))
        opensAName = not (T.null secondWord)
            && isUpper (T.head secondWord)
            && not isAbbreviation
            && T.toLower firstWord `notElem` alwaysLower
        -- A word in capitals throughout is an abbreviation -- "the DLC ...",
        -- "the USSR ..." -- and says nothing about the word in front of it.
        isAbbreviation = T.length secondWord > 1 && T.all isUpper secondWord
        alwaysLower =
            [ "is", "are", "was", "were", "has", "have", "had"
            , "does", "do", "did", "can", "could", "will", "would"
            , "may", "might", "must", "shall", "after", "before", "a", "an" ]

-- | Generic handler for a simple compound statement with extra info.
compoundMessageExtractTag :: (HOI4Info g, Monad m) =>
    Text
    -> (Text -> ScriptMessage) -- ^ Message to use as the block header
    -> StatementHandler g m
compoundMessageExtractTag xtract header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        let (xtracted, rest) = extractStmt (matchLhsText xtract) scr
        xtractflag <- case xtracted of
                Just [pdx| %_ = $vartag:$var |] -> eflag (Just HOI4Country) (Right (vartag, var))
                Just [pdx| %_ = $flag |] -> eflag (Just HOI4Country) (Left flag)
                _ -> return Nothing
        let flagd = case xtractflag of
                Just flag -> flag
                _ -> "<!-- Check Script -->"
        script_pp'd <- ppMany rest
        return ((i, header flagd) : script_pp'd)
compoundMessageExtractTag _ _ stmt = preStatement stmt

compoundMessageExtract :: (HOI4Info g, Monad m) =>
    Text
    -> (Text -> ScriptMessage) -- ^ Message to use as the block header
    -> StatementHandler g m
compoundMessageExtract xtract header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        let (mxtracted, rest) = extractStmt (matchLhsText xtract) scr
            xtracted = case mxtracted of
                Just [pdx| %_ = ?txt |] -> txt
                _-> "<!-- Check game Script -->"
        script_pp'd <- ppMany rest
        return ((i, header xtracted) : script_pp'd)
compoundMessageExtract _ _ stmt = preStatement stmt

compoundMessageExtractNum :: (HOI4Info g, Monad m) =>
    Text
    -> (Double -> ScriptMessage) -- ^ Message to use as the block header
    -> StatementHandler g m
compoundMessageExtractNum xtract header stmt@[pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        let (mxtracted, rest) = extractStmt (matchLhsText xtract) scr
        case mxtracted of
            Just [pdx| %_ = !num |] -> do
                script_pp'd <- ppMany rest
                return ((i, header num) : script_pp'd)
            _-> preStatement stmt
compoundMessageExtractNum _ _ stmt = preStatement stmt

-- | Generic handler for a simple compound statement headed by a pronoun.
compoundMessagePronoun :: (HOI4Info g, Monad m) => StatementHandler g m
compoundMessagePronoun stmt@[pdx| $head = @scr |] = withCurrentIndent $ \i -> do
    -- Where what the pronoun stands for is known -- the target of a targeted
    -- decision, whoever fired the event -- the block is headed by that, and
    -- the scope inside carries it as the new current scope.
    mnamed <- case T.toLower head of
        "from" -> getFromIdent
        "prev" -> getPrevIdent
        _ -> return Nothing
    case mnamed of
      Just val -> do
        valtext <- scopeValText val
        script_pp'd <- scope (scopeValType val) $ withThisIdent (Just val) $ ppMany scr
        return $ headed i (MsgUnprocessed (valtext <> ":")) script_pp'd
      Nothing -> do
        params <- withCurrentFile $ \f -> case T.toLower head of
            "root" -> --do --ROOT
                    --newscope <- getRootScope
                    return (Just HOI4Country, Just MsgROOTSCOPECountry) --case newscope of
                        --Just HOI4Country -> Just MsgROOTSCOPECountry
                        --Just HOI4ScopeCharacter -> Just MsgROOTSCOPECharacter
                        --Just HOI4Operative -> Just MsgROOTSCOPEOperative
                        --Just HOI4ScopeState -> Just MsgROOTSCOPEState
                        --Just HOI4UnitLeader -> Just MsgROOTSCOPEUnitLeader
                        --_ -> Nothing) -- warning printed below
            "prev" -> do --PREV
                    newscope <- getPrevScope
                    return (newscope, case newscope of
                        Just HOI4Country -> Just MsgPREVSCOPECountry
                        Just HOI4ScopeCharacter -> Just MsgPREVSCOPECharacter
                        Just HOI4Operative -> Just MsgPREVSCOPEOperative
                        Just HOI4ScopeState -> Just MsgPREVSCOPEState
                        Just HOI4UnitLeader -> Just MsgPREVSCOPEUnitLeader
                        Just HOI4Misc -> Just MsgPREVSCOPEMisc
                        Just HOI4From -> Just MsgPREVSCOPEFROM -- Roll with it
                        Just HOI4Custom -> Just MsgPREVSCOPECustom
                        Nothing -> Just MsgPREVSCOPECustom2
                        _ -> Nothing) -- warning printed below
            "prev.prev" -> do --PREV
                    newscope <- getPrevScopeCustom 2
                    return (newscope, Just MsgPREVPREV)
            "prev.prev.prev" -> do --PREV
                    newscope <- getPrevScopeCustom 3
                    return (newscope, Just MsgPREVPREVPREV)
            "owner" -> do --PREV
                    newscope <- getCurrentScope
                    return (Just HOI4Country, case newscope of
                        Just HOI4ScopeState -> Just MsgOwnerStateSCOPE
                        Just HOI4ScopeCharacter -> Just MsgOwnerUnitSCOPE
                        Just HOI4UnitLeader -> Just MsgOwnerUnitSCOPE
                        Just HOI4Operative -> Just MsgOwnerUnitSCOPE
                        _ -> Just MsgOwnerSCOPE)
            "from" -> return (Just HOI4From, Just MsgFROMSCOPE) -- FROM / Should be some way to have different message depending on if it is event or decison, etc.
            "from.from" -> return (Just HOI4From, Just MsgFROMFROMSCOPE)
            "from.from.from" -> return (Just HOI4From, Just MsgFROMFROMFROMSCOPE)
            _ -> return (Nothing, Nothing)
        -- What ROOT stands for is what THIS stands for just inside a ROOT
        -- block. The other pronouns known by name were already peeled off.
        mrootval <- case T.toLower head of
            "root" -> getRootIdent
            _ -> return Nothing
        case params of
            (Just newscope, Just scopemsg) -> do
                script_pp'd <- scope newscope $ withThisIdent mrootval $ ppMany scr
                return $ headed i scopemsg script_pp'd
            (Nothing, Just scopemsg) -> do
                script_pp'd <- scope HOI4Custom $ ppMany scr
                return $ headed i scopemsg script_pp'd
            _ -> do
                withCurrentFile $ \f -> do
                    traceM $ "compoundMessagePronoun: " ++ f ++ ": potentially invalid use of " ++ T.unpack head ++ " in " ++ show stmt
                preStatement stmt
compoundMessagePronoun stmt = preStatement stmt

-- | Generic handler for a simple compound statement with a tagged header.
compoundMessageTagged :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -- ^ Message to use as the block header
    -> Maybe HOI4Scope -- ^ Scope to push on the stack, if any
    -> StatementHandler g m
compoundMessageTagged header mscope stmt@[pdx| $_:$tag = %_ |]
    = maybe id scope mscope $ compoundMessage (header tag) stmt
compoundMessageTagged _ _ stmt = preStatement stmt

-- | Verify assumption about rhs
rhsAlways :: (HOI4Info g, Monad m) => Text -> ScriptMessage -> StatementHandler g m
rhsAlways assumedRhs msg [pdx| %_ = ?rhs |] | T.toLower rhs == assumedRhs = msgToPP msg
rhsAlways _ _ stmt = warn (BadValue "rhsAlways" stmt) $ preStatement stmt

rhsAlwaysYes :: (HOI4Info g, Monad m) => ScriptMessage -> StatementHandler g m
rhsAlwaysYes = rhsAlways "yes"

-- | As 'rhsAlwaysYes', for effects that the game's script sometimes writes with
-- a scope on the right instead of @yes@. The effect works on the scope it is
-- written in whatever stands there, so the name says nothing the message needs
-- -- but @no@ still turns it off.
rhsYesOrScope :: (HOI4Info g, Monad m) => ScriptMessage -> StatementHandler g m
rhsYesOrScope msg stmt@[pdx| %_ = ?rhs |]
    | T.toLower rhs /= "no" = msgToPP msg
    | otherwise = return []
rhsYesOrScope _ stmt = preStatement stmt

rhsIgnored :: (IsGameState (GameState g), Monad m) =>
    ScriptMessage -> p -> PPT g m IndentedMessages
rhsIgnored msg stmt = msgToPP msg

-- | The number a statement gives, whether it writes the number out or names a
-- script constant holding it. Comes back as 'Nothing' if it does neither, most
-- likely because it uses a variable instead.
constantOrNumber :: (HOI4Info g, Monad m) => GenericStatement -> PPT g m (Maybe Double)
constantOrNumber [pdx| %_ = !num |] = return (Just num)
constantOrNumber [pdx| %_ = ?name |] = constantValue name
constantOrNumber _ = return Nothing

noloc :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
noloc txt = return Nothing

-- | The name written bare inside an array, as script writes a list of resources
-- or of traits.
bareAtom :: GenericStatement -> Maybe Text
bareAtom (StatementBare (GenericLhs atom [])) = Just atom
bareAtom stmt = warn (UnknownSection "bare atom array" stmt) Nothing

-- | The number written bare inside an array, as script writes a list of states
-- or of strategic regions.
bareInt :: GenericStatement -> Maybe Int
bareInt (StatementBare (IntLhs n)) = Just n
bareInt stmt = warn (UnknownSection "bare number array" stmt) Nothing

-- | Read text that belongs to something of the current scope's -- an idea it
-- gains, a decision of its own that is named -- with the pronouns meaning
-- what they do as the game draws that text for that scope: it is its own
-- ROOT there, and the FROM of the script in hand has no say.
thisContext :: (HOI4Info g, Monad m) => PPT g m a -> PPT g m a
thisContext action = do
    holder <- getThisIdent
    withRootIdent holder $ withFromIdent Nothing action

-----------------------------------------------------
-- handlers for the effects on an ongoing border war --
-----------------------------------------------------

-- | The state a field names, whether by its id or through a variable holding
-- it. Border wars and army teleports both name states this way.
statedFrom :: (HOI4Info g, Monad m) => Maybe GenericStatement -> PPT g m Text
statedFrom (Just [pdx| %_ = !num |]) = getStateLoc num
statedFrom (Just [pdx| %_ = $vartag:$var |]) = eGetStateText (Right (vartag, var))
statedFrom (Just [pdx| %_ = $var |]) = eGetStateText (Left var)
statedFrom _ = return "<!-- Check Script -->"

getbaretraits :: GenericStatement -> Maybe Text
getbaretraits (StatementBare (GenericLhs trait [])) = Just trait
getbaretraits stmt = Nothing

-- | A wiki link to a heading on a page. The heading a link jumps to is written
-- with underscores where the name has spaces; a percent escape is not followed
-- here.
sectionLink :: Text -> Text -> Text -> Text
sectionLink page heading name = mconcat
    [ "[[", page, "#", T.replace " " "_" heading, "|", name, "]]" ]

-- | Emit a tooltip's text, or nothing at all if it has none: scripts use
-- tooltips whose text is only whitespace (@generic_skip_one_line_tt@ and
-- friends) to space the tooltip out in game, and on the wiki they are noise. A
-- tooltip written over several lines keeps its breaks, written the way the wiki
-- writes a break inside a list item.
tooltipText :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> Text -> PPT g m IndentedMessages
tooltipText msg loc
    | T.null stripped = return []
    | otherwise = msgToPP (msg (Doc.nl2br stripped))
    where stripped = T.strip (dropEffectiveChange loc)

-- | Drop an @Effective change:@ that a tooltip ends on. The game writes that
-- heading over the lines it draws next -- what the effects after the tooltip
-- come to, worked out from the state of the game as it draws them. Nothing
-- outside the game can work those out the same way, and the effects are
-- written out under the tooltip in their own right here, so the heading would
-- stand over nothing. One in the middle of a tooltip heads lines the tooltip
-- itself goes on to say, and stays.
dropEffectiveChange :: Text -> Text
dropEffectiveChange txt = maybe txt tidy heading
    where
        body = T.stripEnd txt
        -- The heading is often picked out in colour, which becomes bold here.
        closing = T.takeWhileEnd (== '\'') body
        unclosed = T.dropEnd (T.length closing) body
        heading = listToMaybe
            [ T.dropEnd (T.length h) unclosed
            | h <- ["effective changes:", "effective change:"]
            , h `T.isSuffixOf` T.toLower unclosed ]
        -- What opened the bold around the heading has nothing left to wrap, and
        -- the punctuation that led into the heading nothing left to lead into.
        tidy before =
            let opened = if T.null closing then before else T.dropWhileEnd (== '\'') before
            in T.dropWhileEnd (\c -> isSpace c || c == ',' || c == '.') opened
