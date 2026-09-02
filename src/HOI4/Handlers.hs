{-# LANGUAGE LambdaCase #-}
module HOI4.Handlers (
        preStatement
    ,   preStatementText'
    ,   plainStatement
    ,   plainMsg
    ,   plainMsg'
    ,   msgToPP
    ,   msgToPP'
    ,   ppMtth
    ,   compound
    ,   compoundMessage
    ,   compoundMessageNot
    ,   setDivisionTemplateLock
    ,   clearDivisionTemplateCap
    ,   hasResourcesAmount
    ,   anyProvinceBuildingLevel
    ,   compareAutonomyState
    ,   arrayValue
    ,   createShip
    ,   transferShip
    ,   addEquipmentSubsidy
    ,   addEquipmentProduction
    ,   createProductionLicense
    ,   createFactionFromTemplate
    ,   addUnitsToDivisionTemplate
    ,   setDivisionTemplateCap
    ,   setTruce
    ,   whitePeace
    ,   puppetCountry
    ,   setPowerBalance
    ,   getHighestScoredCountry
    ,   addContestedOwner
    ,   addResistanceTarget
    ,   transferUnitsFraction
    ,   compoundMessageScope
    ,   compoundMessageCondition
    ,   compoundMessageExtractTag
    ,   compoundMessageExtract
    ,   compoundMessageExtractNum
    ,   compoundMessagePronoun
    ,   compoundMessageTagged
    ,   withLocAtom
    ,   withLocAtom'
    ,   withLocAtomName
    ,   withLookupAtom
    ,   withLookupAtomKey
    ,   withLocAtomCompound
    ,   withLocAtomKey
    ,   withMaybelocAtom2
    ,   withLocAtomIcon
    ,   withState
    ,   withNonlocAtom
    ,   withNonlocTextValue
    ,   numeric
    ,   numericCompare
    ,   numericCompareCompound
    ,   numericCompareCompoundLoc
    ,   withFlag
    ,   withBool
    ,   withBoolHOI4Scope
    ,   withFlagOrBool
    ,   numericIcon
    ,   numericIconLoc
    ,   numericLoc
    ,   ideologyIconLoc
    ,   partyIconLoc
    ,   partyIconOf
    ,   beliefIcon
    ,   withPartyIcon
    ,   textValue
    ,   textValueKey
    ,   textValueCompare
    ,   valueValue
    ,   textAtom
    ,   textAtomKey
    ,   taTypeFlag
    ,   simpleEffectNum
    ,   simpleEffectAtom
    ,   ppAiWillDo
    ,   ppAiMod
    ,   amountTakenIdeas
    ,   addScientistXp
    ,   gainXp
    ,   hasResourcesInCountry
    ,   hasResourcesInCollection
    ,   hasResourcesRights
    ,   damageUnits
    ,   numDivisionsInStates
    ,   setVariableToRandom
    ,   addRandomTrait
    ,   forceEnableResistance
    ,   thisScope
    ,   arrayLoop
    ,   collectionSize
    ,   createRailwayGun
    ,   addHistoryEntry
    ,   setCountryLeaderName
    ,   generateScientistCharacter
    ,   setStateProvinceController
    ,   numPlanesStationedInRegions
    ,   listedScope
    ,   listedState
    ,   addMines
    ,   setDivisionForceAllowRecruiting
    ,   numericOrVar
    ,   opinion
    ,   hasOpinion
    ,   triggerEvent
    ,   random
    ,   randomList
    ,   forLoopEffect
    ,   hasDlc
    ,   effectTooltip
    ,   limitClause
    ,   isClause
    ,   isThirdPerson
    ,   lowerFirst
    ,   handleFocus
    ,   focusUncomplete
    ,   focusProgress
    ,   setVariable
    ,   clampVariable
    ,   checkVariable
    ,   rhsAlways
    ,   rhsAlwaysYes
    ,   rhsYesOrScope
    ,   rhsIgnored
    ,   exportVariable
    ,   addBuildingConstruction
    ,   buildingLevel
    ,   constructBuildingInRandomProvince
    ,   setBuildingLevel
    ,   addNamedThreat
    ,   createWargoal
    ,   removeWargoal
    ,   declareWarOn
    ,   annexCountry
    ,   addTechBonus
    ,   addBreakthrough
    ,   setFlag
    ,   hasFlag
    ,   addToWar
    ,   setAutonomy
    ,   setPolitics
    ,   hasCountryLeader
    ,   setPartyName
    ,   loadFocusTree
    ,   setNationality
    ,   prioritize
    ,   hasWarGoalAgainst
    ,   diplomaticRelation
    ,   hasArmySize
    ,   startCivilWar
    ,   createEquipmentVariant
    ,   setRule
    ,   addAiStrategy
    ,   addDoctrineCostReduction
    ,   doctrinePage
    ,   freeBuildingSlots
    ,   addAutonomyRatio
    ,   sendEquipment
    ,   buildRailway
    ,   canBuildRailway
    ,   hasRailwayConnection
    ,   stateResource
    ,   addIntel
    ,   addDecryption
    ,   countryLockAllDivisionTemplate
    ,   changeDivisionTemplate
    ,   hasLicense
    ,   addResource
    ,   modifyBuildingResources
    ,   handleDate
    ,   setTechnology
    ,   setCapital
    ,   setPopularities
    ,   addEquipment
    ,   giveResourceRights
    ,   addAce
    ,   divisionTemplate
    ,   hasNavySize
    ,   hasDeployedAirForceSize
    ,   locandid
    ,   thisContext
    ,   createUnit
    ,   damageBuilding
    ,   withRegion
    ,   divisionsInState
    ,   deleteUnits
    ,   startBorderWar
    ,   cancelBorderWar
    ,   finalizeBorderWar
    ,   setBorderWarData
    ,   teleportArmies
    ,   transferNavy
    ,   shipsIn
    ,   anyStateIn
    ,   countTriggers
    ,   addProvinceModifier
    ,   powerBalanceRange
    ,   navalStrengthComparison
    ,   unlockDecisionTooltip
    --specialhandler exports
    ,   TextAtom(..)
    ,   TextValue(..)
    ,   parseTA
    ,   parseTV
    ) where

import Data.Char (toLower, toUpper, isAlpha, isUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.List (foldl', intersperse, intercalate, findIndex, partition)
import Data.Maybe

import Control.Applicative (liftA2, (<|>))
import Control.Arrow (first)
import Control.Monad (foldM)
import Control.Monad.State (gets)
import Data.Foldable (fold)

import Abstract -- everything
import Doc (Doc)
import qualified Doc -- everything
import HOI4.Messages -- everything
import HOI4.WikiTables (scriptIconTable, iconTerm, scriptIconFileTable, iconKey, doctrineFolderIds)
import MessageTools (plural, iquotes, italicText, boldText, typewriterText
                    , plainNum, plainNumMin, colourNumSign, plainPc, colourPc, reducedNum
                    , formatDays, formatHours)
import QQ -- everything
-- everything
import SettingsTypes ( PPT, IsGameData (..), GameData (..), IsGameState (..), GameState (..)
                     , indentUp, indentDown, withCurrentIndent, withCurrentIndentZero, alsoIndent'
                     , withCurrentFile
                     , getGameInterface, getGameInterfaceIfPresent, unsnoc
                     , LocArg (..) )
import HOI4.Templates
import HOI4.Localization
import StatementUtils -- everything
import {-# SOURCE #-} HOI4.Common (ppScript, ppMany, ppOne)
import HOI4.Types -- everything

import Debug.Trace



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







-----------------------------------------------------------------
-- Script handlers that should be used directly, not via ppOne --
-----------------------------------------------------------------

-- | Data for @mean_time_to_happen@ clauses
data MTTH = MTTH
        {   mtth_years :: Maybe Int
        ,   mtth_months :: Maybe Int
        ,   mtth_days :: Maybe Int
        ,   mtth_modifiers :: [MTTHModifier]
        } deriving Show
-- | Data for @modifier@ clauses within @mean_time_to_happen@ clauses
data MTTHModifier = MTTHModifier
        {   mtthmod_factor :: Maybe Double
        ,   mtthmod_conditions :: GenericScript
        } deriving Show
-- | Empty MTTH
newMTTH :: MTTH
newMTTH = MTTH Nothing Nothing Nothing []
-- | Empty MTTH modifier
newMTTHMod :: MTTHModifier
newMTTHMod = MTTHModifier Nothing []

-- | Format a @mean_time_to_happen@ clause as wiki text.
ppMtth :: (HOI4Info g, Monad m) => Bool -> GenericScript -> PPT g m Doc
ppMtth isTriggeredOnly = ppMtth' . foldl' addField newMTTH
    where
        addField mtth [pdx| years    = !n   |] = mtth { mtth_years = Just n }
        addField mtth [pdx| months   = !n   |] = mtth { mtth_months = Just n }
        addField mtth [pdx| days     = !n   |] = mtth { mtth_days = Just n }
        addField mtth [pdx| modifier = @rhs |] = addMTTHMod mtth rhs
        addField mtth _ = mtth -- unrecognized
        addMTTHMod mtth scr = mtth {
                mtth_modifiers = mtth_modifiers mtth
                                 ++ [foldl' addMTTHModField newMTTHMod scr] } where
            addMTTHModField mtthmod [pdx| factor = !n |]
                = mtthmod { mtthmod_factor = Just n }
            addMTTHModField mtthmod stmt -- anything else is a condition
                = mtthmod { mtthmod_conditions = mtthmod_conditions mtthmod ++ [stmt] }
        ppMtth' (MTTH myears mmonths mdays modifiers) = do
            modifiers_pp'd <- intersperse PP.line <$> mapM pp_mtthmod modifiers
            let hasYears = isJust myears
                hasMonths = isJust mmonths
                hasDays = isJust mdays
                hasModifiers = not (null modifiers)
            return . mconcat $ (if isTriggeredOnly then [] else
                maybe []
                    (\years ->
                        [PP.int years, PP.space, Doc.strictText $ plural years "year" "years"]
                        ++
                        if hasMonths && hasDays then [",", PP.space]
                        else if hasMonths || hasDays then ["and", PP.space]
                        else [])
                    myears
                ++
                maybe []
                    (\months -> [PP.int months, PP.space, Doc.strictText $ plural months "month" "months"])
                    mmonths
                ++
                maybe []
                    (\days ->
                        (if hasYears && hasMonths then ["and", PP.space]
                         else []) -- if years but no months, already added "and"
                        ++
                        [PP.int days, PP.space, Doc.strictText $ plural days "day" "days"])
                    mdays
                ) ++
                (if hasModifiers then
                    (if isTriggeredOnly then
                        [PP.line, "'''Weight modifiers'''", PP.line]
                    else
                        [PP.line, "<br/>'''Modifiers'''", PP.line])
                    ++ modifiers_pp'd
                 else [])
        pp_mtthmod (MTTHModifier (Just factor) conditions) =
            case conditions of
                [_] -> do
                    conditions_pp'd <- ppScript conditions
                    return . mconcat $
                        [conditions_pp'd
                        ,PP.enclose ": '''×" "'''" (Doc.ppFloat factor)
                        ]
                _ -> do
                    conditions_pp'd <- indentUp (ppScript conditions)
                    return . mconcat $
                        ["*"
                        ,PP.enclose "'''×" "''':" (Doc.ppFloat factor)
                        ,PP.line
                        ,conditions_pp'd
                        ]
        pp_mtthmod (MTTHModifier Nothing _)
            = return "(invalid modifier! Bug in extractor?)"

--------------------------------
-- General statement handlers --
--------------------------------

-- | Generic handler for a simple compound statement. Usually you should use
-- 'compoundMessage' instead so the text can be localized.
compound :: (HOI4Info g, Monad m) =>
    Text -- ^ Text to use as the block header, without the trailing colon
    -> StatementHandler g m
compound header [pdx| %_ = @scr |]
    = withCurrentIndent $ \_ -> do -- force indent level at least 1
        headerMsg <- plainMsg (header <> ":")
        scriptMsgs <- ppMany scr
        return $ headerMsg ++ scriptMsgs
compound _ stmt = preStatement stmt

-- | Generic handler for a simple compound statement.
compoundMessage :: (HOI4Info g, Monad m) =>
    ScriptMessage -- ^ Message to use as the block header
    -> StatementHandler g m
compoundMessage header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        script_pp'd <- ppMany scr
        return ((i, header) : script_pp'd)
compoundMessage _ stmt = preStatement stmt

-- | Handler for @prioritize@, which names the states a scope should pick from
-- first. Defined here rather than with the other handlers below because
-- 'compoundMessageScope' folds it into the header line it belongs to, and a
-- Template Haskell splice further down would otherwise put it out of scope.
prioritize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
prioritize stmt@[pdx| %_ = @arr |] = do
                let states = mapMaybe stateFromArray arr
                    stateFromArray (StatementBare (IntLhs e)) = Just e
                    stateFromArray stmt = trace ("Unknown in prioritize array statement: " ++ show stmt) Nothing
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
                return ((i, header) : script_pp'd)
            Just clause -> do
                headtext <- messageText header
                priomsgs <- maybe (return []) prioritize mprio
                priotexts <- map T.strip <$> traverse (messageText . snd) priomsgs
                script_pp'd <- ppMany rest
                let aside = if null priotexts then ""
                        else " (" <> T.intercalate ", " priotexts <> ")"
                    heading = T.dropWhileEnd (== ':') (T.stripEnd headtext)
                        <> aside <> " that " <> clause <> ":"
                return ((i, MsgUnprocessed heading) : script_pp'd)
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
                return ((i, header) : script_pp'd)
            Just clause -> do
                headtext <- messageText header
                script_pp'd <- ppMany rest
                let heading = T.dropWhileEnd (== ':') (T.stripEnd headtext)
                        <> " " <> clause <> ":"
                return ((i, MsgUnprocessed heading) : script_pp'd)
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
        return $ (i, MsgUnprocessed (valtext <> ":")) : script_pp'd
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
            _ -> trace (f ++ ": compoundMessagePronoun: don't know how to handle head " ++ T.unpack head)
                 $ return (Nothing, undefined)
        -- What ROOT stands for is what THIS stands for just inside a ROOT
        -- block. The other pronouns known by name were already peeled off.
        mrootval <- case T.toLower head of
            "root" -> getRootIdent
            _ -> return Nothing
        case params of
            (Just newscope, Just scopemsg) -> do
                script_pp'd <- scope newscope $ withThisIdent mrootval $ ppMany scr
                return $ (i, scopemsg) : script_pp'd
            (Nothing, Just scopemsg) -> do
                script_pp'd <- scope HOI4Custom $ ppMany scr
                return $ (i, scopemsg) : script_pp'd
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

-- | Generic handler for a statement whose RHS is a localizable atom.
-- with the ability to transform the localization key
withLocAtom' :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> (Text -> Text) -> StatementHandler g m
withLocAtom' msg xform [pdx| %_ = ?key |]
    = msgToPP . msg =<< getGameL10n (xform key)
withLocAtom' _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom.
withLocAtom :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage)
    -> GenericStatement -> PPT g m IndentedMessages
withLocAtom msg = withLocAtom' msg id

-- | As 'withLocAtom', localizing the @_name@ key of the atom given.
withLocAtomName :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
withLocAtomName msg = withLocAtom' msg (<> "_name")

-- | Generic handler for a statement whose RHS is an atom run through the given
-- monadic lookup: a character's name, a wiki link, an organization's name.
withLookupAtom :: (HOI4Info g, Monad m) =>
    (Text -> PPT g m Text) -> (Text -> ScriptMessage) -> StatementHandler g m
withLookupAtom look msg [pdx| %_ = ?key |] = msgToPP . msg =<< look key
withLookupAtom _ _ stmt = preStatement stmt

-- | As 'withLookupAtom', passing the raw key to the message as well.
withLookupAtomKey :: (HOI4Info g, Monad m) =>
    (Text -> PPT g m Text) -> (Text -> Text -> ScriptMessage) -> StatementHandler g m
withLookupAtomKey look msg [pdx| %_ = ?key |] = do
    loc <- look key
    msgToPP $ msg loc key
withLookupAtomKey _ _ stmt = preStatement stmt

withLocAtomCompound :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomCompound msg stmt@[pdx| %_ = %rhs |] = case rhs of
    CompoundRhs [scr] -> withLocAtom msg scr
    _ -> preStatement stmt
withLocAtomCompound _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom.
-- with the ability to transform the localization key and also need the key itself
withLocAtomKey' :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> (Text -> Text) -> StatementHandler g m
withLocAtomKey' msg xform [pdx| %_ = ?key |]
    = msgToPP . msg key =<< getGameL10n (xform key)
withLocAtomKey' _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom.
-- and need to use the
withLocAtomKey :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage)
    -> GenericStatement -> PPT g m IndentedMessages
withLocAtomKey msg = withLocAtomKey' msg id

-- | Generic handler for a statement whose RHS is a localizable atom, where we
-- also need an icon.
withLocAtomAndIcon :: (HOI4Info g, Monad m) =>
    Text -- ^ icon name - see
         -- <https://www.hoi4wiki.com/Template:Icon Template:Icon> on the wiki
        -> Bool
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomAndIcon iconkey _ msg stmt@[pdx| %_ = $vartag:$var |] = do
    mtagloc <- tagged vartag var
    case mtagloc of
        Just tagloc -> msgToPP $ msg (iconText iconkey) tagloc
        Nothing -> preStatement stmt
withLocAtomAndIcon iconkey lockey msg [pdx| %_ = ?key |]
    = do what <- Doc.doc2text <$> allowPronoun Nothing (fmap Doc.strictText . getGameL10n) key
         -- The icon template reads its key either way round, and asked for the
         -- term as well it writes it with the capitals the key was given, so a
         -- localized name goes in as it is written rather than folded down.
         lociconkey <- if lockey then getGameL10n iconkey else return iconkey
         msgToPP $ msg (iconText lociconkey) what
withLocAtomAndIcon _ _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom that
-- corresponds to an icon.
withLocAtomIcon :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage)
        -> Bool
        -> StatementHandler g m
withLocAtomIcon msg lockey stmt@[pdx| %_ = ?key |]
    = withLocAtomAndIcon key lockey msg stmt
withLocAtomIcon _ _ stmt = preStatement stmt

withState :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withState msg stmt@[pdx| %lhs = $vartag:$var |] = do
    mtagloc <- eGetState (Right (vartag, var))
    case mtagloc of
        Just tagloc -> msgToPP $ msg tagloc
        Nothing -> preStatement stmt
withState msg stmt@[pdx| %lhs = $var |] = do
    mtagloc <- eGetState (Left var)
    case mtagloc of
        Just tagloc -> msgToPP $ msg tagloc
        Nothing -> preStatement stmt
withState msg [pdx| %lhs = !stateid |]
    = msgToPP . msg =<< getStateLoc stateid
withState _ stmt = preStatement stmt

-- As withLocAtom but no l10n.
withNonlocAtom :: (HOI4Info g, Monad m) => (Text -> ScriptMessage) -> StatementHandler g m
withNonlocAtom msg [pdx| %_ = ?text |] = msgToPP $ msg text
withNonlocAtom _ stmt = preStatement stmt



-- | Handler for a resource's own name used as a trigger, which compares how
-- much of the resource the state has.
stateResource :: (HOI4Info g, Monad m) => Text -> StatementHandler g m
stateResource res = numericCompare "more than" "less than"
    (MsgStateResource (iconText res)) (MsgStateResourceVar (iconText res))

-- | Handler for a building's own name used as a trigger, which compares how many
-- levels of the building the state has.
buildingLevel :: (HOI4Info g, Monad m) => Text -> StatementHandler g m
buildingLevel building = numericCompare "more than" "fewer than"
    (MsgBuildingLevel (iconText building)) (MsgBuildingLevelVar (iconText building))

-- | Handler for @construct_building_in_random_province@, whose block names the
-- building to put up and how many levels of it.
constructBuildingInRandomProvince :: (HOI4Info g, Monad m) => StatementHandler g m
constructBuildingInRandomProvince stmt@[pdx| %_ = @scr |] = case scr of
    [[pdx| $building = !level |]] ->
        msgToPP $ MsgConstructBuildingInRandomProvince (iconText building) level
    _ -> preStatement stmt
constructBuildingInRandomProvince stmt = preStatement stmt




-- TODO (if necessary): allow operators other than = and pass them to message
-- handler
-- | Handler for numeric statements.
numeric :: (IsGameState (GameState g), Monad m) =>
    (Double -> ScriptMessage)
        -> StatementHandler g m
numeric msg [pdx| %_ = !n |] = msgToPP $ msg n
numeric _ stmt = plainMsg $ preStatementText' stmt

-- | As 'numeric', for statements script also writes with a variable holding the
-- number instead of the number itself.
numericOrVar :: (HOI4Info g, Monad m) =>
    (Double -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
numericOrVar msg _ [pdx| %_ = !n |] = msgToPP $ msg n
numericOrVar _ msgvar [pdx| %_ = $vartag:$var |] = msgToPP $ msgvar (vartag <> ":" <> var)
numericOrVar _ msgvar [pdx| %_ = $var |] = msgToPP $ msgvar var
numericOrVar _ _ stmt = plainMsg $ preStatementText' stmt

-- | Handler for numeric compare statements.
numericCompare :: (HOI4Info g, Monad m) =>
    Text -> Text ->
    (Double -> Text -> ScriptMessage) ->
    (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericCompare gt lt msg msgvar stmt@[pdx| %_ = %num |] = case num of
    (floatRhs -> Just n) -> msgToPP $ msg n $ "equal to or " <> gt
    GenericRhs n [] -> msgToPP $ msgvar n $ "equal to or " <> gt
    GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n $ "equal to or " <> gt
    _ -> trace ("Compare '=' failed : " ++ show stmt) $ preStatement stmt
numericCompare gt lt msg msgvar stmt@[pdx| %_ > %num |] = case num of
    (floatRhs -> Just n) -> msgToPP $ msg n gt
    GenericRhs n [] -> msgToPP $ msgvar n gt
    GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n gt
    _ -> trace ("Compare '>' failed : " ++ show stmt) $ preStatement stmt
numericCompare gt lt msg msgvar stmt@[pdx| %_ < %num |] = case num of
    (floatRhs -> Just n) -> msgToPP $ msg n lt
    GenericRhs n [] -> msgToPP $ msgvar n lt
    GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n lt
    _ -> trace ("Compare '<' failed : " ++ show stmt) $ preStatement stmt
numericCompare _ _ _ _ stmt = preStatement stmt

numericCompareCompound :: (HOI4Info g, Monad m) =>
    Text -> Text ->
    (Double -> Text -> ScriptMessage) ->
    (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericCompareCompound gt lt msg msgvar stmt@[pdx| %_ = %rhs |] = case rhs of
    CompoundRhs [scr] -> numericCompare gt lt msg msgvar scr
    _ -> preStatement stmt
numericCompareCompound _ _ _ _ stmt = preStatement stmt

numericCompareLoc :: (HOI4Info g, Monad m) =>
    Text -> Text ->
    (Double -> Text -> Text -> ScriptMessage) ->
    (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericCompareLoc gt lt msg msgvar stmt@[pdx| $txt = %num |] = do
    loc <- getGameL10n txt
    case num of
        (floatRhs -> Just n) -> msgToPP $ msg n ("equal to or " <> gt) loc
        GenericRhs n [] -> msgToPP $ msgvar n ("equal to or " <> gt) loc
        GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n ("equal to or " <> gt) loc
        _ -> trace ("Compare '=' failed : " ++ show stmt) $ preStatement stmt
numericCompareLoc gt lt msg msgvar stmt@[pdx| $txt > %num |] = do
    loc <- getGameL10n txt
    case num of
        (floatRhs -> Just n) -> msgToPP $ msg n gt loc
        GenericRhs n [] -> msgToPP $ msgvar n gt loc
        GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n gt loc
        _ -> trace ("Compare '>' failed : " ++ show stmt) $ preStatement stmt
numericCompareLoc gt lt msg msgvar stmt@[pdx| $txt < %num |] = do
    loc <- getGameL10n txt
    case num of
        (floatRhs -> Just n) -> msgToPP $ msg n lt loc
        GenericRhs n [] -> msgToPP $ msgvar n lt loc
        GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n lt loc
        _ -> trace ("Compare '<' failed : " ++ show stmt) $ preStatement stmt
numericCompareLoc _ _ _ _ stmt = preStatement stmt

numericCompareCompoundLoc :: (HOI4Info g, Monad m) =>
    Text -> Text ->
    (Double -> Text -> Text -> ScriptMessage) ->
    (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericCompareCompoundLoc gt lt msg msgvar stmt@[pdx| %_ = %rhs |] = case rhs of
    -- @show_current@ only asks the game to put the amount held into the
    -- tooltip, and says nothing about what is being compared. What is left is
    -- one comparison per line, each said in turn.
    CompoundRhs scr -> case snd (extractStmt (matchLhsText "show_current") scr) of
        [] -> preStatement stmt
        comparisons -> concat <$> traverse (numericCompareLoc gt lt msg msgvar) comparisons
    _ -> preStatement stmt
numericCompareCompoundLoc _ _ _ _ stmt = preStatement stmt


-- | Handler for @add_ai_strategy@, which leans the AI towards something or away
-- from it. Nothing of the world changes by it and the game draws none of it, so
-- there is no wording of the game's own to keep to: what script writes is the
-- kind of leaning, what it is about, and how strongly, and all three are said.
--
-- The thing it is about is a country for all but a couple of kinds, which name a
-- building or a kind of equipment instead. A kind that names nothing at all --
-- how heavily to garrison, what to hold factories back for -- is about the
-- country's own doings, and is said on its own.
addAiStrategy :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addAiStrategy stmt@[pdx| %_ = @scr |] = case aiKind of
    Just kind -> do
        about <- aiAbout kind aiAbout'
        msgToPP $ case aiValue of
            Just value -> MsgAddAiStrategy (named kind <> targeted) about value
            Nothing -> MsgAddAiStrategyUnweighted (named kind <> targeted) about
    _ -> preStatement stmt
    where
        (aiKind, aiTarget, aiId, aiValue) =
            foldl' addLine (Nothing, Nothing, Nothing, Nothing) scr
        addLine (k, t, i, v) [pdx| type   = $kind |] = (Just kind, t, i, v)
        addLine (k, t, i, v) [pdx| target = $tgt  |] = (k, Just tgt, i, v)
        addLine (k, t, i, v) [pdx| id     = $vartag:$var |] = (k, t, Just (Right (vartag, var)), v)
        addLine (k, t, i, v) [pdx| id     = $who  |] = (k, t, Just (Left who), v)
        addLine (k, t, i, v) [pdx| id     = ?who  |] = (k, t, Just (Left who), v)
        addLine (k, t, i, v) [pdx| value  = !n    |] = (k, t, i, Just n)
        addLine acc _ = acc
        -- Script names the country under either key: where there is no id, the
        -- target is the country the leaning is about. Where there is one, the
        -- target instead says which dealing is meant, the kind on its own saying
        -- only that it is one.
        aiAbout' = case aiId of
            Just ewho -> Just ewho
            Nothing -> Left <$> aiTarget
        targeted = case (aiId, aiTarget) of
            (Just _, Just tgt) -> " (" <> named tgt <> ")"
            _ -> ""
        -- These two name a thing the country builds or keeps; every other kind
        -- names a country.
        aboutAThing = ["building_target", "save_equipment"]
        aiAbout _ Nothing = return ""
        aiAbout kind (Just ewho)
            | kind `elem` aboutAThing, Left who <- ewho = do
                loc <- getGameL10n who
                return (" for " <> boldText loc)
            | otherwise = do
                mflag <- eflag (Just HOI4Country) ewho
                return (maybe "" (" towards " <>) mflag)
        -- Script writes these as ids; a name is those words with the underscores
        -- taken back out.
        named theid = case T.uncons (T.replace "_" " " theid) of
            Just (c, rest) -> T.cons (toUpper c) rest
            Nothing -> theid
addAiStrategy stmt = preStatement stmt

-- | Handler for a statement referring to a country. Use a flag.
withFlag :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage)  -> StatementHandler g m
withFlag msg stmt@[pdx| %_ = $vartag:$var |] = do
    mwhoflag <- eflag (Just HOI4Country) (Right (vartag, var))
    case mwhoflag of
        Just whoflag -> msgToPP $ msg whoflag ""
        Nothing -> preStatement stmt
withFlag msg [pdx| %_ = $who |] = do
    whoflag <- flagText (Just HOI4Country) who
    msgToPP $ msg whoflag who
withFlag msg [pdx| %_ = ?who |] = do
    whoflag <- flagText (Just HOI4Country) who
    msgToPP $ msg whoflag who
withFlag _ stmt = preStatement stmt

-- | Handler for yes-or-no statements.
withBool :: (HOI4Info g, Monad m) =>
    (Bool -> ScriptMessage)
        -> StatementHandler g m
withBool msg stmt = do
    fullmsg <- withBool' msg stmt
    maybe (preStatement stmt)
          return
          fullmsg

withBoolHOI4Scope :: (HOI4Info g, Monad m) =>
    (Bool -> ScriptMessage) -- ^ Message for country scope
        -> (Bool -> ScriptMessage) -- ^ Message for character scope
        -> StatementHandler g m
withBoolHOI4Scope countrymsg charactermsg stmt = do
    thescope <- getCurrentScope
    case thescope of
        Just HOI4Country -> withBool countrymsg stmt
        _ -> withBool charactermsg stmt

-- | Helper for 'withBool'.
withBool' :: (HOI4Info g, Monad m) =>
    (Bool -> ScriptMessage)
        -> GenericStatement
        -> PPT g m (Maybe IndentedMessages)
withBool' msg [pdx| %_ = ?yn |] | T.map toLower yn `elem` ["yes","no","false"]
    = fmap Just . msgToPP $ case T.toCaseFold yn of
        "yes" -> msg True
        "no"  -> msg False
        "false" -> msg False
        _     -> error "impossible: withBool matched a string that wasn't yes, no or false"
withBool' _ _ = return Nothing

-- | Handler for statements whose RHS may be "yes"/"no" or a tag.
withFlagOrBool :: (HOI4Info g, Monad m) =>
    (Bool -> ScriptMessage)
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withFlagOrBool bmsg _ [pdx| %_ = yes |] = msgToPP (bmsg True)
withFlagOrBool bmsg _ [pdx| %_ = no  |]  = msgToPP (bmsg False)
withFlagOrBool _ tmsg stmt = withFlag tmsg stmt

-- | Handler for statements that have a number and an icon.
numericIcon :: (IsGameState (GameState g), Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericIcon the_icon msg _ [pdx| %_ = !amt |]
    = msgToPP $ msg (iconText the_icon) amt
numericIcon the_icon _ msgvar [pdx| %_ = $amt |]
    = msgToPP $ msgvar (iconText the_icon) amt
numericIcon the_icon _ msgvar [pdx| %_ = $amttag:$amt |]
    = msgToPP $ msgvar (iconText the_icon) (amttag <> ":" <> amt)
numericIcon _ _ _ stmt = plainMsg $ preStatementText' stmt

-- | Handler for statements that have a number and an icon, plus a fixed
-- localizable atom.
numericIconLoc :: (HOI4Info g, Monad m) =>
    Text
        -> Text
        -> (Text -> Text -> Double -> ScriptMessage)
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericIconLoc the_icon what msg _ [pdx| %_ = !amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msg (iconText the_icon) whatloc amt
numericIconLoc the_icon what _ msgvar [pdx| %_ = $amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msgvar (iconText the_icon) whatloc amt
numericIconLoc the_icon what _ msgvar [pdx| %_ = $amttag:$amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msgvar (iconText the_icon) whatloc (amttag <> ":" <> amt)
numericIconLoc _ _ _ _ stmt = plainMsg $ preStatementText' stmt

-- | Handler for statements that have a number and a localizable atom.
numericLoc :: (HOI4Info g, Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> StatementHandler g m
numericLoc what msg [pdx| %_ = !amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msg whatloc amt
numericLoc _ _  stmt = plainMsg $ preStatementText' stmt

----------------------
-- Text/value pairs --
----------------------

-- $textvalue
-- This is for statements of the form
--      head = {
--          what = some_atom
--          value = 3
--      }
-- e.g.
--      num_of_religion = {
--          religion = catholic
--          value = 0.5
--      }
-- There are several statements of this form, but with different "what" and
-- "value" labels, so the first two parameters say what those label are.
--
-- There are two message parameters, one for value < 1 and one for value >= 1.
-- In the example num_of_religion, value is interpreted as a percentage of
-- provinces if less than 1, or a number of provinces otherwise. These require
-- rather different messages.
--
-- We additionally attempt to localize the RHS of "what". If it has no
-- localization string, it gets wrapped in a @<tt>@ element instead.



-- | The game keeps two words for every ideology: the one its party and the
-- people in it go by -- Fascist, Communist -- and the one the belief itself
-- goes by -- Fascism, Communism. Script writes neither, only the word it files
-- the ideology under, so which of the two a line wants has to be said here.
--
-- This is the word for the belief, which is the wiki's own key for the icon
-- rather than anything the game says: a capital on what script wrote, except
-- where the wiki knows the ideology by another name altogether.
beliefTerm :: Text -> Text
beliefTerm atom
    | termed /= atom = termed
    | otherwise = case T.uncons atom of
        Just (c, rest) -> T.cons (toUpper c) rest
        Nothing -> atom
    where termed = iconTerm atom

-- | The wiki's icon for an ideology, named as the belief: what a change in
-- popularity is about is the ideology, not the people who hold it.
beliefIcon :: Text -> Text
beliefIcon = iconText . beliefTerm

-- | The wiki's icon for an ideology, named as the party: the word that belongs
-- in "the ... party", which is the one the game itself localizes the ideology
-- to. An atom that names no ideology at all can stand where one does -- a
-- pronoun for whichever party rules, a country whose government is being
-- matched -- and is left to the icon template as it is written.
partyIconOf :: (HOI4Info g, Monad m) => Text -> PPT g m Text
partyIconOf atom = do
    subideos <- getIdeology
    if atom `elem` HM.elems subideos
        then iconText <$> getGameL10n atom
        else return (iconText atom)

-- | Icon and localization for an ideology whose popularity is changing. The
-- wiki writes the ideology as its icon with the name after it. A scope pronoun
-- in the ideology's place means whichever party rules the country in question,
-- which has no one icon to show, so none is given and the message says so in
-- words.
ideologyIconLoc :: (HOI4Info g, Monad m) => Text -> PPT g m (Text,Text)
ideologyIconLoc atom = do
    (_, what) <- tryLocAndIcon atom
    return (if isPronoun atom then mempty else beliefIcon atom, what)

-- | As 'ideologyIconLoc', for a line about the party rather than the belief.
partyIconLoc :: (HOI4Info g, Monad m) => Text -> PPT g m (Text,Text)
partyIconLoc atom = do
    (_, what) <- tryLocAndIcon atom
    ico <- partyIconOf atom
    return (if isPronoun atom then mempty else ico, what)

-- | Handler for a statement whose RHS names the ideology of a party.
withPartyIcon :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
-- A variable holding the ideology names no one party, so there is no icon to
-- show for it and the message says so in words.
withPartyIcon msg stmt@[pdx| %_ = $vartag:$var |] = do
    mtagloc <- tagged vartag var
    maybe (preStatement stmt) (msgToPP . msg "") mtagloc
withPartyIcon msg [pdx| %_ = ?key |] = do
    what <- Doc.doc2text <$> allowPronoun Nothing (fmap Doc.strictText . getGameL10n) key
    ico <- partyIconOf key
    msgToPP $ msg ico what
withPartyIcon _ stmt = preStatement stmt




data TextValue = TextValue
        {   tv_what :: Maybe Text
        ,   tv_value :: Maybe Double
        ,   tv_var :: Maybe Text
        }
newTV :: TextValue
newTV = TextValue Nothing Nothing Nothing

parseTV :: Foldable t => Text -> Text -> t GenericStatement -> TextValue
parseTV whatlabel vallabel = foldl' addLine newTV
    where
        addLine :: TextValue -> GenericStatement -> TextValue
        addLine tv [pdx| $label = ?what |] | sameKey label whatlabel
            = tv { tv_what = Just what }
        addLine tv [pdx| $label = !val |] | sameKey label vallabel
            = tv { tv_value = Just val }
        addLine tv [pdx| $label = $vartag:$val |] | sameKey label vallabel
            = tv { tv_var = Just (vartag <> ":" <> val) }
        addLine tv [pdx| $label = $val |] | sameKey label vallabel
            = tv { tv_var = Just val }
        addLine nor _ = nor

data TextValueComp = TextValueComp
        {   tvc_what :: Maybe Text
        ,   tvc_value :: Maybe Double
        ,   tvc_var :: Maybe Text
        ,   tvc_comp :: Maybe Text
        }
newTVC :: TextValueComp
newTVC = TextValueComp Nothing Nothing Nothing Nothing

parseTVC :: Foldable t =>
    Text -> Text -> Text -> Text -> t GenericStatement -> TextValueComp
parseTVC whatlabel vallabel gt lt = foldl' addLine newTVC
    where
        addLine :: TextValueComp -> GenericStatement -> TextValueComp
        addLine tvc [pdx| $label = ?what |] | sameKey label whatlabel
            = tvc { tvc_what = Just what }
        addLine tvc [pdx| $label = !val |] | sameKey label vallabel
            = tvc { tvc_value = Just val, tvc_comp = Just ("equal to or " <> gt) }
        addLine tvc [pdx| $label > !val |] | sameKey label vallabel
            = tvc { tvc_value = Just val, tvc_comp = Just gt }
        addLine tvc [pdx| $label < !val |] | sameKey label vallabel
            = tvc { tvc_value = Just val, tvc_comp = Just lt  }
        addLine tvc [pdx| $label = $vartag:$val |] | sameKey label vallabel
            = tvc { tvc_var = Just (vartag <> ":" <> val), tvc_comp = Just ("equal to or " <> gt) }
        addLine tvc [pdx| $label > $vartag:$val |] | sameKey label vallabel
            = tvc { tvc_var = Just (vartag <> ":" <> val), tvc_comp = Just gt }
        addLine tvc [pdx| $label < $vartag:$val |] | sameKey label vallabel
            = tvc { tvc_var = Just (vartag <> ":" <> val), tvc_comp = Just lt }
        addLine tvc [pdx| $label = $val |] | label == vallabel
            = tvc { tvc_var = Just val, tvc_comp = Just ("equal to or " <> gt) }
        addLine tvc [pdx| $label > $val |] | label == vallabel
            = tvc { tvc_var = Just val, tvc_comp = Just gt }
        addLine tvc [pdx| $label < $val |] | label == vallabel
            = tvc { tvc_var = Just val, tvc_comp = Just lt  }
        addLine nor _ = nor

textValue :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value < 1
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> (Text -> PPT g m (Text, Text)) -- ^ Action to localize and get icon (applied to RHS of "what")
        -> StatementHandler g m
textValue whatlabel vallabel valmsg varmsg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (parseTV whatlabel vallabel scr)
    where
        pp_tv :: TextValue -> PPT g m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv, tv_var tv) of
            (Just what, Just value, _) -> do
                (what_icon, what_loc) <- loc what
                return $ valmsg what_icon what_loc value
            (Just what, _, Just var) -> do
                (what_icon, what_loc) <- loc what
                return $ varmsg what_icon what_loc var
            _ -> return $ preMessage stmt
textValue _ _ _ _ _ stmt = preStatement stmt


textValueKey :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, for value
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor, for var
        -> StatementHandler g m
textValueKey whatlabel vallabel valmsg varmsg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (parseTV whatlabel vallabel scr)
    where
        pp_tv :: TextValue -> PPT g m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv, tv_var tv) of
            (Just what, Just value,_) -> do
                what_loc <- getGameL10n what
                return $ valmsg what_loc what value
            (Just what, _, Just var) -> do
                what_loc <- getGameL10n what
                return $ varmsg what_loc what var
            _ -> return $ preMessage stmt
textValueKey _ _ _ _ stmt = preStatement stmt

textValueCompare :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> Text
        -> Text
        -> (Text -> Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, for val
        -> (Text -> Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor, for var
        -> (Text -> PPT g m (Text, Text)) -- ^ Action to localize and get icon (applied to RHS of "what")
        -> StatementHandler g m
textValueCompare whatlabel vallabel gt lt valmsg varmsg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (parseTVC whatlabel vallabel gt lt scr)
    where
        pp_tv :: TextValueComp -> PPT g m ScriptMessage
        pp_tv tvc = case (tvc_what tvc, tvc_value tvc, tvc_var tvc, tvc_comp tvc) of
            (Just what, Just value, _, Just comp) -> do
                (what_icon, what_loc) <- loc what
                return $ valmsg what_icon what_loc comp value
            (Just what, _, Just var, Just comp) -> do
                (what_icon, what_loc) <- loc what
                return $ varmsg what_icon what_loc comp var
            _ -> return $ preMessage stmt
textValueCompare _ _ _ _ _ _ _ stmt = preStatement stmt

withNonlocTextValue :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> ScriptMessage                             -- ^ submessage to send
        -> (Text -> Text -> Double -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> StatementHandler g m
withNonlocTextValue whatlabel vallabel submsg valmsg varmsg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (parseTV whatlabel vallabel scr)
    where
        pp_tv :: TextValue -> PPT g m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv, tv_var tv) of
            (Just what, Just value, _) -> do
                mloc <- getGameL10nIfPresent what
                let loc = fromMaybe "" mloc
                extratext <- messageText submsg
                return $ valmsg extratext what value loc
            (Just what, _, Just var) -> do
                mloc <- getGameL10nIfPresent what
                let loc = fromMaybe "" mloc
                extratext <- messageText submsg
                return $ varmsg extratext what var loc
            _ -> return $ preMessage stmt
withNonlocTextValue _ _ _ _ _ stmt = preStatement stmt

data ValueValue = ValueValue
        {   vv_what :: Maybe Double
        ,   vv_value :: Maybe Double
        ,   vv_var :: Maybe Text
        }
newVV :: ValueValue
newVV = ValueValue Nothing Nothing Nothing

parseVV :: Foldable t => Text -> Text -> t GenericStatement -> ValueValue
parseVV whatlabel vallabel = foldl' addLine newVV
    where
        addLine :: ValueValue -> GenericStatement -> ValueValue
        addLine vv [pdx| $label = !what |] | label == whatlabel
            = vv { vv_what = Just what }
        addLine vv [pdx| $label = !val |] | label == vallabel
            = vv { vv_value = Just val }
        addLine vv [pdx| $label = !val |] | label == vallabel
            = vv { vv_value = Just val }
        addLine nor _ = nor

valueValue :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Double -> Double -> ScriptMessage) -- ^ Message constructor, if abs value < 1
        -> (Double -> Text -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> StatementHandler g m
valueValue whatlabel vallabel valmsg varmsg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_vv (parseVV whatlabel vallabel scr)
    where
        pp_vv :: ValueValue -> PPT g m ScriptMessage
        pp_vv vv = case (vv_what vv, vv_value vv, vv_var vv) of
            (Just what, Just value, _) ->
                return $ valmsg what value
            (Just what, _, Just var) ->
                return $ varmsg what var
            _ -> return $ preMessage stmt
valueValue _ _ _ _ stmt = preStatement stmt

-- | Statements of the form
-- @
--      has_trade_modifier = {
--          who = ROOT
--          name = merchant_recalled
--      }
-- @
data TextAtom = TextAtom
        {   ta_what :: Maybe Text
        ,   ta_atom :: Maybe Text
        }
newTA :: TextAtom
newTA = TextAtom Nothing Nothing

parseTA :: Foldable t => Text -> Text -> t GenericStatement -> TextAtom
parseTA whatlabel atomlabel scr = foldl' addLine newTA scr
    where
        addLine :: TextAtom -> GenericStatement -> TextAtom
        addLine ta [pdx| $label = ?what |]
            | sameKey label whatlabel
            = ta { ta_what = Just what }
        addLine ta [pdx| $label = ?at |]
            | sameKey label atomlabel
            = ta { ta_atom = Just at }
        addLine ta scr = trace ("parseTA: Ignoring " ++ show scr) ta


textAtom :: forall g m. (HOI4Info g, Monad m) =>
    Text -- ^ Label for "what" (e.g. "who")
        -> Text -- ^ Label for atom (e.g. "name")
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> PPT g m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> StatementHandler g m
textAtom whatlabel atomlabel msg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_ta (parseTA whatlabel atomlabel scr)
    where
        pp_ta :: TextAtom -> PPT g m ScriptMessage
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                mwhat_loc <- loc what
                atom_loc <- getGameL10n atom
                let what_icon = iconText what
                    what_loc = fromMaybe (typewriterText what) mwhat_loc
                return $ msg what_icon what_loc atom_loc
            _ -> return $ preMessage stmt
textAtom _ _ _ _ stmt = preStatement stmt

textAtomKey :: forall g m. (HOI4Info g, Monad m) =>
    Text -- ^ Label for "what" (e.g. "who")
        -> Text -- ^ Label for atom (e.g. "name")
        -> (Text -> Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> PPT g m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> StatementHandler g m
textAtomKey whatlabel atomlabel msg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_ta (parseTA whatlabel atomlabel scr)
    where
        pp_ta :: TextAtom -> PPT g m ScriptMessage
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                mwhat_loc <- loc what
                atom_loc <- getGameL10n atom
                let what_loc = fromMaybe (typewriterText what) mwhat_loc
                return $ msg what_loc atom_loc what atom
            _ -> return $ preMessage stmt
textAtomKey _ _ _ _ stmt = preStatement stmt

data TextFlag = TextFlag
        {   tf_what :: Maybe Text
        ,   tf_flag :: Maybe (Either Text (Text, Text))
        }
newTF :: TextFlag
newTF = TextFlag Nothing Nothing

parseTF :: Foldable t => Text -> Text -> t GenericStatement -> TextFlag
parseTF whatlabel flaglabel scr = foldl' addLine newTF scr
    where
        addLine :: TextFlag -> GenericStatement -> TextFlag
        addLine tf [pdx| $label = ?what |]
            | label == whatlabel
            = tf { tf_what = Just what }
        addLine tf [pdx| $label = $target |]
            | label == flaglabel
            = tf { tf_flag = Just (Left target) }
        addLine tf [pdx| $label = $vartag:$var |]
            | label == flaglabel
            = tf { tf_flag = Just (Right (vartag, var)) }
        addLine tf scr = trace ("parseTF: Ignoring " ++ show scr) tf

taTypeFlag :: forall g m. (HOI4Info g, Monad m) => Text -> Text -> (Text -> Text -> ScriptMessage) -> StatementHandler g m
taTypeFlag tType tFlag msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tf (parseTF tType tFlag scr)
    where
        pp_tf :: TextFlag -> PPT g m ScriptMessage
        pp_tf tf = case (tf_what tf, tf_flag tf) of
            (Just typ, Just flag) -> do
                typeLoc <- getGameL10n typ
                flagLoc <- eflag (Just HOI4Country) flag
                case flagLoc of
                   Just flagLocd -> return $ msg typeLoc flagLocd
                   _-> return $ preMessage stmt
            _ -> return $ preMessage stmt
taTypeFlag _ _ _ stmt = preStatement stmt

-- | Helper for effects, where the argument is a single statement in a clause
-- E.g. generate_traitor_advisor_effect

getEffectArg :: Text -> GenericStatement -> Maybe GenericRhs
getEffectArg tArg stmt@[pdx| %_ = @scr |] = case scr of
        [[pdx| $arg = %val |]] | T.toLower arg == tArg -> Just val
        _ -> Nothing
getEffectArg _ _ = Nothing -- CHECK FOR USEFULNESS

simpleEffectNum :: forall g m. (HOI4Info g, Monad m) => Text ->  (Double -> ScriptMessage) -> StatementHandler g m
simpleEffectNum tArg msg stmt =
    case getEffectArg tArg stmt of
        Just (FloatRhs num) -> msgToPP (msg num)
        Just (IntRhs num) -> msgToPP (msg (fromIntegral num))
        _ -> trace ("warning: Not handled by simpleEffectNum: " ++ show stmt) $ preStatement stmt -- CHECK FOR USEFULNESS

simpleEffectAtom :: forall g m. (HOI4Info g, Monad m) => Text -> (Text -> Text -> ScriptMessage) -> StatementHandler g m
simpleEffectAtom tArg msg stmt =
    case getEffectArg tArg stmt of
        Just (GenericRhs atom _) -> do
            loc <- getGameL10n atom
            msgToPP $ msg (iconText atom) loc
        _ -> trace ("warning: Not handled by simpleEffectAtom: " ++ show stmt) $ preStatement stmt -- CHECK FOR USEFULNESS

-- AI decision factors

-- | Extract the appropriate message(s) from an @ai_will_do@ clause.
ppAiWillDo :: (HOI4Info g, Monad m) => AIWillDo -> PPT g m IndentedMessages
ppAiWillDo (AIWillDo mbase mods) = do
    mods_pp'd <- fold <$> traverse ppAiMod mods
    let baseWtMsg = maybe MsgNoBaseWeight MsgAIBaseWeight mbase
    iBaseWtMsg <- msgToPP baseWtMsg
    return $ iBaseWtMsg ++ mods_pp'd

-- | Extract the appropriate message(s) from a @modifier@ section within an
-- @ai_will_do@ clause.
ppAiMod :: (HOI4Info g, Monad m) => AIModifier -> PPT g m IndentedMessages
ppAiMod (AIModifier (Just multiplier) Nothing triggers) = do
    triggers_pp'd <- ppMany triggers
    case triggers_pp'd of
        [(i, triggerMsg)] -> do
            triggerText <- messageText triggerMsg
            return [(i, MsgAIFactorOneline triggerText multiplier)]
        _ -> withCurrentIndentZero $ \i -> return $
            (i, MsgAIFactorHeader multiplier)
            : map (first succ) triggers_pp'd -- indent up
ppAiMod (AIModifier Nothing (Just addition) triggers) = do
    triggers_pp'd <- ppMany triggers
    case triggers_pp'd of
        [(i, triggerMsg)] -> do
            triggerText <- messageText triggerMsg
            return [(i, MsgAIAddOneline triggerText addition)]
        _ -> withCurrentIndentZero $ \i -> return $
            (i, MsgAIAddHeader addition)
            : map (first succ) triggers_pp'd -- indent up
ppAiMod AIModifier {} =
    plainMsg "(missing multiplier/add for this factor)"

-- | Verify assumption about rhs
rhsAlways :: (HOI4Info g, Monad m) => Text -> ScriptMessage -> StatementHandler g m
rhsAlways assumedRhs msg [pdx| %_ = ?rhs |] | T.toLower rhs == assumedRhs = msgToPP msg
rhsAlways _ _ stmt = trace ("Expectation is wrong in statement " ++ show stmt) $ preStatement stmt

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

-- Opinions

-- Add an opinion modifier towards someone (for a number of years).
data AddOpinion = AddOpinion {
        op_who :: Maybe (Either Text (Text, Text))
    ,   op_modifier :: Maybe Text
    ,   op_years :: Maybe Double
    } deriving Show
newAddOpinion :: AddOpinion
newAddOpinion = AddOpinion Nothing Nothing Nothing

-- | Handler for the effects that hand a country an opinion modifier towards
-- another, or take one away again.
--
-- An opinion modifier marked @trade@ moves how the two trade with one another
-- rather than what they think of one another, and the game says so in words of
-- its own. Which of the two an effect comes to is settled by the modifier it
-- names, not by the effect, so the modifier is looked up to tell them apart.
-- The trade wording says nothing of how long it lasts, so it is used however
-- long script asks for.
opinion :: (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> ScriptMessage)
        -> (Text -> Text -> Text -> Double -> ScriptMessage)
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
opinion msgIndef msgDur msgTrade stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_add_opinion (foldl' addLine newAddOpinion scr)
    where
        addLine :: AddOpinion -> GenericStatement -> AddOpinion
        addLine op [pdx| target        = $tag         |] = op { op_who = Just (Left tag) }
        addLine op [pdx| target        = $vartag:$var |] = op { op_who = Just (Right (vartag, var)) }
        addLine op [pdx| modifier      = ?label       |] = op { op_modifier = Just label }
        addLine op [pdx| years         = !n           |] = op { op_years = Just n }
        -- following two for add_mutual_opinion_modifier_effect
        addLine op [pdx| scope_country = $tag         |] = op { op_who = Just (Left tag) }
        addLine op [pdx| scope_country = $vartag:$var |] = op { op_who = Just (Right (vartag, var)) }
        addLine op [pdx| opinion_modifier = ?label    |] = op { op_modifier = Just label }
        addLine op _ = op
        pp_add_opinion op = case (op_who op, op_modifier op) of
            (Just ewhom, Just modifier) -> do
                mwhomflag <- eflag (Just HOI4Country) ewhom
                mod_loc <- getGameL10n modifier
                omods <- getOpinionModifiers
                let isTrade = maybe False (fromMaybe False . omodTrade)
                                    (HM.lookup modifier omods)
                case (mwhomflag, op_years op) of
                    (Just whomflag, _) | isTrade -> return $ msgTrade modifier mod_loc whomflag
                    (Just whomflag, Nothing) -> return $ msgIndef modifier mod_loc whomflag
                    (Just whomflag, Just years) -> return $ msgDur modifier mod_loc whomflag years
                    _ -> return (preMessage stmt)
            _ -> trace ("opinion: who or modifier missing: " ++ show stmt) $ return (preMessage stmt)
opinion _ _ _ stmt = preStatement stmt

-- | Handler for @has_resources_in_country@, which asks how much of a resource a
-- country has to hand.
--
-- Resources come in whole units, so a test for more than forty-nine of something
-- is a test for fifty, and reads better written that way.
-- | How much of a resource is asked for, and of what kind of stock. The same
-- fields are written whether the question is put to one country or to a
-- collection of them.
data ResourceCheck = ResourceCheck
        {   rc_resource :: Maybe Text
        ,   rc_amount :: Maybe (Text, Double)
        ,   rc_qualifier :: Text
        ,   rc_collection :: Maybe Text
        }

parseResourceCheck :: String -> GenericScript -> ResourceCheck
parseResourceCheck what = foldl' addLine (ResourceCheck Nothing Nothing "" Nothing)
    where
        addLine rc [pdx| resource = ?r |] = rc { rc_resource = Just r }
        addLine rc [pdx| amount > !n |] = rc { rc_amount = Just ("at least", n + 1) }
        addLine rc [pdx| amount < !n |] = rc { rc_amount = Just ("less than", n) }
        addLine rc [pdx| amount = !n |] = rc { rc_amount = Just ("exactly", n) }
        -- Whether what the country digs up itself counts, or only what it buys.
        addLine rc [pdx| extracted = yes |] = rc { rc_qualifier = "extracted " }
        addLine rc [pdx| only_imported = yes |] = rc { rc_qualifier = "imported " }
        addLine rc [pdx| extracted = no |] = rc
        addLine rc [pdx| only_imported = no |] = rc
        -- Counts what the country's buildings take rather than what it holds.
        -- Nothing in the wording tells the two apart as yet.
        addLine rc [pdx| buildings = %_ |] = rc
        addLine rc [pdx| collection = ?coll |] = rc { rc_collection = Just coll }
        addLine rc stmt = trace ("unknown section in " ++ what ++ ": " ++ show stmt) rc

hasResourcesInCountry :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasResourcesInCountry stmt@[pdx| %_ = @scr |] =
    case parseResourceCheck "has_resources_in_country" scr of
        ResourceCheck (Just res) (Just (comp, amt)) qualifier _ ->
            msgToPP $ MsgHasResourcesInCountry qualifier comp amt (iconText res)
        _ -> preStatement stmt
hasResourcesInCountry stmt = preStatement stmt

-- | As 'hasResourcesInCountry', over every country a named collection gathers
-- rather than the one in scope. Script writes the collection as @collection:@
-- and its name, which is localized under that name in capitals.
hasResourcesInCollection :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasResourcesInCollection stmt@[pdx| %_ = @scr |] =
    case parseResourceCheck "has_resources_in_collection" scr of
        ResourceCheck (Just res) (Just (comp, amt)) qualifier (Just coll) -> do
            let collkey = fromMaybe coll (T.stripPrefix "collection:" coll)
            collloc <- getGameL10n ("COLLECTION_" <> T.toUpper collkey)
            msgToPP $ MsgHasResourcesInCollection qualifier comp amt (iconText res) collloc
        _ -> preStatement stmt
hasResourcesInCollection stmt = preStatement stmt

-- | Handler for @amount_taken_ideas@, which counts how many of a country's idea
-- slots of a kind are filled. An advisor, a theorist and the rest are all ideas
-- as far as script is concerned, so this is how it asks how many of them a
-- country has taken on.
amountTakenIdeas :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
amountTakenIdeas stmt@[pdx| %_ = @scr |] = case foldl' addLine (Nothing, []) scr of
    (Just (comp, amt), slots@(_:_)) -> do
        slotlocs <- traverse getGameL10n slots
        msgToPP $ MsgAmountTakenIdeas comp amt (joinClauses slotlocs)
    _ -> preStatement stmt
    where
        addLine (amt, slots) [pdx| amount > !n |] = (Just ("more than", n), slots)
        addLine (amt, slots) [pdx| amount < !n |] = (Just ("fewer than", n), slots)
        addLine (amt, slots) [pdx| amount = !n |] = (Just ("exactly", n), slots)
        -- The slots are named one after another with nothing between them, or
        -- written out bare where there is only the one to name.
        addLine (amt, slots) [pdx| slots = @scr |] = (amt, slots ++ mapMaybe slotName scr)
        addLine (amt, slots) [pdx| slots = $slot |] = (amt, slots ++ [slot])
        addLine acc stmt = trace ("unknown section in amount_taken_ideas: " ++ show stmt) acc
        slotName (StatementBare (GenericLhs slot [])) = Just slot
        slotName _ = Nothing
amountTakenIdeas stmt = preStatement stmt

data HasOpinion = HasOpinion
        {   hop_target :: Maybe Text
        ,   hop_value :: Maybe Double
        ,   hop_valuevar :: Maybe Text
        ,   hop_ltgt :: Text
        }
newHasOpinion :: HasOpinion
newHasOpinion = HasOpinion Nothing Nothing Nothing undefined
hasOpinion :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> ScriptMessage) ->
    StatementHandler g m
hasOpinion msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_hasOpinion (foldl' addLine newHasOpinion scr)
    where
        addLine :: HasOpinion -> GenericStatement -> HasOpinion
        addLine hop [pdx| target = ?target |] = hop { hop_target = Just target }
        addLine hop [pdx| value = !val |] = hop { hop_value = Just val, hop_ltgt = "equal to or more than" } -- at least
        addLine hop [pdx| value > !val |] = hop { hop_value = Just val, hop_ltgt = "more than" } -- at least
        addLine hop [pdx| value < !val |] = hop { hop_value = Just val, hop_ltgt = "less than" } -- less than
        addLine hop [pdx| value = $val |] = hop { hop_valuevar = Just val, hop_ltgt = "equal to or more than" } -- at least
        addLine hop [pdx| value > $val |] = hop { hop_valuevar = Just val, hop_ltgt = "more than" } -- at least
        addLine hop [pdx| value < $val |] = hop { hop_valuevar = Just val, hop_ltgt = "less than" } -- less than
        addLine hop _ = trace ("warning: unrecognized has_opinion clause in : " ++ show stmt) hop
        pp_hasOpinion :: HasOpinion -> PPT g m ScriptMessage
        pp_hasOpinion hop = case (hop_target hop, hop_value hop, hop_valuevar hop, hop_ltgt hop) of
            (Just target, Just value, _, ltgt) -> do
                target_flag <- flagText (Just HOI4Country) target
                let valuet = templateColor' (colourNumSign True value)
                return (msg valuet target_flag ltgt)
            (Just target, _, Just valuet, ltgt) -> do
                target_flag <- flagText (Just HOI4Country) target
                return (msg valuet target_flag ltgt)
            _ -> return (preMessage stmt)
hasOpinion _ stmt = preStatement stmt

-- Events

data TriggerEvent = TriggerEvent
        { e_id :: Maybe Text
        , e_title_loc :: Maybe Text
        , e_days :: Maybe Double
        , e_hours :: Maybe Double
        , e_random :: Maybe Double
        , e_random_days :: Maybe Double
        , e_random_hours :: Maybe Double
        }
newTriggerEvent :: TriggerEvent
newTriggerEvent = TriggerEvent Nothing Nothing Nothing Nothing Nothing Nothing Nothing
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
        addLine evt [pdx| hours = %rhs |]
            = return evt { e_hours = floatRhs rhs }
        addLine evt [pdx| random = %rhs |]
            = return evt { e_random = floatRhs rhs }
        addLine evt [pdx| random_days = %rhs |]
            = return evt { e_random_days = floatRhs rhs }
        addLine evt [pdx| random_hours = %rhs |]
            = return evt { e_random_hours = floatRhs rhs }
        addLine evt _ = return evt
        pp_trigger_event :: TriggerEvent -> PPT g m ScriptMessage
        pp_trigger_event evt = do
            evtType_t <- messageText evtType
            case e_id evt of
                Just msgid ->
                    let loc = fromMaybe msgid (e_title_loc evt)
                        time = fromMaybe 0 (e_days evt) * 24 + fromMaybe 0 (e_hours evt)
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

-- Random

random :: (HOI4Info g, Monad m) => StatementHandler g m
random stmt@[pdx| %_ = @scr |]
    | (front, back) <- break
                        (\case
                            [pdx| chance = %_ |] -> True
                            _ -> False)
                        scr
      , not (null back)
      , [pdx| %_ = %rhs |] <- head back
      , Just chance <- floatRhs rhs
      = compoundMessage
          (MsgRandomChance chance)
          [pdx| %undefined = @(front ++ tail back) |]
    | (front, back) <- break
                        (\case
                            [pdx| chance = %_ |] -> True
                            _ -> False)
                        scr
      , not (null back)
      , [pdx| %_ = %rhs |] <- head back
      , Just chancevar <- varName rhs
      = compoundMessage
          (MsgRandomChanceVar chancevar)
          [pdx| %undefined = @(front ++ tail back) |]
    | otherwise = compoundMessage MsgRandom stmt
random stmt = preStatement stmt

-- | The name of the variable a right-hand side holds, where it holds one
-- rather than a number.
varName :: GenericRhs -> Maybe Text
varName (GenericRhs vartag [var]) = Just (vartag <> ":" <> var)
varName (GenericRhs var []) = Just var
varName _ = Nothing


toPct :: Double -> Double
toPct num = fromIntegral (round (num * 1000)) / 10 -- round to one digit after the point

data RandomMod = RandomMod{
     rm_mod  :: GenericScript
    ,rm_rest :: GenericScript
}
newRM :: RandomMod
newRM = RandomMod [] []

randomList :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
randomList stmt@[pdx| %_ = @scr |] =
    let (_,scre) = extractStmt (matchLhsText "seed") scr in -- ugly solution to dealing with seed type in random_list
    let (_,scree) = extractStmt (matchLhsText "log") scre in -- ugly solution to dealing with log type in random_list
    if all chk scree then -- Ugly solution for vars in random list
        fmtRandomList $ map entry scree
    else
        fmtRandomVarList $ map entryv scree
    where
        chk [pdx| !weight = @scr |] = True
        chk [pdx| %var = @scr |] = False
        chk _ = trace ("DEBUG: random_list " ++ show scr) (error "Bad clause in random_list check")
        entry [pdx| !weight = @scr |] = (fromIntegral weight, scr)
        entry _ = trace ("DEBUG: random_list " ++ show scr) (error "Bad clause in random_list, possibly vars?")
        entryv [pdx| $var = @scr |] = (var, scr)
        entryv [pdx| $_:$var = @scr |] = (var, scr)
        entryv [pdx| !weight = @scr |] = (T.pack (show weight), scr)
        entryv _ = trace ("DEBUG: random_list " ++ show scr) (error "Bad clause in random_list, possibly ints?")
        fmtRandomList entries = withCurrentIndent $ \i ->
            let total = sum (map fst entries)
            in (:) (i, MsgRandom) <$> (concat <$> indentUp (mapM (fmtRandomList' total) entries))
        fmtRandomList' total (wt, what) = do
            -- TODO: Could probably be simplified.
            let (mtrigger, rest) = extractStmt (matchLhsText "trigger") what
            rm <- foldM foldModifiers newRM rest
            trig <- (case mtrigger of
                Just s -> indentUp (compoundMessage MsgRandomListTrigger s)
                _ -> return [])
            mod <- ppMods rm
            body <- ppMany (rm_rest rm) -- has integral indentUp
            liftA2 (++)
                (msgToPP $ MsgRandomChanceHOI4 (toPct (wt / total)) wt)
                (pure (trig ++ mod ++ body))
        -- Ugly solution for vars in random list
        fmtRandomVarList entries = withCurrentIndent $ \i ->
            (:) (i, MsgRandom) <$> (concat <$> indentUp (mapM fmtRandomVarList' entries))
        fmtRandomVarList' (wt, what) = do
            -- TODO: Could probably be simplified.
            let (mtrigger, rest) = extractStmt (matchLhsText "trigger") what
            rm <- foldM foldModifiers newRM rest
            trig <- (case mtrigger of
                Just s -> indentUp (compoundMessage MsgRandomListTrigger s)
                _ -> return [])
            mod <- ppMods rm
            body <- ppMany (rm_rest rm) -- has integral indentUp
            liftA2 (++)
                (msgToPP $ MsgRandomVarChance wt)
                (pure (trig ++ mod ++ body))

        ppMods rm = concat <$> indentUp (mapM (\case
                s@[pdx| %_ = @scr |] ->
                    let
                        (mfactor, s') = extractStmt (matchLhsText "factor") scr
                        (madd, sa') = extractStmt (matchLhsText "add") scr
                    in
                        case mfactor of
                            Just [pdx| %_ = !factor |] -> do
                                cond <- ppMany s'
                                liftA2 (++) (msgToPP $ MsgRandomListModifier factor) (pure cond)
                            _ -> case madd of
                                    Just [pdx| %_ = !add |] -> do
                                        cond <- ppMany sa'
                                        liftA2 (++) (msgToPP $ MsgRandomListAddModifier add) (pure cond)
                                    Just [pdx| %_ = $vartag:$var |] -> do
                                        cond <- ppMany sa'
                                        liftA2 (++) (msgToPP $ MsgRandomListAddModifierVar (vartag <> ":" <> var)) (pure cond)
                                    Just [pdx| %_ = $var |] -> do
                                        cond <- ppMany sa'
                                        liftA2 (++) (msgToPP $ MsgRandomListAddModifierVar var) (pure cond)
                                    _ -> preStatement s
                s -> preStatement s) (rm_mod rm))

        foldModifiers :: RandomMod -> GenericStatement -> PPT g m RandomMod
        foldModifiers rm stmt@[pdx| modifier = %s |] = return rm {rm_mod = rm_mod rm ++ [stmt]}
        foldModifiers rm stmt = return rm {rm_rest = rm_rest rm ++ [stmt]}
randomList _ = withCurrentFile $ \file ->
    error ("randomList sent strange statement in " ++ file)

-- DLC

hasDlc :: (HOI4Info g, Monad m) => StatementHandler g m
hasDlc [pdx| %_ = ?dlc |]
    = msgToPP $ MsgHasDLC dlc_icon dlc
    where
        -- Script does not always spell an expansion's name the way its store
        -- page does ("Peace For Our Time"), and the difference is never more
        -- than which letters are capital, so the name is matched without
        -- regard to case.
        mdlc_key = HM.lookup (T.toLower dlc) . HM.fromList . map (first T.toLower) $
            [("Together for Victory", "tfv")
            ,("Death or Dishonor", "dod")
            ,("Waking the Tiger", "wtt")
            ,("Man the Guns", "mtg")
            ,("La Resistance", "lar")
            ,("Battle for the Bosporus", "bftb")
            ,("No Step Back", "nsb")
            ,("By Blood Alone", "bba")
            ,("Arms Against Tyranny", "aat")
            ,("Trial of Allegiance", "toa")
            ,("Gotterdammerung", "gtd")
            ,("Graveyard of Empires", "goe")
            ,("No Compromise, No Surrender", "ncns")
            ,("Peace for Our Time", "pfot")
            ,("Thunder at Our Gates", "taog")
            ]
        dlc_icon = maybe "" iconText mdlc_key
hasDlc stmt = preStatement stmt

-- | Handler for @effect_tooltip@, which shows the effects inside it without
-- executing them.
effectTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
effectTooltip [pdx| %_ = @scr |]
    -- The tooltip says what the effects inside it would do without doing them,
    -- so what it holds is written where the tooltip itself stood. A line of its
    -- own would say nothing, and the indent that comes with one would put its
    -- contents a step further in than the effects standing beside it.
    = indentDown (ppMany scr)
effectTooltip stmt = preStatement stmt

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
        addLine uf scr = trace ("uncompleteFocus: Ignoring " ++ show scr) uf

        ppuf uf = do
            mfoc <- focusIconKeyLoc (uf_focus uf)
            case mfoc of
                Nothing -> return $ preMessage stmt -- unknown national focus
                Just (nfIcon, nfKey, nf_loc) ->
                    return $ msg nfIcon nfKey nf_loc (uf_uncomplete_children uf)
focusUncomplete _ stmt = preStatement stmt

------------------------------
-- Handler for xxx_variable --
------------------------------

data SetVariable = SetVariable
        { sv_which  :: Maybe Text
        , sv_which2 :: Maybe Text
        , sv_value  :: Maybe Double
        , sv_tooltip  :: Maybe Text
        }

newSV :: SetVariable
newSV = SetVariable Nothing Nothing Nothing Nothing

setVariable :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) ->
    (Text -> Double -> ScriptMessage) ->
    StatementHandler g m
setVariable msgWW msgWV stmt@[pdx| %_ = @scr |]
    = pp_sv =<< resolveConstant (foldl' addLine newSV scr)
    where
        addLine :: SetVariable -> GenericStatement -> SetVariable
        addLine sv [pdx| var = ?val |]
            = if isNothing (sv_which sv) then
                sv { sv_which = Just val }
              else
                sv { sv_which2 = Just val }
        addLine sv [pdx| value = !val |]
            = sv { sv_value = Just val }
        addLine sv [pdx| value = ?val |]
            = sv { sv_which2 = Just val }
        addLine sv [pdx| tooltip = ?txt |]
            = sv { sv_tooltip = Just txt }
        addLine sv [pdx| $var = !val |]
            = sv { sv_which = Just var, sv_value = Just val }
        addLine sv [pdx| $var = $val |]
            = sv { sv_which = Just var, sv_which2 = Just val }
        addLine sv [pdx| $vartag:$var = !val |]
            = sv { sv_which = Just (vartag <> ":" <> var), sv_value = Just val }
        addLine sv [pdx| $vartag:$var = $valtag:$val |]
            = sv { sv_which = Just (vartag <> ":" <> var), sv_which2 = Just (valtag <> ":" <> val) }
        addLine sv [pdx| $var = $valtag:$val |]
            = sv { sv_which = Just var, sv_which2 = Just (valtag <> ":" <> val) }
        addLine sv [pdx| $vartag:$var = $val |]
            = sv { sv_which = Just (vartag <> ":" <> var), sv_which2 = Just val }
        addLine sv _ = trace ("failed to parse var: " ++ show stmt) sv
        toTT :: Text -> Text
        toTT = typewriterText
        -- A value script names as a script constant is a number like any other,
        -- and the same number every time, so it is written as that number rather
        -- than as a name only the script knows the meaning of.
        resolveConstant :: SetVariable -> PPT g m SetVariable
        resolveConstant sv = case T.stripPrefix "constant:" =<< sv_which2 sv of
            Just path -> do
                mval <- constantValue path
                return $ case mval of
                    Just val -> sv { sv_which2 = Nothing, sv_value = Just val }
                    Nothing -> sv
            Nothing -> return sv
        -- The modifier localization a tooltip names has a slot for the value
        -- being written, which the game fills in as it draws the tooltip and the
        -- statement has just told us.
        rightArg :: SetVariable -> HashMap Text LocArg
        rightArg sv = case (sv_value sv, sv_which2 sv) of
            (Just val, _) -> HM.singleton "RIGHT" (LocNum val)
            (_, Just var) -> HM.singleton "RIGHT" (LocText (toTT var))
            _ -> HM.empty
        pp_sv :: SetVariable -> PPT g m IndentedMessages
        pp_sv sv = case (sv_which sv, sv_which2 sv, sv_value sv) of
            (Just v1, Just v2, Nothing) -> withTooltip sv (msgWW (toTT v1) (toTT v2)) (msgWW v1 v2)
            (Just v,  Nothing, Just val) -> withTooltip sv (msgWV (toTT v) val) (msgWV v val)
            _ -> preStatement stmt
        -- A statement with a tooltip is shown by that tooltip: it is the
        -- game's own sentence for what the change means, where the statement
        -- itself only says what it does to a name no reader knows. The
        -- literal reading goes behind a hover after it, for whoever wants the
        -- variable itself -- written without markup, like every other hover,
        -- since it ends up in a title attribute where tags don't render.
        withTooltip :: SetVariable -> ScriptMessage -> ScriptMessage -> PPT g m IndentedMessages
        withTooltip sv marked plain = case sv_tooltip sv of
            Nothing -> msgToPP marked
            Just tt -> do
                ttloc <- getGameL10nArgs (rightArg sv) tt
                literal <- messageText plain
                msgToPP $ MsgVariableTooltip ttloc literal
setVariable _ _ stmt = preStatement stmt

data ClampVariable = ClampVariable
        { clv_which  :: Text
        , clv_var :: Maybe Text
        , clv_var2 :: Maybe Text
        , clv_value  :: Maybe Double
        , clv_value2  :: Maybe Double
        }

newCLV :: ClampVariable
newCLV = ClampVariable undefined Nothing Nothing Nothing Nothing

clampVariable :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Double -> Double -> ScriptMessage) ->
    (Text -> Double -> Text -> ScriptMessage) ->
    (Text -> Text -> Double -> ScriptMessage) ->
    (Text -> Text -> Text -> ScriptMessage) ->
    StatementHandler g m
clampVariable msgVV msgVW msgWV msgWW stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_clv (foldl' addLine newCLV scr)
    where
        addLine :: ClampVariable -> GenericStatement -> ClampVariable
        addLine clv [pdx| var = $var |]
            = clv { clv_which = var }
        addLine clv [pdx| variable = $var |]
            = clv { clv_which = var }
        addLine clv [pdx| min = !val |]
            = clv { clv_value = Just val }
        addLine clv [pdx| max = !val |]
            = clv { clv_value2 = Just val }
        addLine clv [pdx| min = $var |]
            = clv { clv_var = Just var}
        addLine clv [pdx| max = $var |]
            = clv { clv_var2 = Just var }
        addLine clv _ = clv
        toTT :: Text -> Text
        toTT = typewriterText
        pp_clv :: ClampVariable -> PPT g m ScriptMessage
        pp_clv clv = case (clv_value clv,  clv_value2 clv,
                             clv_var clv, clv_var2 clv) of
            (Just val, Just val2, Nothing, Nothing) -> do return $ msgVV (clv_which clv) val val2
            (Just val, Nothing, Nothing, Just v2) -> do return $ msgVW (clv_which clv) val (toTT v2)
            (Nothing, Just val2, Just v1, Nothing) -> do return $ msgWV (clv_which clv) (toTT v1) val2
            (Nothing, Nothing, Just v1, Just v2) -> do return $ msgWW (clv_which clv) (toTT v1) (toTT v2)
            _ ->  do return $ preMessage stmt
clampVariable _ _ _ _ stmt = preStatement stmt

data CheckVariable = CheckVariable
        { cv_which  :: Maybe Text
        , cv_which2 :: Maybe Text
        , cv_value  :: Maybe Double
        , cv_comp   :: Text
        }

newCV :: CheckVariable
newCV = CheckVariable Nothing Nothing Nothing "greater than or equals"

checkVariable :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> ScriptMessage) ->
    (Text -> Text -> Double -> ScriptMessage) ->
    StatementHandler g m
checkVariable msgWW msgWV stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_cv (foldl' addLine newCV scr)
    where
        addLine :: CheckVariable -> GenericStatement -> CheckVariable
        addLine cv [pdx| var = $val |]
            = cv { cv_which = Just val }
        addLine cv [pdx| value = !val |]
            = cv { cv_value = Just val }
        addLine cv [pdx| value > !val |]
            = cv { cv_value = Just val, cv_comp = "greater than" }
        addLine cv [pdx| value < !val |]
            = cv { cv_value = Just val, cv_comp = "less than" }
        addLine cv [pdx| value = $val |]
            = cv { cv_which2 = Just val }
        addLine cv [pdx| value > $val |]
            = cv { cv_which2 = Just val, cv_comp = "greater than" }
        addLine cv [pdx| value < $val |]
            = cv { cv_which2 = Just val, cv_comp = "less than" }
        addLine cv [pdx| value = $vartag:$val |]
            = cv { cv_which2 = Just (vartag <> ":" <> val) }
        addLine cv [pdx| value > $vartag:$val |]
            = cv { cv_which2 = Just (vartag <> ":" <> val), cv_comp = "greater than" }
        addLine cv [pdx| value < $vartag:$val |]
            = cv { cv_which2 = Just (vartag <> ":" <> val), cv_comp = "less than" }
        addLine cv [pdx| compare = $comp |]
            = cv { cv_comp = T.replace "_" " " comp}
        -- The short forms below name the variable on the left, so they match any
        -- statement at all. A field we do not know -- @tooltip@, which names the
        -- text the game shows in place of the comparison -- would be read as one
        -- of them and overwrite the variable already found, so a short form is
        -- only taken while nothing has claimed the slot.
        addLine cv [pdx| $var = !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_value = Just val, cv_comp = "equals" }
        addLine cv [pdx| $var < !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_value = Just val, cv_comp = "less than" }
        addLine cv [pdx| $var > !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_value = Just val, cv_comp = "greater than" }
        addLine cv [pdx| $var = $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just val, cv_comp = "equals" }
        addLine cv [pdx| $var < $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just val, cv_comp = "less than" }
        addLine cv [pdx| $var > $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just val, cv_comp = "greater than" }
        addLine cv [pdx| $var = $vartag:$val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just (vartag <> ":" <> val), cv_comp = "equals" }
        addLine cv [pdx| $var < $vartag:$val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just (vartag <> ":" <> val), cv_comp = "less than" }
        addLine cv [pdx| $var > $vartag:$val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just (vartag <> ":" <> val), cv_comp = "greater than" }
        -- A variable of another country's is reached through that country, which
        -- leaves the name on the left carrying tags of its own.
        addLine cv [pdx| $a:$b:$c = $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just (a <> ":" <> b <> ":" <> c), cv_which2 = Just val, cv_comp = "equals" }
        addLine cv _ = cv
        toTT :: Text -> Text
        toTT = typewriterText
        pp_cv :: CheckVariable -> PPT g m ScriptMessage
        pp_cv cv = case (cv_which cv, cv_which2 cv, cv_value cv, cv_comp cv) of
            (Just v1, Just v2, Nothing, comp) -> do
                mconst <- if "constant:" `T.isPrefixOf` v2
                    then constantValue v2
                    else return Nothing
                case mconst of
                    Just val -> return $ msgWV comp (toTT v1) val
                    Nothing -> return $ msgWW comp (toTT v1) (toTT v2)
            (Just v,  Nothing, Just val, comp) -> do return $ msgWV comp (toTT v) val
            _ -> do return $ preMessage stmt
checkVariable _ _ stmt = preStatement stmt

-------------------------------------
-- Handler for export_to_variable  --
-------------------------------------

data ExportVariable = ExportVariable
        { ev_which  :: Maybe Text
        , ev_value :: Maybe Text
        , ev_who :: Maybe Text
        } deriving Show

newEV :: ExportVariable
newEV = ExportVariable Nothing Nothing Nothing

exportVariable :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
exportVariable stmt@[pdx| %_ = @scr |] = msgToPP =<< pp_ev (foldl' addLine newEV scr)
    where
        addLine :: ExportVariable -> GenericStatement -> ExportVariable
        addLine ev [pdx| which = ?val |]
            = ev { ev_which = Just val }
        addLine ev [pdx| variable_name = ?val |]
            = ev { ev_which = Just val }
        addLine ev [pdx| value = ?val |]
            = ev { ev_value = Just val }
        addLine ev [pdx| who = ?val |]
            = ev { ev_who = Just val }
        addLine ev stmt = trace ("Unknown in export_to_variable " ++ show stmt) ev
        toTT :: Text -> Text
        toTT = typewriterText
        pp_ev :: ExportVariable -> PPT g m ScriptMessage
        pp_ev ExportVariable { ev_which = Just which, ev_value = Just value, ev_who = Nothing } =
            return $ MsgExportVariable (toTT which) value
        pp_ev ExportVariable { ev_which = Just which, ev_value = Just value, ev_who = Just who } = do
            whoLoc <- Doc.doc2text <$> allowPronoun (Just HOI4Country) (fmap Doc.strictText . getGameL10n) who
            return $ MsgExportVariableWho (toTT which) value whoLoc
        pp_ev ev = return $ trace ("Missing info for export_to_variable " ++ show ev ++ " " ++ show stmt) $ preMessage stmt
exportVariable stmt = trace ("Not handled in export_to_variable: " ++ show stmt) $ preStatement stmt

-- Helper
getMaybeRhsText :: Maybe GenericStatement -> Maybe Text
getMaybeRhsText (Just [pdx| %_ = $t |]) = Just t
getMaybeRhsText _ = Nothing

-------------------------------------------
-- Handler for add_building_construction --
-------------------------------------------

-- | Which provinces of a state an effect touches: the body of the @province@
-- block that @add_building_construction@, @set_building_level@ and
-- @add_province_modifier@ all share.
data ProvSel = ProvSel
    { ps_ids :: Maybe [Double]
    , ps_all :: Bool
    , ps_coastal :: Bool
    , ps_naval :: Bool
    , ps_border :: Bool
    , ps_border_country :: Maybe (Either Text (Text, Text))
    , ps_supply_node :: Bool
    , ps_vp :: Bool
    , ps_vp_comp :: Text
    , ps_vp_num :: Maybe Double
    , ps_level_comp :: Text
    , ps_level :: Maybe Double
    } deriving Show

newProvSel :: ProvSel
newProvSel = ProvSel Nothing False False False False Nothing False False "" Nothing "" Nothing

-- | One line of a @province@ block.
provSelLine :: String -> ProvSel -> GenericStatement -> ProvSel
provSelLine what ps stmt = case stmt of
    [pdx| all_provinces = %rhs |] -> ifYes rhs ps { ps_all = True }
    [pdx| limit_to_coastal = %rhs |] -> ifYes rhs ps { ps_coastal = True }
    [pdx| limit_to_naval_base = %rhs |] -> ifYes rhs ps { ps_naval = True }
    [pdx| limit_to_border = %rhs |] -> ifYes rhs ps { ps_border = True }
    [pdx| limit_to_supply_node = %rhs |] -> ifYes rhs ps { ps_supply_node = True }
    [pdx| limit_to_border_country = $txt |] -> ps { ps_border_country = Just (Left txt) }
    [pdx| limit_to_border_country = $vartag:$var |] -> ps { ps_border_country = Just (Right (vartag, var)) }
    [pdx| id = !num |] -> ps { ps_ids = Just (fromMaybe [] (ps_ids ps) ++ [num]) }
    [pdx| limit_to_victory_point > !num |] -> ps { ps_vp_comp = "higher than", ps_vp_num = Just num }
    [pdx| limit_to_victory_point < !num |] -> ps { ps_vp_comp = "lower than", ps_vp_num = Just num }
    [pdx| limit_to_victory_point = %rhs |] -> ifYes rhs ps { ps_vp = True }
    [pdx| level > !num |] -> ps { ps_level_comp = "higher than", ps_level = Just num }
    [pdx| level < !num |] -> ps { ps_level_comp = "lower than", ps_level = Just num }
    [pdx| first = %_ |] -> ps -- picks the first valid province after reduction
    [pdx| $other = %_ |] -> trace ("unknown section in " ++ what ++ "@province: " ++ show other) ps
    _ -> trace ("Unknown form in " ++ what ++ "@province: " ++ show stmt) ps
    where
        ifYes (GenericRhs "yes" []) set = set
        ifYes _ _ = ps

-- | Word the province qualifiers the way the three effects say them.
ppProvSel :: forall g m. (HOI4Info g, Monad m) => ProvSel -> PPT g m Text
ppProvSel ps = do
    allmsg <- opt ps_all MsgAllProvinces
    bordmsg <- opt ps_border MsgLimitToBorder
    coastmsg <- opt ps_coastal MsgLimitToCoastal
    navmsg <- opt ps_naval MsgLimitToNavalBase
    supplymsg <- opt ps_supply_node MsgLimitToSupplyNode
    victmsg <- case (ps_vp ps, ps_vp_num ps) of
        (False, Just num) -> messageText $ MsgLimitToVictoryPoint False (ps_vp_comp ps) num
        (True, Nothing) -> messageText $ MsgLimitToVictoryPoint True "" 0
        _ -> return ""
    bordcountmsg <- case ps_border_country ps of
        Just country -> do
            mflagloc <- eflag (Just HOI4Country) country
            messageText $ MsgLimitToBorderCountry (fromMaybe "<!--CHECK SCRIPT-->" mflagloc)
        _ -> return ""
    let provmsg = case ps_ids ps of
            Just ids -> if length ids > 1
                then T.pack $ concat [", on the provinces (" , intercalate "), (" (map (show . round) ids), ")"]
                else T.pack $ concat [", on the province (" , concatMap (show . round) ids, ")"]
            _ -> ""
    levelmsg <- case ps_level ps of
        Just level -> messageText $ MsgProvinceLevel (ps_level_comp ps) level
        _ -> return ""
    return $ allmsg <> bordmsg <> coastmsg <> navmsg <> supplymsg <> victmsg <> bordcountmsg <> provmsg <> levelmsg
    where
        opt f msg = if f ps then messageText msg else return ""

-- | What @add_building_construction@ and @set_building_level@ both write: the
-- building, how many levels, and which provinces.
data BuildStmt = BuildStmt
    { bld_type :: Text
    , bld_level :: Maybe Double
    , bld_levelvar :: Maybe Text
    , bld_instant :: Bool
    , bld_prov :: ProvSel
    } deriving Show

parseBuildStmt :: String -> GenericScript -> BuildStmt
parseBuildStmt what = foldl' addLine (BuildStmt "" Nothing Nothing False newProvSel)
    where
        addLine b [pdx| type = $build |] = b { bld_type = build }
        addLine b [pdx| level = !num |] = b { bld_level = Just num }
        addLine b [pdx| level = $txt |] = b { bld_levelvar = Just txt }
        addLine b [pdx| level = $vartag:$var |] = b { bld_levelvar = Just (vartag <> ":" <> var) }
        addLine b [pdx| instant_build = yes |] = b { bld_instant = True }
        addLine b [pdx| instant_build = %_ |] = b
        addLine b [pdx| province = !num |] = b { bld_prov = (bld_prov b) { ps_ids = Just [num] } }
        addLine b [pdx| province = @pscr |] = b { bld_prov = foldl' (provSelLine what) (bld_prov b) pscr }
        addLine b [pdx| level > !num |] = b { bld_prov = (bld_prov b) { ps_level_comp = "higher than", ps_level = Just num } }
        addLine b [pdx| level < !num |] = b { bld_prov = (bld_prov b) { ps_level_comp = "lower than", ps_level = Just num } }
        addLine b [pdx| $other = %_ |] = trace ("unknown section in " ++ what ++ ": " ++ show other) b
        addLine b stmt = trace ("Unknown in " ++ what ++ ": " ++ show stmt) b

addBuildingConstruction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addBuildingConstruction stmt@[pdx| %_ = @scr |] = msgToPP =<< do
    let b = parseBuildStmt "add_building_construction" scr
        buildicon = iconText (bld_type b)
    prov <- ppProvSel (bld_prov b)
    buildloc <- getGameL10n (bld_type b)
    return $ case (bld_level b, bld_levelvar b) of
        (Just val, _) -> MsgAddBuildingConstruction (bld_instant b) buildicon buildloc val prov
        (_, Just var) -> MsgAddBuildingConstructionVar (bld_instant b) buildicon buildloc var prov
        -- Script may leave out how many levels to build, in which case
        -- the game builds the one. A @level@ written with a comparison
        -- rather than an @=@ is not that number: it is a condition on
        -- what is already standing there, and has gone into 'prov'.
        _ -> MsgAddBuildingConstruction (bld_instant b) buildicon buildloc 1 prov
addBuildingConstruction stmt = trace ("Not handled in addBuildingConstruction: " ++ show stmt) $ preStatement stmt

setBuildingLevel :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setBuildingLevel stmt@[pdx| %_ = @scr |] = msgToPP =<< do
    let b = parseBuildStmt "set_building_level" scr
        buildicon = iconText (bld_type b)
    prov <- ppProvSel (bld_prov b)
    buildloc <- getGameL10n (bld_type b)
    return $ case (bld_level b, bld_levelvar b) of
        (Just val, _) -> MsgSetBuildingLevel buildicon buildloc val prov
        (_, Just var) -> MsgSetBuildingLevelVar buildicon buildloc var prov
        _ -> preMessage stmt
setBuildingLevel stmt = preStatement stmt




----------------------------------
-- Handler for add_named_threat --
----------------------------------
foldCompound "addNamedThreat" "NamedThreat" "nt"
    []
    [CompField "threat" [t|Double|] Nothing True
    ,CompField "name" [t|Text|] Nothing True
    ]
    [|  do
        threatLoc <- getGameL10n _name
        tensionLoc <- getGameL10n "WORLD_TENSION_NAME"
        return $ MsgAddNamedThreat tensionLoc _threat threatLoc
    |]

----------------------------------
-- Handler for create_wargoal --
----------------------------------


----------------------------------
-- Handler for declare_war_on --
----------------------------------


-- | What @create_wargoal@ and @declare_war_on@ both write: the wargoal type,
-- the country it is against, and the states it is over.
data WarGoal = WarGoal
    {   war_type :: Maybe Text
    ,   war_type_loc :: Maybe Text
    ,   war_target_flag :: Maybe Text
    ,   war_expire :: Maybe Double
    ,   war_generator :: Maybe (Either Text [Int]) -- ^ a variable, or the states themselves
    ,   war_states :: [Text]
    } deriving Show

parseWarGoal :: forall g m. (HOI4Info g, Monad m) => String -> GenericScript -> PPT g m WarGoal
parseWarGoal what = foldM addLine (WarGoal Nothing Nothing Nothing Nothing Nothing [])
    where
        addLine :: WarGoal -> GenericStatement -> PPT g m WarGoal
        addLine wg [pdx| type = $wargoal |]
            = (\wgtype_loc -> wg
                   { war_type = Just wargoal
                   , war_type_loc = Just wgtype_loc })
              <$> getGameL10n wargoal
        addLine wg [pdx| target = $vartag:$var |]
            = (\target_loc -> wg { war_target_flag = target_loc })
              <$> eflag (Just HOI4Country) (Right (vartag, var))
        addLine wg [pdx| target = ?target |]
            = (\target_loc -> wg { war_target_flag = target_loc })
              <$> eflag (Just HOI4Country) (Left target)
        addLine wg [pdx| expire = %rhs |]
            = return wg { war_expire = floatRhs rhs }
        addLine wg stmts@[pdx| generator = %state |] = case state of
            CompoundRhs array -> do
                let states = mapMaybe stateFromArray array
                statesloc <- traverse getStateLoc states
                return wg { war_generator = Just (Right states)
                          , war_states = statesloc }
            IntRhs intstate -> do
                statesloc <- getStateLoc intstate
                return wg { war_generator = Just (Right [intstate])
                          , war_states = [statesloc] }
            GenericRhs _vartag [vstate] ->
                return wg { war_generator = Just (Left vstate) } --Need to deal with existing variables here
            GenericRhs vstate _ ->
                return wg { war_generator = Just (Left vstate) } --Need to deal with existing variables here
            _ -> trace ("Unknown generator statement in " ++ what ++ ": " ++ show stmts) $ return wg
        addLine wg stmt
            = trace ("unknown section in " ++ what ++ ": " ++ show stmt) $ return wg

        stateFromArray :: GenericStatement -> Maybe Int
        stateFromArray (StatementBare (IntLhs e)) = Just e
        stateFromArray stmt = trace ("Unknown in generator array statement: " ++ show stmt) Nothing

createWargoal :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createWargoal stmt@[pdx| %_ = @scr |] =
    msgToPP . pp_create_wg =<< parseWarGoal "create_wargoal" scr
    where
        pp_create_wg :: WarGoal -> ScriptMessage
        pp_create_wg wg =
            let states = case war_generator wg of
                    Just (Right arr) -> T.pack $ concat [" for the ", T.unpack $ plural (length arr) "state " "states " , intercalate ", " $ map T.unpack (war_states wg)]
                    Just (Left var) -> " for the states in " <> typewriterText var
                    _ -> ""
            in case (war_type wg, war_type_loc wg,
                     war_target_flag wg,
                     war_expire wg) of
                (Nothing, _, _, _) -> preMessage stmt -- need WG type
                (_, _, Nothing, _) -> preMessage stmt -- need target
                (_, Just wgtype_loc, Just target_flag, Just days) -> MsgCreateWGDuration wgtype_loc target_flag days states
                (Just wgtype, Nothing, Just target_flag, Just days) -> MsgCreateWGDuration wgtype target_flag days states
                (_, Just wgtype_loc, Just target_flag, Nothing) -> MsgCreateWG wgtype_loc target_flag states
                (Just wgtype, Nothing, Just target_flag, Nothing) -> MsgCreateWG wgtype target_flag states
createWargoal stmt = preStatement stmt

-- | Takes away a war goal the country holds. It names the goal and whoever it
-- is held against exactly as @create_wargoal@ does.
removeWargoal :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
removeWargoal stmt@[pdx| %_ = @scr |] =
    msgToPP . pp_remove_wg =<< parseWarGoal "remove_wargoal" scr
    where
        pp_remove_wg :: WarGoal -> ScriptMessage
        pp_remove_wg wg = case (war_type wg, war_type_loc wg, war_target_flag wg) of
            (Nothing, _, _) -> preMessage stmt -- need WG type
            (_, _, Nothing) -> preMessage stmt -- need target
            (_, Just wgtype_loc, Just target_flag) -> MsgRemoveWargoal wgtype_loc target_flag
            (Just wgtype, Nothing, Just target_flag) -> MsgRemoveWargoal wgtype target_flag
removeWargoal stmt = preStatement stmt

declareWarOn :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
declareWarOn stmt@[pdx| %_ = @scr |] =
    msgToPP . pp_declare_war =<< parseWarGoal "declare_war_on" scr
    where
        pp_declare_war :: WarGoal -> ScriptMessage
        pp_declare_war wg =
            let states = case war_generator wg of
                    Just (Right arr) -> T.pack $ concat ["for the ", T.unpack $ plural (length arr) "state " "states " , intercalate ", " $ map T.unpack (war_states wg)]
                    Just (Left var) -> T.pack ("for " ++ T.unpack var)
                    _ -> ""
            in case (war_type wg, war_type_loc wg,
                     war_target_flag wg) of
                (Nothing, _, _) -> preMessage stmt -- need DW type
                (_, _, Nothing) -> preMessage stmt -- need target
                (_, Just dwtype_loc, Just target_flag) -> MsgDeclareWarOn target_flag dwtype_loc states
                (Just dwtype, Nothing, Just target_flag) -> MsgDeclareWarOn target_flag dwtype states
declareWarOn stmt = preStatement stmt

----------------------------------
-- Handler for annex_country --
----------------------------------
foldCompound "annexCountry" "AnnexCountry" "an"
    []
    [CompField "target" [t|Text|] Nothing True
    ,CompField "transfer_troops" [t|Text|] Nothing False
    ]
    [|  do
        let transferTroops = case _transfer_troops of
                Just "yes" -> " (troops transferred)"
                Just "no" -> " (troops not transferred)"
                _ -> ""
        targetTag <- flagText (Just HOI4Country) _target
        return $ MsgAnnexCountry targetTag transferTroops
    |]

--------------------
-- add_tech_boost --
--------------------

data AddTechBonus = AddTechBonus
        {   tb_name :: Maybe Text
        ,   tb_bonus :: Maybe Double
        ,   tb_uses :: Double
        ,   tb_ahead_reduction :: Maybe Double
        ,   tb_category :: [Text]
        ,   tb_technology :: [Text]
        }
newATB :: AddTechBonus
newATB = AddTechBonus Nothing Nothing 1 Nothing [] []
addTechBonus :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addTechBonus stmt@[pdx| %_ = @scr |]
    = pp_atb =<< foldM addLine newATB scr
    where
        addLine :: AddTechBonus -> GenericStatement -> PPT g m AddTechBonus
        addLine atb [pdx| name = $name |] = do
            nameloc <- getGameL10n name
            return atb { tb_name = Just nameloc }
        addLine atb [pdx| bonus = !amt |] =
            return atb { tb_bonus = Just amt }
        addLine atb [pdx| ahead_reduction = !amt |] =
            return atb { tb_ahead_reduction = Just amt }
        addLine atb [pdx| uses = !amt  |]
            = return atb { tb_uses = amt }
        addLine atb [pdx| category = $cat |] = do
            let oldcat = tb_category atb
            catloc <- getGameL10n cat
            return atb { tb_category = oldcat ++ [catloc] }
        addLine atb [pdx| technology = $tech |] = do
            let oldtech = tb_technology atb
            techloc <- getGameL10n tech
            return atb { tb_technology = oldtech ++ [techloc] }
        addLine atb _ = return atb
        pp_atb :: AddTechBonus -> PPT g m IndentedMessages
        pp_atb atb = do
            -- What the bonus goes towards has a page of the wiki to itself,
            -- whether script named a whole category of research or a single
            -- technology out of one.
            let techcat = joinClauses
                    [ "[[" <> tc <> "]]" | tc <- tb_category atb ++ tb_technology atb ]
                uses = tb_uses atb
                tbmsg = case (tb_bonus atb, tb_ahead_reduction atb) of
                    (Just bonus, Just ahead) ->
                        MsgAddTechBonusAheadBoth bonus ahead techcat uses
                    (Just bonus, _) ->
                        MsgAddTechBonus bonus techcat uses
                    (_, Just ahead) ->
                        MsgAddTechBonusAhead ahead techcat uses
                    _ -> trace ("issues in add_technology_bonus: " ++ show stmt ) $ preMessage stmt
            msgToPP tbmsg
addTechBonus stmt = preStatement stmt

-- | Handler for @add_breakthrough_progress@ and @add_breakthrough_points@, which
-- advance a country towards its next special project breakthrough in one
-- specialization, or hand it whole breakthroughs outright.
--
-- The amount may be a number or the name of a script constant, and the
-- specialization may be @all@, in which case there is no one specialization to
-- name and the message says so instead.
addBreakthrough :: (HOI4Info g, Monad m) =>
    ScriptMessage -> (Text -> Double -> ScriptMessage) -> StatementHandler g m
addBreakthrough headmsg linemsg stmt@[pdx| %_ = @scr |] = do
    let (mspec, rest) = extractStmt (matchLhsText "specialization") scr
        (mvalue, _) = extractStmt (matchLhsText "value") rest
    mamt <- maybe (return Nothing) constantOrNumber mvalue
    case (mspec, mamt) of
        (Just [pdx| %_ = $spec |], Just amt) -> do
            specloc <- if spec == "all"
                        then return ""
                        else Doc.oneLine <$> getGameL10n spec
            headpp <- msgToPP headmsg
            linepp <- indentUp (msgToPP (linemsg specloc amt))
            return $ headpp ++ linepp
        _ -> preStatement stmt
addBreakthrough _ _ stmt = preStatement stmt

-- | The number a statement gives, whether it writes the number out or names a
-- script constant holding it. Comes back as 'Nothing' if it does neither, most
-- likely because it uses a variable instead.
constantOrNumber :: (HOI4Info g, Monad m) => GenericStatement -> PPT g m (Maybe Double)
constantOrNumber [pdx| %_ = !num |] = return (Just num)
constantOrNumber [pdx| %_ = ?name |] = constantValue name
constantOrNumber _ = return Nothing

-- | Handler for @gain_xp@, which hands experience to whoever the surrounding
-- scope is about. The trigger of the same name, which asks about a combat, is a
-- block and is left to fall through.
gainXp :: (HOI4Info g, Monad m) => StatementHandler g m
gainXp [pdx| %_ = !amt |] = msgToPP $ MsgGainXp amt
gainXp stmt = preStatement stmt

-- | Handler for @add_scientist_xp@, which is written in the scope of the
-- scientist it is about, so the line above it is what names them. The amount is
-- most often a script constant rather than a number written out.
addScientistXp :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addScientistXp stmt@[pdx| %_ = @scr |] = do
    xp <- maybe (return Nothing) constantOrNumber (fst (extractStmt (matchLhsText "experience") scr))
    case (xp, fst (extractStmt (matchLhsText "specialization") scr)) of
        (Just amt, Just [pdx| %_ = $field |]) -> do
            fieldloc <- getGameL10n field
            msgToPP $ MsgAddScientistXp amt fieldloc
        _ -> preStatement stmt
addScientistXp stmt = preStatement stmt

------------------------------------------
-- handlers for various flag statements --
------------------------------------------
withMaybelocAtom2 :: (HOI4Info g, Monad m) =>
    ScriptMessage
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withMaybelocAtom2 submsg msg [pdx| %_ = ?txt |] = do
    mloc <- getGameL10nIfPresent txt
    let loc = fromMaybe "" mloc
    extratext <- messageText submsg
    msgToPP $ msg extratext txt loc
withMaybelocAtom2 _ _ stmt = preStatement stmt

data SetFlag = SetFlag
        {   sf_flag :: Text
        ,   sf_value :: Maybe Double
        ,   sf_days :: Maybe Double
        ,   sf_dayst :: Maybe Text
        }

newSF :: SetFlag
newSF = SetFlag undefined Nothing Nothing Nothing
setFlag :: forall g m. (HOI4Info g, Monad m) => ScriptMessage -> StatementHandler g m
setFlag msgft stmt@[pdx| %_ = $flag |] = withMaybelocAtom2 msgft MsgSetFlag stmt
setFlag msgft stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_sf =<< foldM addLine newSF scr
    where
        addLine :: SetFlag -> GenericStatement -> PPT g m SetFlag
        addLine sf [pdx| flag = $flag |] =
            return sf { sf_flag = flag }
        addLine sf [pdx| value = !amt |] =
            return sf { sf_value = Just amt }
        addLine sf [pdx| days = !amt |] =
            return sf { sf_days = Just amt }
        addLine sf [pdx| days = $amt |] =
            return sf { sf_dayst = Just amt }
        addLine sf stmt
            = trace ("unknown section in set_country_flag: " ++ show stmt) $ return sf
        pp_sf sf = do
            let value = case sf_value sf of
                    Just num -> T.pack $ " to " ++ show (round num)
                    _ -> ""
                days = case (sf_days sf, sf_dayst sf) of
                    (Just day, _) -> " for " <> formatDays day
                    (_, Just day) -> " for " <> day <> " days"
                    _ -> ""
            mloc <- getGameL10nIfPresent (sf_flag sf)
            let loc = fromMaybe "" mloc
            msgfts <- messageText msgft
            return $ MsgSetFlagFor msgfts (sf_flag sf) value days loc
setFlag _ stmt = preStatement stmt

data HasFlag = HasFlag
        {   hf_flag :: Text
        ,   hf_value :: Maybe Text
        ,   hf_days :: Maybe Text
        ,   hf_date :: Maybe Text
        }

newHF :: HasFlag
newHF = HasFlag undefined Nothing Nothing Nothing
hasFlag :: forall g m. (HOI4Info g, Monad m) => ScriptMessage -> StatementHandler g m
hasFlag msgft stmt@[pdx| %_ = $flag |] = withMaybelocAtom2 msgft MsgHasFlag stmt
hasFlag msgft stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_hf =<< foldM addLine newHF scr
    where
        addLine :: HasFlag -> GenericStatement -> PPT g m HasFlag
        addLine hf [pdx| flag = $flag |] =
            return hf { hf_flag = flag }
        addLine hf [pdx| value = !amt |] =
            let amtd = " Is set equal to or more than " <> show (amt :: Int) <> "." in
            return hf { hf_value = Just $ T.pack amtd }
        addLine hf [pdx| value < !amt |] =
            let amtd = " Is set to less than " <> show (amt :: Int) <> "." in
            return hf { hf_value = Just $ T.pack amtd }
        addLine hf [pdx| value > !amt |] =
            let amtd = " Is set to more than " <> show (amt :: Int) <> "." in
            return hf { hf_value = Just $ T.pack amtd }
        addLine hf [pdx| days < !amt |] =
            let amtd = " Has been set for less than " <> show (amt :: Int) <> " days." in
            return hf { hf_days = Just $ T.pack amtd }
        addLine hf [pdx| days > !amt |] =
            let amtd = " Has been set for more than " <> show (amt :: Int) <> " days." in
            return hf { hf_days = Just $ T.pack amtd }
        addLine hf [pdx| date > %amt |] =
            let amtd = " Has been set later than " <> show amt <> "." in
            return hf { hf_date = Just $ T.pack amtd }
        addLine hf [pdx| date < %amt |] =
            let amtd = " Has been set earlier than " <> show amt <> "." in
            return hf { hf_date = Just $ T.pack amtd }
        addLine hf stmt
            = trace ("unknown section in has_country_flag: " ++ show stmt) $ return hf
        pp_hf hf =
            case (hf_value hf, hf_days hf, hf_date hf) of
                (Nothing, Nothing, Nothing) -> do
                    mloc <- getGameL10nIfPresent (hf_flag hf)
                    let loc = fromMaybe "" mloc
                    msgfts <- messageText msgft
                    return $ MsgHasFlag msgfts (hf_flag hf) loc
                _ -> do
                    mloc <- getGameL10nIfPresent (hf_flag hf)
                    let loc = fromMaybe "" mloc
                    msgfts <- messageText msgft
                    return $ MsgHasFlagFor msgfts (hf_flag hf) (fromMaybe "" (hf_value hf)) (fromMaybe "" (hf_days hf)) (fromMaybe "" (hf_date hf)) loc
hasFlag _ stmt = preStatement stmt

----------------------------------
-- Handler for add_to_war --
----------------------------------
foldCompound "addToWar" "AddToWar" "atw"
    []
    [CompField "targeted_alliance" [t|Text|] Nothing True
    ,CompField "enemy" [t|Text|] Nothing True
    ,CompField "hostility_reason" [t|Text|] Nothing False -- guarantee, asked_to_join, war, ally
    ]
    [|  do
        let reason = case _hostility_reason of
                Just "guarantee" -> ""
                Just "asked_to_join" -> ""
                Just "war" -> ""
                Just "ally" -> ""
                _ -> ""
        ally <- flagText (Just HOI4Country) _targeted_alliance
        enemy <- flagText (Just HOI4Country) _enemy
        return $ MsgAddToWar ally enemy reason
    |]

------------------------------
-- Handler for set_autonomy --
------------------------------
data SetAutonomy = SetAutonomy
        {   sa_target :: Maybe Text
        ,   sa_autonomy_state :: Maybe Text
        ,   sa_freedom_level :: Maybe Double
        ,   sa_end_wars :: Bool
        ,   sa_end_civil_wars :: Bool
        }

newSA :: SetAutonomy
newSA = SetAutonomy Nothing Nothing Nothing True True
setAutonomy :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> Double -> Text -> ScriptMessage) -> StatementHandler g m
setAutonomy msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_sa =<< foldM addLine newSA scr
    where
        addLine :: SetAutonomy -> GenericStatement -> PPT g m SetAutonomy
        addLine sa [pdx| target = $vartag:$var |] = do
            flagd <- eflag (Just HOI4Country) (Right (vartag,var))
            return sa { sa_target = flagd }
        addLine sa [pdx| target = ?txt |] = do
            flagd <- eflag (Just HOI4Country) (Left txt)
            return sa { sa_target = flagd }
        addLine sa [pdx| autonomy_state = $txt |] =
            return sa { sa_autonomy_state = Just txt }
        addLine sa [pdx| autonomous_state = $txt |] =
            return sa { sa_autonomy_state = Just txt }
        addLine sa [pdx| freedom_level = !amt |] =
            return sa { sa_freedom_level = Just (amt :: Double) }
        addLine sa [pdx| end_wars = $yn |] =
            return sa { sa_end_wars = False }
        addLine sa [pdx| end_civil_wars = $yn |] =
            return sa { sa_end_civil_wars = False }
        addLine sa [pdx| release_non_owned_controlled = %_|] =
            return sa
        addLine sa stmt
            = trace ("unknown section in set_autonomy: " ++ show stmt) $ return sa
        pp_sa sa = do
            let endwar = case (sa_end_wars sa, sa_end_civil_wars sa) of
                    (True, True) -> T.pack " and end wars and civil wars for subject"
                    (True, False) -> T.pack " and end wars for subject"
                    (False, True) -> T.pack " and end civil wars for subject"
                    _ -> ""
                freedom = fromMaybe 0 (sa_freedom_level sa)
                autonomy_state = fromMaybe "<!-- Check Script -->" (sa_autonomy_state sa)
                target = fromMaybe "<!-- Check Script -->" (sa_target sa)
            autonomy <- getGameL10n autonomy_state
            return $ msg target (iconText autonomy) autonomy freedom endwar
setAutonomy _ stmt = preStatement stmt

------------------------------
-- Handler for set_politics --
------------------------------
data SetPolitics = SetPolitics
        {   sp_ruling_party :: Text
        ,   sp_elections_allowed :: Maybe Text
        ,   sp_last_election :: Maybe Text
        ,   sp_election_frequency :: Maybe Double
        ,   sp_election_frequencyvar :: Maybe Text
        ,   sp_long_name :: Maybe Text
        ,   sp_name :: Maybe Text
        }

newSP :: SetPolitics
newSP = SetPolitics "<!-- Check Script -->" Nothing Nothing Nothing Nothing Nothing Nothing
setPolitics :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setPolitics stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_sp =<< foldM addLine newSP scr
    where
        addLine :: SetPolitics -> GenericStatement -> PPT g m SetPolitics
        addLine sp [pdx| ruling_party = $txt |] = return sp { sp_ruling_party = txt }
        addLine sp [pdx| elections_allowed = %_ |] = return sp
        addLine sp [pdx| last_election = %_ |] = return sp
        addLine sp [pdx| election_frequency = $txt |] =
            return sp { sp_election_frequencyvar = Just txt }
        addLine sp [pdx| election_frequency = !amt |] =
            return sp { sp_election_frequency = Just (amt :: Double) }
        addLine sp [pdx| long_name = $yn |] = return sp
        addLine sp [pdx| name = $yn |] = return sp
        addLine sp stmt
            = trace ("unknown section in set_politics: " ++ show stmt) $ return sp
        pp_sp sp = do
            let freq = fromMaybe 0 (sp_election_frequency sp)
            party <- getGameL10n (sp_ruling_party sp)
            case sp_election_frequencyvar sp of
                Just freq -> return $ MsgSetPoliticsVar (iconText party) party freq
                _ -> return $ MsgSetPolitics (iconText party) party freq
setPolitics stmt = preStatement stmt

------------------------------------
-- Handler for has_country_leader --
------------------------------------
foldCompound "hasCountryLeader" "HasLeader" "hcl"
    []
    [CompField "character" [t|Text|] Nothing False
    ,CompField "ruling_only" [t|Text|] Nothing False
    ,CompField "name" [t|Text|] Nothing False
    ,CompField "id" [t|Double|] Nothing False
    ]
    [|  do
        let charjust = case (_character, _name, _id) of
                (Just character, _, _) -> character
                (_, Just name, _) -> name
                (_, _, Just id)-> T.pack $ show $ floor id
                _ -> "<!-- Check Script -->"
        charloc <- getGameL10n charjust
        return $ MsgHasCountryLeader charloc
    |]

------------------------------------
-- Handler for has_country_leader --
------------------------------------
foldCompound "setPartyName" "SetPartyName" "spn"
    []
    [CompField "ideology" [t|Text|] Nothing True
    ,CompField "long_name" [t|Text|] Nothing False
    ,CompField "name" [t|Text|] Nothing True
    ]
    [|  do
        let long_name = fromMaybe "" _long_name
        long_loc <- getGameL10n long_name
        short_loc <- getGameL10n _name
        ideoicon <- partyIconOf _ideology
        return $ MsgSetPartyName ideoicon short_loc long_loc
    |]

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

noloc :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
noloc txt = return Nothing
---------------------------------
-- Handler for set_nationality --
---------------------------------

-- | Handler for @set_nationality@, which moves a character to another country.
-- Script writes the country on its own where the character is the scope the
-- effect stands in, and names both where it is not -- and it writes the block
-- form with only the country in it as well, which says the same as the bare
-- one.
setNationality :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setNationality stmt@[pdx| %_ = @scr |] =
    case extractStmt (matchLhsText "target_country") scr of
        (Just target, rest) | not (any (matchLhsText "character") rest) ->
            withFlag MsgSetNationality target
        _ -> taTypeFlag "character" "target_country" MsgSetNationalityChar stmt
-- Whatever else names the country -- a tag, a pronoun, a variable holding one.
setNationality stmt = withFlag MsgSetNationality stmt

hasWarGoalAgainst :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasWarGoalAgainst stmt@[pdx| %_ = $txt |] = withFlag MsgHasWargoalAgainst stmt
-- A war goal of no named type is only the country it is held against, which is
-- what the trigger's plain form says of it.
hasWarGoalAgainst stmt@[pdx| %_ = @scr |] = case extractStmt (matchLhsText "type") scr of
    (Just _, _) -> textAtom "target" "type" MsgHasWargoalAgainstType (fmap Just . flagText (Just HOI4Country)) stmt
    _ -> case extractStmt (matchLhsText "target") scr of
        (Just target, _) -> withFlag MsgHasWargoalAgainst target
        _ -> preStatement stmt
hasWarGoalAgainst stmt = preStatement stmt

-------------------------------------
-- Handler for diplomatic_relation --
-------------------------------------
foldCompound "diplomaticRelation" "DiplomaticRelation" "dr"
    []
    [CompField "country" [t|Text|] Nothing True
    ,CompField "relation" [t|Text|] Nothing True
    ,CompField "active" [t|Text|] Nothing False
    ]
    [|  do
        let active = case _active of
                Just "no" -> False
                Just "yes" -> True
                _ -> True
            relation = case _relation of
                "non_aggression_pact" -> if active then "Enters a {{icon|nap|1}} with " else "Disbands the {{icon|nap|1}} with "
                "guarantee" -> if active then "Guarantee " else "Revokes guarantee for "
                "puppet" -> if active then "Becomes a subject of " else "Is no longer a subject of "
                "military_access" -> if active then "Grants military access for " else "Revokes military access for "
                "docking_rights" -> if active then "Grants docking rights for " else "Revokes docking rights for "
                _ -> "<!-- Check Script -->"
        flag <- flagText (Just HOI4Country) _country
        return $ MsgDiplomaticRelation relation flag
    |]

hasArmySize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasArmySize stmt@[pdx| %_ = @scr |] = do
    let (_size, _) = extractStmt (matchLhsText "size") scr
        (_type, _) = extractStmt (matchLhsText "type") scr
    (comp, amt) <- case _size of
        Just [pdx| %_ < !num |] -> return ("less than", num)
        Just [pdx| %_ > !num |] -> return ("more than", num)
        _ -> return ("<!-- Check Script -->", 0)
    typed <- case _type of
        Just [pdx| %_ = $txt |] -> if txt == "anti_tank" then return " anti-tank" else return $ " " <> txt
        _ -> return " "
    msgToPP $ MsgHasArmySize comp amt typed
hasArmySize stmt = preStatement stmt

startCivilWar :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
startCivilWar stmt@[pdx| %_ = @scr |] = do
    let (_ideology, _) = extractStmt (matchLhsText "ideology") scr
        (_size, _) = extractStmt (matchLhsText "size") scr
    size <- case _size of
        Just [pdx| %_ = !num |] -> return $ templateColor'(reducedNum (colourPc False) num)
        Just [pdx| %_ = ?var |] -> return var
        _ -> return "<!-- Check Script -->"
    ideology <- case _ideology of
        Just [pdx| %_ = $txt |] -> getGameL10n txt
        _ -> return "<!-- Check Script -->"
    msgToPP $ MsgStartCivilWar ideology size
startCivilWar stmt = preStatement stmt

createEquipmentVariant :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createEquipmentVariant stmt@[pdx| %_ = @scr |] = do
    let (_name, _) = extractStmt (matchLhsText "name") scr
        (_typed, _) = extractStmt (matchLhsText "type") scr
    name <- case _name of
        Just [pdx| %_ = ?txt |] -> return txt
        _ -> return "<!-- Check Script -->"
    typed <- case _typed of
        Just [pdx| %_ = $txt |] -> getGameL10n txt
        _ -> return "<!-- Check Script -->"
    msgToPP $ MsgCreateEquipmentVariant typed name
createEquipmentVariant stmt = preStatement stmt

-- | Handler for set_rule
setRule :: forall g m. (HOI4Info g, Monad m) =>
    (Double -> ScriptMessage) -- ^ Message to use as the block header
    -> StatementHandler g m
setRule header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        -- @desc@ names a piece of text the game shows in place of the wording a
        -- rule would otherwise be given. It is not a rule and is not one of the
        -- ones counted.
        let (_, rules) = extractStmt (matchLhsText "desc") scr
        rules_pp'd <- ppRules rules
        let numrules = fromIntegral $ length rules
        return ((i, header numrules) : rules_pp'd)
    where
        ppRules :: GenericScript -> PPT g m IndentedMessages
        ppRules scr = indentUp (concat <$> mapM ppRule scr)
        ppRule :: StatementHandler g m
        ppRule stmt@[pdx| $lhs = ?yn |] =
            case yn of
                "yes" -> do
                    let lhst = T.toUpper lhs
                    loc <- getGameL10n lhst
                    msgToPP $ MsgSetRuleYesNo "{{icon|yes}}" loc
                "no" -> do
                    let lhst = T.toUpper lhs
                    loc <- getGameL10n lhst
                    msgToPP $ MsgSetRuleYesNo "{{icon|no}}" loc
                _ -> preStatement stmt
        ppRule stmt = trace ("unknownsecton found in set_rule for " ++ show stmt) preStatement stmt
setRule _ stmt = preStatement stmt

-------------------------------------
-- Handler for add_doctrine_cost_reduction  --
-------------------------------------
-- | The wiki page a doctrine folder is written about on.
doctrinePage :: Text -> Text
doctrinePage folder = case T.uncons (T.replace "_" " " folder) of
    Just (c, rest) -> T.cons (toUpper c) rest <> " doctrine"
    Nothing -> "Doctrine"

-- | The folders the doctrine tree is divided into, which are the doctrine pages
-- the wiki has.


-- | A link to the page a doctrine category is written about, under the name the
-- game gives the category. Only the folders have a page each; a category
-- narrower than a folder has none of its own, and is left as its name.
doctrineCatLink :: Text -> Text -> Text
doctrineCatLink cat catloc = case T.stripSuffix "_doctrine" cat of
    Just folder | folder `elem` doctrineFolderIds ->
        mconcat ["[[", doctrinePage folder, "|", catloc, "]]"]
    _ -> catloc

data DoctrineCostReduction = DoctrineCostReduction
        {   dcr_name :: Maybe Text
        ,   dcr_cost_reduction :: Double
        ,   dcr_uses :: Double
        ,   dcr_category :: [Text]
        ,   dcr_technology :: [Text]
        }
newDCR :: DoctrineCostReduction
newDCR = DoctrineCostReduction Nothing 0 1 [] []
addDoctrineCostReduction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addDoctrineCostReduction stmt@[pdx| %_ = @scr |]
    = pp_dcr =<< foldM addLine newDCR scr
    where
        addLine :: DoctrineCostReduction -> GenericStatement -> PPT g m DoctrineCostReduction
        addLine dcr [pdx| name = $name |] = do
            nameloc <- getGameL10n name
            return dcr { dcr_name = Just nameloc }
        addLine dcr [pdx| cost_reduction = !amt |] =
            return dcr { dcr_cost_reduction = amt }
        addLine dcr [pdx| uses = !amt  |]
            = return dcr { dcr_uses = amt }
        addLine dcr [pdx| category = $cat |] = do
            let oldcat = dcr_category dcr
            catloc <- getGameL10n cat
            return dcr { dcr_category = oldcat ++ [doctrineCatLink cat catloc] }
        addLine dcr [pdx| technology = $tech |] = do
            let oldtech = dcr_technology dcr
            techloc <- getGameL10n tech
            return dcr { dcr_technology = oldtech ++ ["[[" <> techloc <> "]]"] }
        addLine dcr _ = return dcr
        -- What the reduction covers is said in the same breath as the
        -- reduction itself, however many doctrines that is.
        pp_dcr :: DoctrineCostReduction -> PPT g m IndentedMessages
        pp_dcr dcr = msgToPP $ MsgAddDoctrineCostReduction
            (dcr_uses dcr)
            (dcr_cost_reduction dcr)
            (joinClauses (dcr_category dcr ++ dcr_technology dcr))
addDoctrineCostReduction stmt = preStatement stmt

-------------------------------------
-- Handler for free_building_slots --
-------------------------------------
data FreeBuildingSlots = FreeBuildingSlots
        {   fbs_building :: Text
        ,   fbs_size :: Double
        ,   fbs_comp :: Text
        ,   fbs_include_locked :: Bool
        ,   fbs_province :: Maybe Int
        }

newFBS :: FreeBuildingSlots
newFBS = FreeBuildingSlots undefined undefined undefined False Nothing
freeBuildingSlots  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
freeBuildingSlots stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_fbs =<< foldM addLine newFBS scr
    where
        addLine :: FreeBuildingSlots -> GenericStatement -> PPT g m FreeBuildingSlots
        addLine fbs [pdx| building = $txt |] = do
            return fbs { fbs_building = txt }
        addLine fbs [pdx| size < !amt |] =
            let comp = "less than" in
            return fbs { fbs_comp = comp, fbs_size = amt }
        addLine fbs [pdx| size > !amt |] =
            let comp = "more than" in
            return fbs { fbs_comp = comp, fbs_size = amt}
        addLine fbs [pdx| size = !amt |] =
            let comp = "more than" in
            return fbs { fbs_comp = comp, fbs_size = amt}
        addLine fbs [pdx| include_locked = yes |] =
            return fbs { fbs_include_locked = True }
        addLine fbs [pdx| include_locked = %_ |] =
            return fbs { fbs_include_locked = False }
        addLine fbs [pdx| province = !num |] =
            return fbs { fbs_province = Just num }
        addLine fbs stmt
            = trace ("unknown section in free_building_slots: " ++ show stmt) $ return fbs
        pp_fbs fbs = do
            let buildicon = iconText $ fbs_building fbs
                provloc = maybe "" (\p -> " in province (" <> T.pack (show p) <> ")") (fbs_province fbs)
                buildiconloc = buildicon <> provloc
            return $ MsgFreeBuildingSlots (fbs_comp fbs) (fbs_size fbs) buildiconloc (fbs_include_locked fbs)
freeBuildingSlots stmt = preStatement stmt

addAutonomyRatio :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Double -> ScriptMessage) -> (Text -> Text -> Text -> ScriptMessage) -> StatementHandler g m
addAutonomyRatio valmsg varmsg stmt@[pdx| %_ = @scr |] = case length scr of
    2 -> textValue "localization" "value" valmsg varmsg tryLocMaybe stmt
    1 -> do
        let (_value, _) = extractStmt (matchLhsText "value") scr
        case _value of
            Just [pdx| %_ = !num |] -> msgToPP $ valmsg "" "" num
            Just [pdx| %_ = $var |] -> msgToPP $ varmsg "" "" var
            _ -> preStatement stmt
    _ -> preStatement stmt
addAutonomyRatio _ _ stmt = preStatement stmt

--------------------------------
-- Handler for send_equipment --
--------------------------------
data SendEquipment = SendEquipment
        {   se_equipment :: Text
        ,   se_amount :: Text
        ,   se_old_prioritised :: Bool
        ,   se_target :: Maybe Text
        }

newSE :: SendEquipment
newSE = SendEquipment undefined undefined False Nothing
sendEquipment  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
sendEquipment stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_se =<< foldM addLine newSE scr
    where
        addLine :: SendEquipment -> GenericStatement -> PPT g m SendEquipment
        addLine se [pdx| equipment = $txt |] = do
            txtd <- getGameL10n txt
            return se { se_equipment = txtd }
        addLine se [pdx| type = $txt |] = do
            txtd <- getGameL10n txt
            return se { se_equipment = txtd }
        addLine se [pdx| amount = !amt |] =
            let amtd = T.pack $ show (amt :: Int)  in
            return se { se_amount = amtd }
        addLine se [pdx| amount = $amt |] =
            return se { se_amount = amt }
        addLine se [pdx| old_prioritised = %rhs |]
            | GenericRhs "yes" [] <- rhs = return se { se_old_prioritised = True }
            | GenericRhs "no"  [] <- rhs = return se { se_old_prioritised = False }
        addLine se [pdx| target = $vartag:$var |] = do
            flagd <- eflag (Just HOI4Country) (Right (vartag,var))
            return se { se_target = flagd }
        addLine se [pdx| target = $tag |] = do
            flagd <- eflag (Just HOI4Country) (Left tag)
            return se { se_target = flagd }
        addLine se stmt
            = trace ("unknown section in send_equipment: " ++ show stmt) $ return se
        pp_se se = do
            let target = fromMaybe "<!-- Check Script -->" (se_target se)
            return $ MsgSendEquipment (se_amount se) (se_equipment se) target (se_old_prioritised se)
sendEquipment stmt = preStatement stmt

--------------------------------
-- Handler for build_railway --
--------------------------------




-- | What @build_railway@ and @can_build_railway@ both write: a railway level
-- and where it runs, as a path, a pair of states, or a pair of provinces.
data Railway = Railway
        {   rail_level :: Double
        ,   rail_path :: Maybe [Double]
        ,   rail_start_state :: Maybe Text
        ,   rail_target_state :: Maybe Text
        ,   rail_start_province :: Maybe Double
        ,   rail_target_province :: Maybe Double
        }

parseRailway :: forall g m. (HOI4Info g, Monad m) => String -> GenericScript -> PPT g m Railway
parseRailway what = foldM addLine (Railway 1 Nothing Nothing Nothing Nothing Nothing)
    where
        addLine :: Railway -> GenericStatement -> PPT g m Railway
        addLine br stmt@[pdx| $lhs = %rhs |] = case lhs of
            "level" -> case rhs of
                (floatRhs -> Just num) -> return br { rail_level = num }
                _ -> trace ("bad level in " ++ what) $ return br
            "build_only_on_allied" -> return br
            "fallback" -> return br
            "controller_priority" -> return br
            "path" -> case rhs of
                CompoundRhs arr ->
                    let provs = mapMaybe provinceFromArray arr in
                    return br { rail_path = Just provs }
                _ -> trace ("bad path in " ++ what) $ return br
            "start_state" -> (\loc -> br { rail_start_state = loc }) <$> stateRhs rhs
            "target_state" -> (\loc -> br { rail_target_state = loc }) <$> stateRhs rhs
            "start_province" -> return br { rail_start_province = floatRhs rhs }
            "target_province" -> return br { rail_target_province = floatRhs rhs }
            _other -> trace ("unknown section in " ++ what ++ ": " ++ show stmt) $ return br
        addLine br stmt
            = trace ("unknown form in " ++ what ++ ": " ++ show stmt) $ return br

        -- A state written as its id, a variable, or a tagged variable.
        stateRhs :: GenericRhs -> PPT g m (Maybe Text)
        stateRhs = \case
            IntRhs num -> Just <$> getStateLoc num
            GenericRhs vartag [var] -> eGetState (Right (vartag, var))
            GenericRhs txt [] -> eGetState (Left txt)
            _ -> trace ("bad state in " ++ what) $ return Nothing

        provinceFromArray :: GenericStatement -> Maybe Double
        provinceFromArray (StatementBare (IntLhs e)) = Just $ fromIntegral e
        provinceFromArray stmt = trace ("Unknown in generator array statement: " ++ show stmt) Nothing

buildRailway  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
buildRailway stmt@[pdx| %_ = @scr |]
    = msgToPP . pp_br =<< parseRailway "build_railway" scr
    where
        pp_br br = case rail_path br of
            Just path ->
                let paths = T.pack $ concat ["on the provinces (" , intercalate "), (" (map (show . round) path),")"]
                in MsgBuildRailwayPath (rail_level br) paths
            _ -> case (rail_start_state br, rail_target_state br,
                       rail_start_province br, rail_target_province br) of
                    (Just start, Just end, _,_) -> MsgBuildRailway (rail_level br) start end
                    (_,_, Just start, Just end) -> MsgBuildRailwayProv (rail_level br) start end
                    _ -> preMessage stmt
buildRailway stmt = preStatement stmt

canBuildRailway  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
canBuildRailway stmt@[pdx| %_ = @scr |]
    = msgToPP . pp_cbr =<< parseRailway "can_build_railway" scr
    where
        pp_cbr cbr = case rail_path cbr of
            Just path ->
                let paths = T.pack $ concat ["on the provinces (" , intercalate "), (" (map (show . round) path),")"]
                in MsgCanBuildRailwayPath paths
            _ -> case (rail_start_state cbr, rail_target_state cbr,
                       rail_start_province cbr, rail_target_province cbr) of
                    (Just start, Just end, _,_) -> MsgCanBuildRailway start end
                    (_,_, Just start, Just end) -> MsgCanBuildRailwayProv start end
                    _ -> preMessage stmt
canBuildRailway stmt = preStatement stmt

-- | Whether the railway @can_build_railway@ would build is already there. The
-- two are written with the same fields.
hasRailwayConnection :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasRailwayConnection stmt@[pdx| %_ = @scr |]
    = msgToPP . pp_hrc =<< parseRailway "has_railway_connection" scr
    where
        pp_hrc hrc = case rail_path hrc of
            Just path ->
                let paths = T.pack $ concat ["on the provinces (" , intercalate "), (" (map (show . round) path),")"]
                in MsgHasRailwayConnectionPath paths
            _ -> case (rail_start_state hrc, rail_target_state hrc,
                       rail_start_province hrc, rail_target_province hrc) of
                    (Just start, Just end, _,_) -> MsgHasRailwayConnection start end
                    (_,_, Just start, Just end) -> MsgHasRailwayConnectionProv start end
                    _ -> preMessage stmt
hasRailwayConnection stmt = preStatement stmt

------------------------------
-- handler for has_license  --
------------------------------

-- | Whether the country holds a production license, which names either an
-- archetype or one exact piece of equipment, and may name who granted it.
hasLicense :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasLicense stmt@[pdx| %_ = @scr |] = do
    let (mfrom, s1) = extractStmt (matchLhsText "from") scr
        (marchetype, s2) = extractStmt (matchLhsText "archetype") s1
        (mequipment, _) = extractStmt (matchLhsText "equipment") s2
    whoflag <- case mfrom of
        Just [pdx| %_ = ?who |] -> flagText (Just HOI4Country) who
        _ -> return ""
    mwhat <- case (marchetype, mequipment) of
        (Just [pdx| %_ = ?arch |], _) -> Just <$> getGameL10n arch
        (_, Just [pdx| %_ = @equip |]) ->
            case fst (extractStmt (matchLhsText "type") equip) of
                Just [pdx| %_ = ?ty |] -> Just <$> getGameL10n ty
                _ -> return Nothing
        _ -> return Nothing
    case mwhat of
        Just what -> msgToPP $ MsgHasLicense whoflag what
        Nothing -> preStatement stmt
hasLicense stmt = preStatement stmt

------------------------------------------------
-- handlers for the intelligence effects       --
------------------------------------------------

-- | Intel gained on another country, which is counted separately for each of
-- the four branches.
addIntel :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addIntel stmt@[pdx| %_ = @scr |] = do
    let (mtarget, kinds) = extractStmt (matchLhsText "target") scr
    case mtarget of
        Just [pdx| %_ = ?who |] -> do
            whoflag <- flagText (Just HOI4Country) who
            header <- msgToPP $ MsgAddIntel whoflag
            branches <- indentUp (concat <$> traverse ppKind kinds)
            return $ header ++ branches
        _ -> preStatement stmt
    where
        ppKind :: GenericStatement -> PPT g m IndentedMessages
        ppKind [pdx| $kind = !num |] = do
            kindloc <- getGameL10n (intelKey kind)
            msgToPP $ MsgAddIntelKind kindloc num
        ppKind st = preStatement st
        -- The ledger the game names these on calls the air branch AIR_INTEL,
        -- where script writes airforce_intel.
        intelKey "airforce_intel" = "AIR_INTEL"
        intelKey kind = T.toUpper kind
addIntel stmt = preStatement stmt

-- | Decryption gained against another country, given either as a flat amount or
-- as a share of what the target has to defend with.
addDecryption :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addDecryption stmt@[pdx| %_ = @scr |] = do
    let (mtarget, rest) = extractStmt (matchLhsText "target") scr
        (mratio, rest') = extractStmt (matchLhsText "ratio") rest
        (mamount, _) = extractStmt (matchLhsText "amount") rest'
    whoflag <- case mtarget of
        Just [pdx| %_ = ?who |] -> flagText (Just HOI4Country) who
        _ -> return ""
    case (mratio, mamount) of
        (Just [pdx| %_ = !num |], _) -> msgToPP $ MsgAddDecryptionRatio whoflag num
        (_, Just [pdx| %_ = !num |]) -> msgToPP $ MsgAddDecryptionAmount whoflag num
        _ -> preStatement stmt
addDecryption stmt = preStatement stmt

--------------------------------------------------------
-- handlers for the country-wide division template locks --
--------------------------------------------------------

countryLockAllDivisionTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
-- The reason the lock is given is a localization key the game shows in place of
-- the wording it would otherwise use, which says nothing of what changes.
countryLockAllDivisionTemplate [pdx| %_ = @scr |] =
    msgToPP $ MsgLockDivision (null [() | [pdx| is_locked = no |] <- scr])
countryLockAllDivisionTemplate [pdx| %_ = no |] = msgToPP $ MsgLockDivision False
countryLockAllDivisionTemplate [pdx| %_ = yes |] = msgToPP $ MsgLockDivision True
countryLockAllDivisionTemplate stmt = preStatement stmt

-- | Which template a division is built to. Script writes the name either bare
-- or in a field of a block.
changeDivisionTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
changeDivisionTemplate stmt@[pdx| %_ = @scr |] =
    case fst (extractStmt (matchLhsText "division_template") scr) of
        Just [pdx| %_ = ?tmpl |] -> msgToPP $ MsgChangeDivisionTemplate tmpl
        _ -> preStatement stmt
changeDivisionTemplate [pdx| %_ = ?tmpl |] = msgToPP $ MsgChangeDivisionTemplate tmpl
changeDivisionTemplate stmt = preStatement stmt

------------------------------
-- Handler for add_resource --
------------------------------
data AddResource = AddResource
        {   ar_type :: Text
        ,   ar_amount :: Maybe Double
        ,   ar_amountvar :: Maybe Text
        ,   ar_state :: Maybe Double
        }

newAR :: AddResource
newAR = AddResource undefined Nothing Nothing Nothing
addResource  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addResource stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_ar =<< foldM addLine newAR scr
    where
        addLine :: AddResource -> GenericStatement -> PPT g m AddResource
        addLine ar [pdx| type = $txt |] = return ar { ar_type = txt }
        addLine ar [pdx| amount = !num |] = return ar { ar_amount = Just num }
        addLine ar [pdx| amount = $txt |] = return ar { ar_amountvar = Just txt }
        addLine ar [pdx| state = !num |] = return ar { ar_state = Just num }
        addLine ar [pdx| show_state_in_tooltip = %_ |] = return ar
        addLine ar [pdx| $other = %_ |] = trace ("unknown section in add_resource: " ++ show other) $ return ar
        addLine ar stmt = trace ("unknown form in add_resource: " ++ show stmt) $ return ar
        pp_ar ar = do
            let buildicon = iconText $ ar_type ar
            stateloc <- maybe (return "") (getStateLoc . round) $ ar_state ar
            buildloc <- getGameL10n $ "PRODUCTION_MATERIALS_" <> T.toUpper (ar_type ar)
            case (ar_amount ar, ar_amountvar ar) of
                    (Just amount, _) -> return $ MsgAddResource buildicon buildloc amount stateloc
                    (_, Just amountvar) -> return $ MsgAddResourceVar buildicon buildloc amountvar stateloc
                    _ -> return $ preMessage stmt
addResource stmt = preStatement stmt

-------------------------------------------
-- Handler for modify_building_resources --
-------------------------------------------
foldCompound "modifyBuildingResources" "ModifyBuildingResources" "mbr"
    []
    [CompField "building" [t|Text|] Nothing True
    ,CompField "resource" [t|Text|] Nothing True
    ,CompField "amount" [t|Double|] Nothing True
    ]
    [|  do
        let buildicon = iconText _building
            resourceicon = iconText _resource
        return $ MsgModifyBuildingResources buildicon resourceicon _amount
    |]

----------
-- date --
----------


handleDate :: (Monad m, HOI4Info g) =>
    Text -> Text -> StatementHandler g m
handleDate after before  stmt@[pdx| %_ = %date |] = case dateParts date of
    Just (year, month, day) -> do
        monthloc <- isMonth month
        msgToPP $ MsgDate after monthloc (fromIntegral day) (fromIntegral year)
    _ -> preStatement stmt
handleDate after before stmt@[pdx| %_ > %date |] = case dateParts date of
    Just (year, month, day) ->  do
        monthloc <- isMonth month
        msgToPP $ MsgDate after monthloc (fromIntegral day) (fromIntegral year)
    _ -> preStatement stmt
handleDate after before stmt@[pdx| %_ < %date |] = case dateParts date of
    Just (year, month, day) ->  do
        monthloc <- isMonth month
        msgToPP $ MsgDate before monthloc (fromIntegral day) (fromIntegral year)
    _ -> preStatement stmt
handleDate _ _ stmt = preStatement stmt

-- | The year, month and day a right-hand side holds. Script writes a date
-- either bare, where the parser reads it as a date of its own, or in quotes,
-- where it comes through as the text of one.
dateParts :: GenericRhs -> Maybe (Int, Int, Int)
dateParts (DateRhs Date {year = year, month = month, day = day}) = Just (year, month, day)
dateParts rhs = do
    text <- textRhs rhs
    case map (T.unpack . T.strip) (T.splitOn "." text) of
        [y, m, d] | all (all isDigit) [y, m, d], not (any null [y, m, d]) ->
            Just (read y, read m, read d)
        _ -> Nothing


isMonth :: (HOI4Info g, Monad m) =>
    Int -> PPT g m Text
isMonth month
    = getGameL10n $ case month of
            1 -> "January"
            2 -> "February"
            3 -> "March"
            4 -> "April"
            5 -> "May"
            6 -> "June"
            7 -> "July"
            8 -> "August"
            9 -> "September"
            10 -> "October"
            11 -> "November"
            12 -> "December"
            0 -> "" -- no programmer counting, is used when only year is used to check
            14 -> "14th month for some reason" -- for some reason there is a month 14, but not idea why and what for.
            _ -> error ("impossible: tried to localize bad month number" ++ show month)

setTechnology :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setTechnology stmt@[pdx| %_ = @scr |] =
        let (_, rest) = extractStmt (matchLhsText "popup") scr in
        mapM (\case
            stmt2@[pdx| $tech = !addrm |] -> do
                mtechloc <- getGameL10nIfPresent tech
                case mtechloc of
                    Just techloc -> msgToPP' $ MsgSetTechnology addrm techloc
                    _ -> msgToPP' $ MsgSetTechnology addrm (typewriterText tech)
            unknowntechformat -> msgToPP' $ preMessage unknowntechformat)
            rest
setTechnology stmt = preStatement stmt

setCapital :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
setCapital msg stmt@[pdx| %_ = @scr |] =
        let (_, rest) = extractStmt (matchLhsText "remember_old_capital") scr in
        case rest of
            [[pdx| state = !state |]] -> do
                stateloc <- getStateLoc state
                msgToPP $ msg stateloc ""
            [[pdx| state = $state |]] -> do
                stateloc <- eGetStateText (Left state)
                msgToPP $ msg stateloc ""
            [[pdx| state = $vartag:$var |]] -> do
                stateloc <- eGetStateText (Right (vartag, var))
                msgToPP $ msg stateloc ""
            _ -> preStatement stmt
setCapital msg stmt = withFlag msg stmt


setPopularities :: (HOI4Info g, Monad m) => StatementHandler g m
setPopularities [pdx| %_ = @scr |] = do
    basemsg <- msgToPP MsgSetPopularities
    popmsg <- fold <$> indentUp (traverse getpops scr)
    return $ basemsg ++ popmsg
    where
        getpops stmt@[pdx| $ideo = !num |] = do
            ideoloc <- getGameL10n ideo
            msgToPP $ MsgSetPopularity (beliefIcon ideo) ideoloc num
        getpops stmt@[pdx| $ideo = ?txt |] = do
            ideoloc <- getGameL10n ideo
            msgToPP $ MsgSetPopularityVar (beliefIcon ideo) ideoloc txt
        getpops stmt = preStatement stmt
setPopularities stmt = preStatement stmt

------------------------------
-- Handler for add_equipment_to_stockpile --
------------------------------
data AddEquipment = AddEquipment
        {   ae_type :: Text
        ,   ae_amount :: Maybe Double
        ,   ae_amountvar :: Maybe Text
        ,   ae_producer :: Maybe (Either Text (Text, Text))
        ,   ae_variant :: Maybe Text
        }

newAE :: AddEquipment
newAE = AddEquipment undefined Nothing Nothing Nothing Nothing
addEquipment  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addEquipment stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppAe =<< foldM addLine newAE scr
    where
        addLine :: AddEquipment -> GenericStatement -> PPT g m AddEquipment
        addLine ae [pdx| type = ?txt |] = return ae { ae_type = txt }
        addLine ae [pdx| amount = !num |] = return ae { ae_amount = Just num }
        addLine ae [pdx| amount = ?txt |] = return ae { ae_amountvar = Just txt }
        addLine ae [pdx| producer = ?tag |] = return ae { ae_producer = Just (Left tag) }
        addLine ae [pdx| producer = $vartag:$var |] = return ae { ae_producer = Just (Right (vartag, var)) }
        addLine ae [pdx| variant_name = ?txt |] = return ae { ae_variant = Just txt }
        addLine ae [pdx| $other = %_ |] = trace ("unknown section in add_equipment_to_stockpile: " ++ show other) $ return ae
        addLine ae stmt = trace ("unknown form in add_equipment_to_stockpile: " ++ show stmt) $ return ae
        ppAe ae = do
            let variant = fromMaybe "" $ ae_variant ae
            flaglocm <- case ae_producer ae of
                Just producer -> eflag (Just HOI4Country) producer
                _ -> return Nothing
            let flagloc = fromMaybe "" flaglocm
            equiploc <- getGameL10n $ ae_type ae
            case (ae_amount ae, ae_amountvar ae) of
                    (Just amount, _) -> return $ MsgAddEquipmentToStockpile amount flagloc equiploc variant
                    (_, Just amountvar) -> return $ MsgAddEquipmentToStockpileVar amountvar flagloc equiploc variant
                    _ -> return $ preMessage stmt
addEquipment stmt = preStatement stmt

--------------------------------------
-- Handler for give_resource_rights --
--------------------------------------
data GiveRights = GiveRights
        {   gr_receiver :: Text
        ,   gr_state :: Maybe Int
        ,   gr_statevar :: Maybe Text
        ,   gr_resource :: Maybe [Text]
        }

newGR :: GiveRights
newGR = GiveRights undefined Nothing Nothing Nothing
giveResourceRights :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
giveResourceRights stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppGR =<< foldM addLine newGR scr
    where
        addLine :: GiveRights -> GenericStatement -> PPT g m GiveRights
        addLine gr [pdx| receiver = ?txt |] = return gr { gr_receiver = txt }
        addLine gr [pdx| state = !num |] = return gr { gr_state = Just num }
        addLine gr [pdx| state = ?txt |] = return gr { gr_statevar = Just txt }
        addLine gr [pdx| resources = @scr |] =
            let ress = mapMaybe getbareRess scr in
            return gr { gr_resource = Just ress }
        addLine gr [pdx| $other = %_ |] = trace ("unknown section in give_resource_rights: " ++ show other) $ return gr
        addLine gr stmt = trace ("unknown form in give_resource_rights: " ++ show stmt) $ return gr
        ppGR :: GiveRights -> PPT g m ScriptMessage
        ppGR gr = do
            flag_loc <- flagText (Just HOI4Country) (gr_receiver gr)
            resloc <- case gr_resource gr of
                Just resl -> do
                    reslloc <- traverse getGameL10n resl
                    return $ T.intercalate ", " resl <> " "
                _ -> return mempty
            case (gr_state gr, gr_statevar gr) of
                (Just state, _) -> do
                    state_loc <- getStateLoc state
                    return $ MsgGiveResourceRights flag_loc state_loc resloc
                (_, Just state) -> do
                    state_loc <- eGetStateText (Left state)
                    return $ MsgGiveResourceRights flag_loc state_loc resloc
                _ -> return $ preMessage stmt

        getbareRess (StatementBare (GenericLhs e [])) = Just e
        getbareRess stmt = trace ("Unknown in give_resource_rights array statement: " ++ show stmt) Nothing
giveResourceRights stmt = preStatement stmt

-- | Whether anyone holds the rights to a state's resources. The receiver and
-- the state may each be left out where the scope already names one of them.
hasResourcesRights :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasResourcesRights stmt@[pdx| %_ = @scr |] = do
    let (mreceiver, s1) = extractStmt (matchLhsText "receiver") scr
        (mstate, s2) = extractStmt (matchLhsText "state") s1
        (mresources, _) = extractStmt (matchLhsText "resources") s2
    whoflag <- case mreceiver of
        Just [pdx| %_ = ?who |] -> flagText (Just HOI4Country) who
        _ -> return ""
    -- With no state named the trigger asks about the state in scope.
    whereloc <- case mstate of
        Just st -> statedFrom (Just st)
        _ -> return "the state"
    resloc <- case mresources of
        Just [pdx| %_ = @ress |] -> do
            locs <- traverse getGameL10n (mapMaybe bareAtom ress)
            return $ if null locs then "" else T.intercalate ", " locs <> " "
        _ -> return ""
    msgToPP $ MsgHasResourcesRights whoflag whereloc resloc
hasResourcesRights stmt = preStatement stmt

-- | The name written bare inside an array, as script writes a list of resources
-- or of traits.
bareAtom :: GenericStatement -> Maybe Text
bareAtom (StatementBare (GenericLhs atom [])) = Just atom
bareAtom stmt = trace ("Unknown in bare array statement: " ++ show stmt) Nothing

-------------------------------
-- handler for damage_units  --
-------------------------------

-- | Hurts the units somewhere without a word from the game about it. The
-- @limit@ is a condition on whoever owns them, so it keeps a block of its own
-- under the line saying what is damaged.
damageUnits :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
damageUnits [pdx| %_ = @scr |] = do
    let isField st = any (`matchLhsText` st)
            ["province", "state", "region", "damage", "org_damage", "str_damage"
            ,"ratio", "template", "army", "navy"]
        (fields, rest) = partition isField scr
        field k = listToMaybe [st | st <- fields, matchLhsText k st]
        saidYes k = not (null [() | [pdx| $lbl = yes |] <- fields, sameKey lbl k])
        amount k = case field k of
            Just [pdx| %_ = !n |] -> Just (n :: Double)
            _ -> Nothing
        -- A ratio is a share of what the unit has, anything else a flat amount.
        ratio = saidYes "ratio"
        showAmt n = boldText (Doc.doc2text (if ratio then reducedNum plainPc n else plainNum n))
        parts = [ showAmt n <> " of their " <> lbl
                | (k, lbl) <- [ ("damage", "organisation and strength")
                              , ("org_damage", "organisation")
                              , ("str_damage", "strength") ]
                , Just n <- [amount k] ]
        what | saidYes "army" && not (saidYes "navy") = "armies"
             | saidYes "navy" && not (saidYes "army") = "navies"
             | otherwise = "units"
    whereloc <- case (field "province", field "state", field "region") of
        (Just [pdx| %_ = !prov |], _, _) -> return $ "province " <> T.pack (show (prov :: Int))
        (_, Just st, _) -> statedFrom (Just st)
        (_, _, Just [pdx| %_ = !reg |]) -> getRegionLoc reg
        _ -> return ""
    header <- msgToPP $ MsgDamageUnits what whereloc
        (if null parts then "an unstated amount" else T.intercalate " and " parts)
    script_pp'd <- ppMany rest
    return (header ++ script_pp'd)
damageUnits stmt = preStatement stmt

-----------------------------------------
-- handler for num_divisions_in_states --
-----------------------------------------

numDivisionsInStates :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
numDivisionsInStates stmt@[pdx| %_ = @scr |] = do
    let (mcount, s1) = extractStmt (matchLhsText "count") scr
        (mstates, _) = extractStmt (matchLhsText "states") s1
    stateslocs <- case mstates of
        Just [pdx| %_ = @sts |] -> traverse getStateLoc (mapMaybe bareState sts)
        _ -> return []
    let whereloc = if null stateslocs then "the states named"
                   else T.intercalate ", " stateslocs
    case mcount of
        Just [pdx| %_ > !num |] -> msgToPP $ MsgNumDivisionsInStates "more than" num whereloc
        Just [pdx| %_ < !num |] -> msgToPP $ MsgNumDivisionsInStates "fewer than" num whereloc
        Just [pdx| %_ = !num |] -> msgToPP $ MsgNumDivisionsInStates "exactly" num whereloc
        _ -> preStatement stmt
    where
        bareState (StatementBare (IntLhs n)) = Just n
        bareState st = trace ("Unknown in num_divisions_in_states array: " ++ show st) Nothing
numDivisionsInStates stmt = preStatement stmt

------------------------------------
-- handler for set_variable_to_random --
------------------------------------

-- | Puts a random number in a variable. Written bare it is a number from 0 up
-- to 1; the block form gives the range and says whether it is a whole number.
setVariableToRandom :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setVariableToRandom [pdx| %_ = @scr |] = do
    let (mvar, s1) = extractStmt (matchLhsText "var") scr
        (mmin, s2) = extractStmt (matchLhsText "min") s1
        (mmax, s3) = extractStmt (matchLhsText "max") s2
        (minteger, _) = extractStmt (matchLhsText "integer") s3
        bound def mstmt = case mstmt of
            Just [pdx| %_ = !num |] -> Doc.doc2text (plainNumMin (num :: Double))
            Just [pdx| %_ = $vartag:$var |] -> vartag <> ":" <> var
            Just [pdx| %_ = $var |] -> var
            _ -> def
        varname = case mvar of
            Just [pdx| %_ = $vartag:$var |] -> vartag <> ":" <> var
            Just [pdx| %_ = ?var |] -> var
            _ -> "<!-- Check Script -->"
        isint = case minteger of
            Just [pdx| %_ = yes |] -> True
            _ -> False
    msgToPP $ MsgSetVariableToRandom varname (bound "0" mmin) (bound "1" mmax) isint
setVariableToRandom [pdx| %_ = ?var |] = msgToPP $ MsgSetVariableToRandom var "0" "1" False
setVariableToRandom stmt = preStatement stmt

-----------------------------------
-- handler for add_random_trait  --
-----------------------------------

addRandomTrait :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addRandomTrait stmt@[pdx| %_ = @scr |] = do
    locs <- traverse getGameL10n (mapMaybe bareAtom scr)
    if null locs
        then preStatement stmt
        else msgToPP $ MsgAddRandomTrait (T.intercalate ", " (map italicText locs))
addRandomTrait stmt = preStatement stmt

--------------------------------------------
-- handler for force_enable_resistance    --
--------------------------------------------

-- | Turns resistance on in a state whatever the game would otherwise make of
-- it. Whoever the occupier has to be for that is the only part of it a reader
-- needs; @clear@ only says whether what was set before is thrown away first.
forceEnableResistance :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
forceEnableResistance msg [pdx| %_ = @scr |] = do
    let (moccupier, _) = extractStmt (matchLhsText "occupier") scr
    whoflag <- case moccupier of
        Just [pdx| %_ = ?who |] -> flagText (Just HOI4Country) who
        _ -> return ""
    msgToPP $ msg whoflag ""
forceEnableResistance msg [pdx| %_ = ?who |]
    | T.toLower who `notElem` ["yes", "no"] = do
        whoflag <- flagText (Just HOI4Country) who
        msgToPP $ msg whoflag who
forceEnableResistance _ stmt = preStatement stmt

-------------------------
-- handler for THIS    --
-------------------------

-- | @THIS@ names the scope the block is already written in, so it says nothing
-- the reader is not already under: what is inside it stands where it stood,
-- without a heading and without the step in that a heading would bring.
thisScope :: (HOI4Info g, Monad m) => StatementHandler g m
thisScope [pdx| %_ = @scr |] = indentDown (ppMany scr)
thisScope stmt = preStatement stmt

-------------------------------------------
-- handler for the loops over an array   --
-------------------------------------------

-- | A loop or a test that runs over an array: @any_of@, @all_of@,
-- @for_each_loop@. Which temporary variables the loop writes its value and its
-- index into says nothing about what it does with them, and the script under it
-- names them itself wherever it reads them.
arrayLoop :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -- ^ Message to use as the block header
        -> StatementHandler g m
arrayLoop header [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let (marray, s1) = extractStmt (matchLhsText "array") scr
        (_, s2) = extractStmt (matchLhsText "value") s1
        (_, rest) = extractStmt (matchLhsText "index") s2
        arr = case marray of
            Just [pdx| %_ = ?a |] -> a
            _ -> "<!-- Check Script -->"
    script_pp'd <- ppMany rest
    return ((i, header arr) : script_pp'd)
arrayLoop _ stmt = preStatement stmt

-------------------------------
-- handler for collection_size --
-------------------------------

-- | How many things a collection gathers. The collection is either named or
-- written out in full; where it is written out, saying so is as much as the
-- line can carry, since the whole of it stands in the script beside.
collectionSize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
collectionSize stmt@[pdx| %_ = @scr |] = do
    let (minput, rest) = extractStmt (matchLhsText "input") scr
        (mvalue, _) = extractStmt (matchLhsText "value") rest
    whatloc <- case minput of
        Just [pdx| %_ = ?inp |] -> do
            let collkey = fromMaybe inp (T.stripPrefix "collection:" inp)
            getGameL10n ("COLLECTION_" <> T.toUpper collkey)
        _ -> return "The collection"
    case mvalue of
        Just [pdx| %_ > !num |] -> msgToPP $ MsgCollectionSize "" whatloc "more than" num
        Just [pdx| %_ < !num |] -> msgToPP $ MsgCollectionSize "" whatloc "less than" num
        Just [pdx| %_ = !num |] -> msgToPP $ MsgCollectionSize "" whatloc "exactly" num
        Just [pdx| %_ > $var |] -> msgToPP $ MsgCollectionSizeVar "" whatloc "more than" var
        Just [pdx| %_ < $var |] -> msgToPP $ MsgCollectionSizeVar "" whatloc "less than" var
        _ -> preStatement stmt
collectionSize stmt = preStatement stmt

------------------------------------
-- handler for create_railway_gun --
------------------------------------

createRailwayGun :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createRailwayGun stmt@[pdx| %_ = @scr |] = do
    let (mequip, rest) = extractStmt (matchLhsText "equipment") scr
        (mloc, _) = extractStmt (matchLhsText "location") rest
    case mequip of
        Just [pdx| %_ = ?equip |] -> do
            equiploc <- getGameL10n equip
            -- With nowhere named the gun is created in the country's capital,
            -- which the line need not spell out.
            whereloc <- case mloc of
                Just [pdx| %_ = !prov |] ->
                    return $ "province " <> T.pack (show (prov :: Int))
                _ -> return ""
            msgToPP $ MsgCreateRailwayGun equiploc whereloc
        _ -> preStatement stmt
createRailwayGun stmt = preStatement stmt

-----------------------------------
-- handler for add_history_entry --
-----------------------------------

addHistoryEntry :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addHistoryEntry stmt@[pdx| %_ = @scr |] =
    case fst (extractStmt (matchLhsText "key") scr) of
        Just [pdx| %_ = ?key |] -> msgToPP . MsgAddHistoryEntry =<< getGameL10n key
        _ -> preStatement stmt
addHistoryEntry stmt = preStatement stmt

----------------------------------------
-- handler for set_country_leader_name --
----------------------------------------

-- | Renames a country's leader. With no party named it is whoever leads the
-- country now.
setCountryLeaderName :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setCountryLeaderName stmt@[pdx| %_ = @scr |] = do
    let (mname, rest) = extractStmt (matchLhsText "name") scr
        (mideo, _) = extractStmt (matchLhsText "ideology") rest
    ideoloc <- case mideo of
        Just [pdx| %_ = ?ideo |] -> getGameL10n ideo
        _ -> return ""
    case mname of
        Just [pdx| %_ = ?name |] -> do
            nameloc <- getGameL10n name
            msgToPP $ MsgSetCountryLeaderName nameloc ideoloc
        _ -> preStatement stmt
setCountryLeaderName stmt = preStatement stmt

------------------------------------------------
-- handler for generate_scientist_character   --
------------------------------------------------

-- | Hires a scientist the game makes up on the spot. What they are good at is
-- the whole of what the country gains; the portrait and the gender they are
-- drawn with are not.
generateScientistCharacter :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
generateScientistCharacter [pdx| %_ = @scr |] = do
    let (mtraits, rest) = extractStmt (matchLhsText "traits") scr
        (mskills, _) = extractStmt (matchLhsText "skills") rest
    traitlocs <- case mtraits of
        Just [pdx| %_ = @trts |] -> traverse getGameL10n (mapMaybe bareAtom trts)
        _ -> return []
    header <- msgToPP $ MsgGenerateScientistCharacter
        (T.intercalate ", " (map italicText traitlocs))
    skillmsgs <- case mskills of
        Just [pdx| %_ = @skls |] -> indentUp (concat <$> traverse skillMsg skls)
        _ -> return []
    return $ header ++ skillmsgs
    where
        skillMsg :: GenericStatement -> PPT g m IndentedMessages
        skillMsg [pdx| $spec = !lvl |] = do
            specloc <- getGameL10n spec
            msgToPP $ MsgScientistSkill specloc lvl
        skillMsg stmt = preStatement stmt
generateScientistCharacter stmt = preStatement stmt

-----------------------------------------------
-- handler for set_state_province_controller  --
-----------------------------------------------

-- | Hands the provinces of a state to someone. The @limit@ is a condition on
-- whoever holds each province now, so it keeps a block of its own under the
-- line saying who takes them.
setStateProvinceController :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setStateProvinceController stmt@[pdx| %_ = @scr |] = do
    let (mcontroller, rest) = extractStmt (matchLhsText "controller") scr
    case mcontroller of
        Just [pdx| %_ = ?who |] -> do
            whoflag <- flagText (Just HOI4Country) who
            header <- msgToPP $ MsgSetStateProvinceController whoflag
            script_pp'd <- ppMany rest
            return $ header ++ script_pp'd
        _ -> preStatement stmt
setStateProvinceController stmt = preStatement stmt

--------------------------------------------------
-- handler for num_planes_stationed_in_regions   --
--------------------------------------------------

numPlanesStationedInRegions :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
numPlanesStationedInRegions stmt@[pdx| %_ = @scr |] = do
    let (mvalue, rest) = extractStmt (matchLhsText "value") scr
        (mregions, _) = extractStmt (matchLhsText "regions") rest
    regionlocs <- case mregions of
        Just [pdx| %_ = @regs |] -> traverse getRegionLoc (mapMaybe bareInt regs)
        _ -> return []
    let whereloc = if null regionlocs then "the regions named"
                   else T.intercalate ", " regionlocs
    case mvalue of
        Just [pdx| %_ > !num |] -> msgToPP $ MsgNumPlanesStationedInRegions "more than" num whereloc
        Just [pdx| %_ < !num |] -> msgToPP $ MsgNumPlanesStationedInRegions "fewer than" num whereloc
        Just [pdx| %_ = !num |] -> msgToPP $ MsgNumPlanesStationedInRegions "exactly" num whereloc
        _ -> preStatement stmt
numPlanesStationedInRegions stmt = preStatement stmt

------------------------------------------------------
-- handler for the scopes over a list of targets     --
------------------------------------------------------

-- | A trigger asked of states or countries listed out by name, rather than of
-- whatever a scope of its own catches: @any_state_of@, @all_country_of@ and
-- their like. The list may also be a script constant, which is written out as
-- the constant's name since what it holds is not in the script at hand.
listedScope :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -- ^ Message to use as the block header
        -> (Text -> PPT g m Text) -- ^ How to name one of the things listed
        -> StatementHandler g m
listedScope header nameOf [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let (mtarget, rest) = extractStmt (matchLhsText "target") scr
    names <- case mtarget of
        Just [pdx| %_ = @tgts |] -> traverse nameOf (mapMaybe bareTarget tgts)
        Just [pdx| %_ = ?one |] -> (:[]) <$> nameOf one
        _ -> return []
    let listed = if null names then "the ones named" else T.intercalate ", " names
    script_pp'd <- ppMany rest
    return ((i, header listed) : script_pp'd)
    where
        bareTarget (StatementBare (IntLhs n)) = Just (T.pack (show n))
        bareTarget (StatementBare (GenericLhs t [])) = Just t
        bareTarget stmt = trace ("Unknown in target array: " ++ show stmt) Nothing
listedScope _ _ stmt = preStatement stmt

-- | The name of a state written into such a list, whether by its id or through
-- a variable holding it.
listedState :: (HOI4Info g, Monad m) => Text -> PPT g m Text
listedState t
    | not (T.null t), T.all isDigit t = getStateLoc (read (T.unpack t))
    | otherwise = eGetStateText (Left t)

----------------------------
-- handler for add_mines  --
----------------------------

-- | Mines laid in a strategic region, which is named by its id.
addMines :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addMines stmt@[pdx| %_ = @scr |] = do
    let (mregion, rest) = extractStmt (matchLhsText "region") scr
        (mamount, _) = extractStmt (matchLhsText "amount") rest
    whereloc <- case mregion of
        Just [pdx| %_ = !num |] -> getRegionLoc num
        Just [pdx| %_ = ?var |] -> return (typewriterText var)
        _ -> return "<!-- Check Script -->"
    case mamount of
        Just [pdx| %_ = !num |] -> msgToPP $ MsgAddMines "" whereloc num
        Just [pdx| %_ = ?var |] -> msgToPP $ MsgAddMinesVar "" whereloc (typewriterText var)
        _ -> preStatement stmt
addMines stmt = preStatement stmt

-------------------------------------------------------
-- handler for set_division_force_allow_recruiting    --
-------------------------------------------------------

setDivisionForceAllowRecruiting :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setDivisionForceAllowRecruiting stmt@[pdx| %_ = @scr |] = do
    let (mtemplate, rest) = extractStmt (matchLhsText "division_template") scr
        allowed = null [() | [pdx| force_allow_recruiting = no |] <- rest]
    case mtemplate of
        Just [pdx| %_ = ?tmpl |] -> msgToPP $ MsgSetDivisionForceAllowRecruiting tmpl allowed
        _ -> preStatement stmt
setDivisionForceAllowRecruiting stmt = preStatement stmt

-- | The number written bare inside an array, as script writes a list of states
-- or of strategic regions.
bareInt :: GenericStatement -> Maybe Int
bareInt (StatementBare (IntLhs n)) = Just n
bareInt stmt = trace ("Unknown in bare number array: " ++ show stmt) Nothing

--------------------------------------
-- Handler for add_ace --
--------------------------------------

data AddAce = AddAce
        {   aa_name :: Text
        ,   aa_surname :: Text
        ,   aa_callsign :: Text
        }

newAA :: AddAce
newAA = AddAce undefined undefined undefined
addAce  :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addAce stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppAa =<< foldM addLine newAA scr
    where
        addLine :: AddAce -> GenericStatement -> PPT g m AddAce
        addLine aa [pdx| name = ?txt |] = return aa { aa_name = txt }
        addLine aa [pdx| surname = ?txt |] = return aa { aa_surname = txt }
        addLine aa [pdx| callsign = ?txt |] = return aa { aa_callsign = txt }
        addLine aa [pdx| type = ?_ |] = return aa
        addLine aa [pdx| is_female = ?_ |] = return aa
        addLine aa [pdx| $other = %_ |] = trace ("unknown section in add_ace: " ++ show other) $ return aa
        addLine aa stmt = trace ("unknown form in add_ace: " ++ show stmt) $ return aa
        ppAa aa = do
            return $ MsgAddAce (aa_name aa) (aa_callsign aa) (aa_surname aa)
addAce stmt = preStatement stmt

-------------------------------
-- Handler for has_navy_size --
------------------------------

data NavySize = NavySize
        {   ns_size :: Maybe Double
        ,   ns_sizevar :: Maybe Text
        ,   ns_comp :: Text
        ,   ns_type :: Maybe Text
        ,   ns_archetype :: Maybe Text
        }

newNS :: NavySize
newNS = NavySize Nothing Nothing undefined Nothing Nothing

hasNavySize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasNavySize stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppNS =<< foldM addLine newNS scr
    where
        addLine :: NavySize -> GenericStatement -> PPT g m NavySize
        addLine ns [pdx| size < !num |] = return ns { ns_comp = "less than", ns_size = Just num}
        addLine ns [pdx| size > !num |] = return ns { ns_comp = "more than", ns_size = Just num }
        addLine ns [pdx| size < $num |] = return ns { ns_comp = "less than", ns_sizevar = Just num}
        addLine ns [pdx| size > $num |] = return ns { ns_comp = "more than", ns_sizevar = Just num }
        addLine ns [pdx| type = $txt |] = return ns { ns_type = Just txt }
        addLine ns [pdx| unit = $txt |] = return ns { ns_type = Just txt }
        addLine ns [pdx| archetype = ?txt |] = return ns { ns_archetype = Just txt }
        addLine ns [pdx| $other = %_ |] = trace ("unknown section in has_navy_size: " ++ show other) $ return ns
        addLine ns stmt = trace ("unknown form in has_navy_size: " ++ show stmt) $ return ns
        ppNS ns = do
            typed <- case(ns_type ns, ns_archetype ns) of
                (Just txt, _) -> getGameL10n txt
                (_, Just txt) -> getGameL10n txt
                _ -> return ""
            case (ns_size ns, ns_sizevar ns) of
                (Just amt, _) -> return $ MsgHasNavySize (ns_comp ns) amt typed
                (_, Just amt) -> return $ MsgHasNavySizeVar (ns_comp ns) amt typed
                _ -> return $ preMessage stmt
hasNavySize stmt = preStatement stmt

----------------------------------------------
-- Handler for has_deployed_air_force_size --
----------------------------------------------

data AirForceSize = AirForceSize
        {   afs_size :: Maybe Double
        ,   afs_sizevar :: Maybe Text
        ,   afs_comp :: Text
        ,   afs_type :: Maybe Text
        }

newAFS :: AirForceSize
newAFS = AirForceSize Nothing Nothing "least" Nothing

-- | Handler for @has_deployed_air_force_size@, which asks after the aircraft a
-- country has in the air. The game reads the comparison as a bound rather than
-- a strict one -- at least so many, at most so many -- and names the kind of
-- aircraft where the trigger is given one.
hasDeployedAirForceSize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasDeployedAirForceSize stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppAFS =<< foldM addLine newAFS scr
    where
        addLine :: AirForceSize -> GenericStatement -> PPT g m AirForceSize
        addLine afs [pdx| size < !num |] = return afs { afs_comp = "most", afs_size = Just num }
        addLine afs [pdx| size > !num |] = return afs { afs_comp = "least", afs_size = Just num }
        addLine afs [pdx| size = !num |] = return afs { afs_comp = "least", afs_size = Just num }
        addLine afs [pdx| size < $num |] = return afs { afs_comp = "most", afs_sizevar = Just num }
        addLine afs [pdx| size > $num |] = return afs { afs_comp = "least", afs_sizevar = Just num }
        addLine afs [pdx| size = $num |] = return afs { afs_comp = "least", afs_sizevar = Just num }
        addLine afs [pdx| type = ?txt |] = return afs { afs_type = Just txt }
        addLine afs [pdx| $other = %_ |] = trace ("unknown section in has_deployed_air_force_size: " ++ show other) $ return afs
        addLine afs stmt = trace ("unknown form in has_deployed_air_force_size: " ++ show stmt) $ return afs
        ppAFS afs = do
            typed <- maybe (return "") getGameL10n (afs_type afs)
            case (afs_size afs, afs_sizevar afs) of
                (Just amt, _) -> return $ MsgHasDeployedAirForceSize (afs_comp afs) amt typed
                (_, Just amt) -> return $ MsgHasDeployedAirForceSizeVar (afs_comp afs) amt typed
                _ -> return $ preMessage stmt
hasDeployedAirForceSize stmt = preStatement stmt

-----------------------------------
-- Handler for division_template --
-----------------------------------

divisionTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
divisionTemplate stmt@[pdx| %_ = @scr |] = do
    let (mname, _) = extractStmt (matchLhsText "name") scr
    case mname of
        Just [pdx| %_ = ?txt |] -> msgToPP $ MsgDivisionTemplate txt
        _ -> preStatement stmt
divisionTemplate  stmt = preStatement stmt

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

-- | Read text that belongs to something of the current scope's -- an idea it
-- gains, a decision of its own that is named -- with the pronouns meaning
-- what they do as the game draws that text for that scope: it is its own
-- ROOT there, and the FROM of the script in hand has no say.
thisContext :: (HOI4Info g, Monad m) => PPT g m a -> PPT g m a
thisContext action = do
    holder <- getThisIdent
    withRootIdent holder $ withFromIdent Nothing action

-----------------------------
-- Handler for create_unit --
-----------------------------

createUnit :: (Monad m, HOI4Info g) => StatementHandler g m
createUnit stmt@[pdx| %_ = @scr |] = do
    let (division,rest) = extractStmt (matchLhsText "division") scr
        (owner, _) = extractStmt (matchLhsText "owner") rest
    divis <- case division of
        Just [pdx| %_ = ?txt |] -> do
            let divtempi = fromMaybe 0 $ findString "division_template" txt
                divtemps = T.drop 1 $ T.dropWhile (/= '\"') (T.drop divtempi txt)
            return $ T.dropEnd 1 (T.dropWhileEnd (/= '\"') divtemps)
        _ -> return "<!-- Check game Script -->"
    owner <- case owner of
        Just [pdx| %_ = ?txt |] -> flagText (Just HOI4Country) txt
        _ -> return "<!-- Check game Script -->"
    msgToPP $ MsgCreateUnit owner divis
    where
    findString :: Text -> Text -> Maybe Int
    findString search str = findIndex (T.isPrefixOf search) (T.tails str)
createUnit stmt = preStatement stmt

---------------------------------
-- Handler for damage_building --
---------------------------------

data DamageBuilding = DamageBuilding
        {   db_type :: Text
        ,   db_damage :: Maybe Double
        ,   db_damagevar :: Maybe Text
        ,   db_province :: Maybe Double
        }

newDB :: DamageBuilding
newDB = DamageBuilding "" Nothing Nothing Nothing

damageBuilding :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
damageBuilding stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppDB =<< foldM addLine newDB scr
    where
        addLine :: DamageBuilding -> GenericStatement -> PPT g m DamageBuilding
        addLine db [pdx| type = ?txt |] = return db { db_type = txt }
        addLine db [pdx| tags = ?txt |] = return db { db_type = txt }
        addLine db [pdx| tags = @_ |] = return db { db_type = "<!-- Multiple tags check script -->" }
        addLine db [pdx| damage = !num |] = return db { db_damage = Just num }
        addLine db [pdx| damage = $txt |] = return db { db_damagevar = Just txt }
        addLine db [pdx| province = !num |] = return db {db_province = Just num}
        addLine db [pdx| $other = %_ |] = trace ("unknown section in damage_building: " ++ show other) $ return db
        addLine db stmt = trace ("unknown form in damage_building: " ++ show stmt) $ return db
        ppDB db = do
            let typeicon = iconText (db_type db)
                prov = fromMaybe (-1) (db_province db)
            typeloc <- getGameL10n (db_type db)
            case (db_damage db, db_damagevar db) of
                (Just amt, _) -> return $ MsgDamageBuilding typeicon typeloc amt prov
                (_, Just amt) -> return $ MsgDamageBuildingVar typeicon typeloc amt prov
                _ -> return $ preMessage stmt
damageBuilding stmt = preStatement stmt

------ region handler -----

withRegion :: (HOI4Info g, Monad m) =>
        StatementHandler g m
withRegion stmt@[pdx| %lhs = $vartag:$var |] = do
    mtagloc <- tagged vartag var
    case mtagloc of
        Just tagloc -> msgToPP $ MsgRegion tagloc
        Nothing -> preStatement stmt
withRegion stmt@[pdx| %lhs = $var |]
    = msgToPP $ MsgRegion (typewriterText var)
withRegion [pdx| %lhs = !stateid |]
    = msgToPP . MsgRegion =<< getRegionLoc stateid
withRegion stmt = preStatement stmt



------------------------------------
-- handler for divisions_in_state --
------------------------------------

data DivisionsInState = DivisionsInState
        {   ds_size :: Double
        ,   ds_comp :: Text
        ,   ds_type :: Maybe Text
        ,   ds_state :: Maybe Int
        ,   ds_statevar :: Maybe (Either Text (Text, Text))
        ,   ds_border_state :: Maybe Int
        ,   ds_border_statevar :: Maybe (Either Text (Text, Text))
        }

newDS :: DivisionsInState
newDS = DivisionsInState undefined undefined Nothing Nothing Nothing Nothing Nothing

divisionsInState :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Double -> Text -> Text -> Text -> ScriptMessage) -> StatementHandler g m
divisionsInState msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppDS =<< foldM addLine newDS scr
    where
        addLine :: DivisionsInState -> GenericStatement -> PPT g m DivisionsInState
        addLine ds [pdx| size > !num |] = return ds { ds_comp = "more than", ds_size = num }
        addLine ds [pdx| size < !num |] = return ds { ds_comp = "less than", ds_size = num }
        addLine ds [pdx| amount > !num |] = return ds { ds_comp = "more than", ds_size = num }
        addLine ds [pdx| amount < !num |] = return ds { ds_comp = "less than", ds_size = num }
        addLine ds [pdx| type = $txt |] = return ds { ds_type = Just txt }
        addLine ds [pdx| state = !num |] = return ds {ds_state = Just num}
        addLine ds [pdx| state = $vartag:$var |] = return ds { ds_statevar = Just (Right (vartag,var))}
        addLine ds [pdx| state = $var |] = return ds { ds_statevar = Just (Left var)}
        addLine ds [pdx| border_state = !num |] = return ds { ds_border_state = Just num}
        addLine ds [pdx| border_state = $vartag:$var |] = return ds { ds_border_statevar = Just (Right (vartag,var))}
        addLine ds [pdx| border_state = $var |] = return ds { ds_border_statevar = Just (Left var)}
        addLine ds [pdx| $other = %_ |] = trace ("unknown section in divisionsInState: " ++ show other) $ return ds
        addLine ds stmt = trace ("unknown form in divisionsInState: " ++ show stmt) $ return ds
        ppDS :: DivisionsInState -> PPT g m ScriptMessage
        ppDS ds = do
            borderstate <- case (ds_border_state ds, ds_border_statevar ds) of
                (Just state,_) -> getStateLoc state
                (_,Just estate) -> eGetStateText estate
                _-> return "<!-- Check Script -->"
            typeloc <- maybe (return "") getGameL10n (ds_type ds)
            stateloc <- case (ds_state ds, ds_statevar ds) of
                (Just state,_) -> getStateLoc state
                (_,Just (Left state)) -> do
                    mstate <- eGetState (Left state)
                    return $ fromMaybe "" mstate
                (_,Just (Right state)) -> do
                    mstate <- eGetState (Right state)
                    return $ fromMaybe "" mstate
                _-> return ""
            return $ msg (ds_comp ds) (ds_size ds) typeloc stateloc borderstate
divisionsInState _ stmt = preStatement stmt

-------------------------------------------------------
-- handler for delete_units and delete_unit_template --
-------------------------------------------------------

data DeleteUnits = DeleteUnits
        {   du_division_template :: Maybe Text
        ,   du_disband :: Bool
        ,   du_state :: Maybe Text
        }

newDU :: DeleteUnits
newDU = DeleteUnits Nothing False Nothing

deleteUnits :: forall g m. (HOI4Info g, Monad m) =>
    (Bool -> Text -> Text -> ScriptMessage) -> StatementHandler g m
deleteUnits msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppDU =<< foldM addLine newDU scr
    where
        addLine :: DeleteUnits -> GenericStatement -> PPT g m DeleteUnits
        addLine du [pdx| division_template = ?txt |] = return du { du_division_template = Just txt }
        addLine du [pdx| disband = %rhs |]
            | GenericRhs "yes" [] <- rhs = return du { du_disband = True }
            | otherwise = return du
        addLine du [pdx| state = !num |] = do
            stateloc <- getStateLoc num
            return du { du_division_template = Just stateloc }
        addLine du [pdx| $other = %_ |] = trace ("unknown section in deleteUnits: " ++ show other) $ return du
        addLine du stmt = trace ("unknown form in deleteUnits: " ++ show stmt) $ return du
        ppDU :: DeleteUnits -> PPT g m ScriptMessage
        ppDU du = do
            return $ msg (du_disband du) (fromMaybe "" (du_division_template du)) (fromMaybe "" (du_state du))
deleteUnits _ stmt = preStatement stmt

----------------------------------
-- handler for start_border_war --
----------------------------------

data StartBorderWar = StartBorderWar
        {   sbw_change_state_after_war :: Bool
        ,   sbw_state_attacker :: Text
        ,   sbw_state_defender :: Text
        ,   sbw_on_win_attacker :: Text
        ,   sbw_on_win_defender :: Text
        ,   sbw_on_loss_attacker :: Text
        ,   sbw_on_loss_defender :: Text
        ,   sbw_on_cancel_attacker :: Text
        ,   sbw_on_cancel_defender :: Text
        }

newSBW :: StartBorderWar
newSBW = StartBorderWar False "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->"

startBorderWar :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
startBorderWar stmt@[pdx| %_ = @scr |]
    = ppSBW =<< foldM addLine newSBW scr
    where
        addLine :: StartBorderWar -> GenericStatement -> PPT g m StartBorderWar
        addLine sbw [pdx| change_state_after_war = %rhs |]
            | GenericRhs "yes" [] <- rhs = return sbw { sbw_change_state_after_war = True }
            | otherwise = return sbw
        addLine sbw [pdx| attacker = @scr |] = foldM (addLine' True) sbw scr
        addLine sbw [pdx| defender = @scr |] = foldM (addLine' False) sbw scr
        addLine sbw [pdx| combat_width = %_ |] = return sbw
        addLine sbw [pdx| minimum_duration_in_days = %_ |] = return sbw
        addLine sbw [pdx| dig_in_factor = %_ |] = return sbw
        addLine sbw [pdx| terrain_factor = %_ |] = return sbw
        addLine sbw [pdx| $other = %_ |] = trace ("unknown section in startBorderWar: " ++ show other) $ return sbw
        addLine sbw stmt = trace ("unknown form in startBorderWar: " ++ show stmt) $ return sbw

        addLine' :: Bool -> StartBorderWar -> GenericStatement -> PPT g m StartBorderWar
        addLine' atde sbw [pdx| state = !num |] = do
            stateloc <- getStateLoc num
            if atde
            then return  sbw { sbw_state_attacker = stateloc }
            else return sbw { sbw_state_defender = stateloc }
        addLine' atde sbw [pdx| state = $txt |] = do
            if atde
            then return  sbw { sbw_state_attacker = txt }
            else return sbw { sbw_state_defender = txt }
        addLine' atde sbw [pdx| state = $vartag:$var |] = do
            if atde
            then return  sbw { sbw_state_attacker = var }
            else return sbw { sbw_state_defender = var }
        addLine' atde sbw [pdx| on_win = $eid |] = do
            if atde
            then return  sbw { sbw_on_win_attacker = eid }
            else return sbw { sbw_on_win_defender = eid }
        addLine' atde sbw [pdx| on_lose = $eid |] = do
            if atde
            then return  sbw { sbw_on_loss_attacker = eid }
            else return sbw { sbw_on_loss_defender = eid }
        addLine' atde sbw [pdx| on_cancel = $eid |] = do
            if atde
            then return  sbw { sbw_on_cancel_attacker = eid }
            else return sbw { sbw_on_cancel_defender = eid }
        addLine' atde sbw [pdx| num_provinces = %_ |] = return sbw
        addLine' _ sbw [pdx| leader_score = %_ |] = return sbw
        addLine' _ sbw [pdx| dig_in_factor = %_ |] = return sbw
        addLine' _ sbw [pdx| terrain_factor = %_ |] = return sbw
        addLine' _ sbw [pdx| modifier = %_ |] = return sbw
        addLine' _ sbw [pdx| $other = %_ |] = trace ("unknown section in startBorderWar@attdef: " ++ show other) $ return sbw
        addLine' _ sbw stmt = trace ("unknown form in startBorderWar@attdef: " ++ show stmt) $ return sbw
        ppSBW :: StartBorderWar -> PPT g m IndentedMessages
        ppSBW sbw = do
            headMsg <- msgToPP $ MsgStartBorderWar (sbw_state_attacker sbw) (sbw_state_defender sbw) (sbw_change_state_after_war sbw)
            bordEventMsgs <- do
                onwinmsg <- messageText MsgBorderWin
                onlossmsg <- messageText MsgBorderLoss
                oncancelmsg <- messageText MsgBorderCancel
                defmsg <- messageText MsgBorderDefender
                attmsg <- messageText MsgBorderAttacker
                let getevntloc eid = do
                        mloc <- getEventTitle eid
                        return $ fromMaybe eid mloc
                winattevt <- getevntloc (sbw_on_win_attacker sbw)
                windefevt <- getevntloc (sbw_on_win_defender sbw)
                lossattevt <- getevntloc (sbw_on_loss_attacker sbw)
                lossdefevt <- getevntloc (sbw_on_loss_defender sbw)
                cancattevt <- getevntloc (sbw_on_cancel_attacker sbw)
                cancdefevt <- getevntloc (sbw_on_cancel_defender sbw)
                msgwinatt <- indentUp $ msgToPP $ MsgTriggerBorderEvent onwinmsg (sbw_on_win_attacker sbw) winattevt attmsg
                msgwindeff <- indentUp $ msgToPP $ MsgTriggerBorderEvent onwinmsg (sbw_on_win_defender sbw) windefevt defmsg
                msglossatt <- indentUp $ msgToPP $ MsgTriggerBorderEvent onlossmsg (sbw_on_loss_attacker sbw) lossattevt attmsg
                msglossdef <- indentUp $ msgToPP $ MsgTriggerBorderEvent onlossmsg (sbw_on_loss_defender sbw) lossdefevt defmsg
                msgcancatt <- indentUp $ msgToPP $ MsgTriggerBorderEvent oncancelmsg (sbw_on_cancel_attacker sbw) cancattevt attmsg
                msgcancdef <- indentUp $ msgToPP $ MsgTriggerBorderEvent oncancelmsg (sbw_on_cancel_defender sbw) cancdefevt defmsg
                return $ msgwinatt ++ msgwindeff ++ msglossatt ++ msglossdef ++ msgcancatt ++ msgcancdef
            return $ headMsg ++ bordEventMsgs
startBorderWar stmt = preStatement stmt

-----------------------------------
-- handler for count_triggers    --
-----------------------------------

-- | How many of the conditions under it must hold. Script writes the number
-- with any of @=@, @>@ and @<@, and the three do not say the same thing, so
-- the comparison is carried into the message rather than assumed.
countTriggers :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
countTriggers stmt@[pdx| %_ = @scr |] =
    withCurrentIndent $ \i -> do
        let (mamount, rest) = extractStmt (matchLhsText "amount") scr
            withAmount num comp = do
                script_pp'd <- ppMany rest
                return ((i, MsgCountTriggers num comp) : script_pp'd)
        case mamount of
            Just [pdx| %_ = !num |] -> withAmount num "At least"
            Just [pdx| %_ > !num |] -> withAmount num "More than"
            Just [pdx| %_ < !num |] -> withAmount num "Fewer than"
            _ -> preStatement stmt
countTriggers stmt = preStatement stmt

-----------------------------------
-- handler for for_loop_effect   --
-----------------------------------

-- | Runs what is under it once for each value of a counter. The bounds and the
-- counter are fields of the block rather than effects of their own, so they are
-- read off into the heading and only the body is listed.
forLoopEffect :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
forLoopEffect [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let (mstart, s1) = extractStmt (matchLhsText "start") scr
        (mend,   s2) = extractStmt (matchLhsText "end") s1
        (madd,   s3) = extractStmt (matchLhsText "add") s2
        (mvalue, s4) = extractStmt (matchLhsText "value") s3
        -- The name of the variable that breaks the loop says nothing about what
        -- the loop does, and neither does how its bounds are compared.
        (_,      s5) = extractStmt (matchLhsText "break") s4
        (_,     rest) = extractStmt (matchLhsText "compare") s5
        -- Defaults are the game's: from 0, up by 1, counting in 'v'.
        bound def mstmt = case mstmt of
            Just [pdx| %_ = !num |] -> Doc.doc2text (plainNumMin (num :: Double))
            Just [pdx| %_ = $vartag:$var |] -> vartag <> ":" <> var
            Just [pdx| %_ = $var |] -> var
            _ -> def
        loopvar = case mvalue of
            Just [pdx| %_ = $var |] -> var
            _ -> "v"
    script_pp'd <- ppMany rest
    return ((i, MsgForLoop loopvar (bound "0" mstart) (bound "0" mend) (bound "1" madd))
            : script_pp'd)
forLoopEffect stmt = preStatement stmt

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

-- | The two states a border war is fought between, as every effect that works
-- on an ongoing one names them, along with whatever else the block held.
borderWarSides :: (HOI4Info g, Monad m) =>
    GenericScript -> PPT g m (Text, Text, GenericScript)
borderWarSides scr = do
    let (matt, rest) = extractStmt (matchLhsText "attacker") scr
        (mdef, rest') = extractStmt (matchLhsText "defender") rest
    att <- statedFrom matt
    def <- statedFrom mdef
    return (att, def, rest')

cancelBorderWar :: (HOI4Info g, Monad m) => StatementHandler g m
cancelBorderWar [pdx| %_ = @scr |] = do
    (att, def, _) <- borderWarSides scr
    msgToPP $ MsgCancelBorderWar att def
cancelBorderWar stmt = preStatement stmt

-- | Ends a border war with one side declared the winner. Where neither side is
-- named the winner the war is called off instead, which is what cancelling it
-- comes to.
finalizeBorderWar :: (HOI4Info g, Monad m) => StatementHandler g m
finalizeBorderWar [pdx| %_ = @scr |] = do
    (att, def, rest) <- borderWarSides scr
    let attwin = not (null [() | [pdx| attacker_win = yes |] <- rest])
        defwin = not (null [() | [pdx| defender_win = yes |] <- rest])
    msgToPP $ if attwin then MsgFinalizeBorderWar att def
              else if defwin then MsgFinalizeBorderWar def att
              else MsgCancelBorderWar att def
finalizeBorderWar stmt = preStatement stmt

-- | Changes the terms an ongoing border war is fought on. What it changes is
-- listed under the two states, the way the game's own tooltip lists it.
setBorderWarData :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setBorderWarData [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    (att, def, rest) <- borderWarSides scr
    changes <- indentUp (concat <$> traverse change rest)
    return ((i, MsgSetBorderWarData att def) : changes)
    where
        change :: GenericStatement -> PPT g m IndentedMessages
        change [pdx| combat_width = !num |] = msgToPP $ MsgBorderWarCombatWidth num
        change [pdx| attacker_modifier = !num |] = msgToPP $ MsgBorderWarSideModifier "Attacker" num
        change [pdx| defender_modifier = !num |] = msgToPP $ MsgBorderWarSideModifier "Defender" num
        change [pdx| change_state_after_war = %rhs |]
            | GenericRhs "yes" [] <- rhs = msgToPP $ MsgBorderWarChangeState True
            | otherwise = msgToPP $ MsgBorderWarChangeState False
        -- Whether the events the war was started with fire is nothing the
        -- reader can see either way.
        change [pdx| dont_fire_events = %_ |] = return []
        change stmt = preStatement stmt
setBorderWarData stmt = preStatement stmt

-------------------------------------
-- handler for teleport_armies     --
-------------------------------------

-- | Moves the armies in a state elsewhere. The @limit@ is a condition on
-- whoever owns them rather than a narrowing of the destination, so it keeps a
-- block of its own under the line saying where they go.
teleportArmies :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
teleportArmies [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let isTo st = any (`matchLhsText` st) ["to_state", "to_state_array", "to_province"]
        -- Script sometimes names a state and a province both. The first named
        -- is where they go; the rest is the game's own fallback and says
        -- nothing more.
        (tos, rest) = partition isTo scr
        mto = listToMaybe tos
    header <- case mto of
        Just [pdx| to_state_array = $vartag:$var |] -> return $ MsgTeleportArmiesArray (vartag <> "." <> var)
        Just [pdx| to_state_array = $arr |] -> return $ MsgTeleportArmiesArray arr
        Just [pdx| to_province = !num |] -> return $ MsgTeleportArmiesProvince num
        Just to@[pdx| to_state = %_ |] -> MsgTeleportArmies <$> statedFrom (Just to)
        -- With nowhere named the game sends them to their owner's capital.
        _ -> return MsgTeleportArmiesCapital
    script_pp'd <- ppMany rest
    return ((i, header) : script_pp'd)
teleportArmies stmt = preStatement stmt

-------------------------------------
-- handler for transfer_navy       --
-------------------------------------

transferNavy :: (HOI4Info g, Monad m) => StatementHandler g m
transferNavy stmt@[pdx| %_ = @scr |] = do
    let (mtarget, rest) = extractStmt (matchLhsText "target") scr
        exile = not (null [() | [pdx| is_government_in_exile = yes |] <- rest])
    case mtarget of
        Just [pdx| %_ = $tag |] -> do
            who <- flagText (Just HOI4Country) tag
            msgToPP $ MsgTransferNavy who exile
        _ -> preStatement stmt
transferNavy stmt = preStatement stmt

-------------------------------------------------------
-- handler for ships_in_area and ships_in_state_ports --
-------------------------------------------------------

data ShipsIn = ShipsIn
        {   si_size :: Double
        ,   si_comp :: Text
        ,   si_type :: Maybe Text
        ,   si_where :: Maybe Text
        }

newSI :: ShipsIn
newSI = ShipsIn 0 "at least" Nothing Nothing

-- | How many ships of a kind the country has somewhere: a strategic region for
-- @ships_in_area@, a state's ports for @ships_in_state_ports@. The two are
-- written alike but for the field naming the place.
shipsIn :: forall g m. (HOI4Info g, Monad m) =>
    Text -- ^ Label of the field naming the place
        -> (Int -> PPT g m Text) -- ^ How to name the place from its id
        -> (Text -> Double -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
shipsIn wherelabel wherelocof msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppSI =<< foldM addLine newSI scr
    where
        addLine :: ShipsIn -> GenericStatement -> PPT g m ShipsIn
        addLine si [pdx| size > !num |] = return si { si_comp = "more than", si_size = num }
        addLine si [pdx| size < !num |] = return si { si_comp = "less than", si_size = num }
        addLine si [pdx| size = !num |] = return si { si_comp = "at least", si_size = num }
        addLine si [pdx| type = $txt |] = return si { si_type = Just txt }
        addLine si st@[pdx| $label = %_ |] | sameKey label wherelabel = case st of
            [pdx| %_ = !num |] -> do
                whereloc <- wherelocof num
                return si { si_where = Just whereloc }
            _ -> do
                whereloc <- statedFrom (Just st)
                return si { si_where = Just whereloc }
        addLine si st = trace ("unknown form in shipsIn: " ++ show st) $ return si
        ppSI :: ShipsIn -> PPT g m ScriptMessage
        ppSI si = do
            -- With no kind named the trigger counts every ship there is.
            typeloc <- maybe (return "ships") getGameL10n (si_type si)
            return $ msg (si_comp si) (si_size si) typeloc
                         (fromMaybe "<!-- Check Script -->" (si_where si))
shipsIn _ _ _ stmt = preStatement stmt

-------------------------------
-- handler for any_state_in  --
-------------------------------

-- | The states of a strategic region, a continent, or an array, with a
-- condition asked of each.
anyStateIn :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
anyStateIn [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let isWhere st = any (`matchLhsText` st)
            ["strategic_region", "array", "continent", "ai_area"]
        (mwhere, rest) = extractStmt isWhere scr
    header <- case mwhere of
        Just [pdx| strategic_region = !num |] -> MsgAnyStateIn <$> getRegionLoc num
        Just [pdx| continent = $con |] -> MsgAnyStateIn <$> getGameL10n con
        Just [pdx| %_ = $vartag:$var |] -> return $ MsgAnyStateInArray (vartag <> "." <> var)
        Just [pdx| %_ = $arr |] -> return $ MsgAnyStateInArray arr
        _ -> return $ MsgAnyStateInArray "<!-- Check Script -->"
    script_pp'd <- ppMany rest
    return ((i, header) : script_pp'd)
anyStateIn stmt = preStatement stmt

---------------------------------------
-- handler for add_province_modifier --
---------------------------------------

addProvinceModifier :: forall g m. (HOI4Info g, Monad m) => Bool -> StatementHandler g m
addProvinceModifier addrem stmt@[pdx| %_ = @scr |] =
    msgToPP =<< pp_apm (foldl' addLine ([], newProvSel) scr)
    where
        addLine :: ([Text], ProvSel) -> GenericStatement -> ([Text], ProvSel)
        addLine (mods, ps) [pdx| static_modifiers = @mscr |] = (mapMaybe getbaremods mscr, ps)
        addLine (mods, ps) [pdx| province = !num |] = (mods, ps { ps_ids = Just [num] })
        addLine (mods, ps) [pdx| province = @pscr |] = (mods, foldl' (provSelLine "add_province_modifier") ps pscr)
        addLine acc [pdx| $other = %_ |] = trace ("unknown section in add_province_modifier: " ++ show other) acc
        addLine acc stmt = trace ("Unknown in add_province_modifier: " ++ show stmt) acc

        getbaremods (StatementBare (GenericLhs e [])) = Just e
        getbaremods stmt = trace ("Unknown in static_modifier array statement: " ++ show stmt) Nothing

        pp_apm :: ([Text], ProvSel) -> PPT g m ScriptMessage
        pp_apm (mods, ps) = do
            modlocs <- mapM (\m -> do
                loc <- getGameL10n m
                return $ "<!--" <> m <> "-->" <> Doc.doc2text (iquotes loc))
                mods
            let modloc
                    | length modlocs > 1 = "modifiers " <> T.intercalate ", " modlocs
                    | length modlocs == 1 = "modifier " <> T.concat modlocs
                    | otherwise = "<!--CHECK SCRIPT-->"
            prov <- ppProvSel ps
            return $ MsgAddProvinceModifier addrem modloc prov
addProvinceModifier _ stmt = trace ("Not handled in addProvinceModifier: " ++ show stmt) $ preStatement stmt


-------------------------------------------
-- handler for is_power_balance_in_range --
-------------------------------------------

data PowerBalanceRange = PowerBalanceRange
        {   pbr_id :: Text
        ,   pbr_range :: Text
        ,   pbr_comp :: Double
        }

newPBR :: PowerBalanceRange
newPBR = PowerBalanceRange "<!--Check Script-->" "<!--Check Script-->" 0

powerBalanceRange :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
powerBalanceRange stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppPBR =<< foldM addLine newPBR scr
    where
        addLine :: PowerBalanceRange -> GenericStatement -> PPT g m PowerBalanceRange
        addLine pbr [pdx| id = ?txt |] = return pbr { pbr_id = txt }
        addLine pbr [pdx| range = ?txt |] = return pbr { pbr_range = txt }
        addLine pbr [pdx| range > ?txt |] = return pbr { pbr_range = txt, pbr_comp = 1 }--right
        addLine pbr [pdx| range < ?txt |] = return pbr { pbr_range = txt, pbr_comp = -1 }--left
        addLine pbr [pdx| $other = %_ |] = trace ("unknown section in powerBalanceRange: " ++ show other) $ return pbr
        addLine pbr stmt = trace ("unknown form in powerBalanceRange: " ++ show stmt) $ return pbr
        ppPBR :: PowerBalanceRange -> PPT g m ScriptMessage
        ppPBR pbr = do
            idloc <- getGameL10n (pbr_id pbr)
            rangeloc <- getGameL10n (pbr_range pbr)
            return $ MsgIsPowerBalanceInRange idloc rangeloc (pbr_id pbr) (pbr_range pbr) (pbr_comp pbr)
powerBalanceRange stmt = preStatement stmt

-------------------------------------------
-- handler for is_power_balance_in_range --
-------------------------------------------

data NavalStrengthComparison = NavalStrengthComparison
        {   nsc_other :: Text
        ,   nsc_ratio :: Double
        ,   nsc_comp :: Text
        ,   nsc_sub_unit_def_weights :: [(Text,Double)]
        }

newNSC :: NavalStrengthComparison
newNSC = NavalStrengthComparison "<!--Check Script-->" 0 "<!--Check Script-->" []

navalStrengthComparison :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
navalStrengthComparison stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppNSC =<< foldM addLine newNSC scr
    where
        addLine :: NavalStrengthComparison -> GenericStatement -> PPT g m NavalStrengthComparison
        addLine nsc [pdx| other = ?txt |] = return nsc { nsc_other = txt }
        addLine nsc [pdx| ratio > !amt |] = return nsc { nsc_ratio = amt, nsc_comp = "more" }--right
        addLine nsc [pdx| ratio < !amt |] = return nsc { nsc_ratio = amt, nsc_comp = "less" }--left
        addLine nsc [pdx| sub_unit_def_weights = @scr |] = return $ foldl' addLine' nsc scr
        addLine nsc [pdx| $other = %_ |] = trace ("unknown section in navalStrengthComparison: " ++ show other) $ return nsc
        addLine nsc stmt = trace ("unknown form in navalStrengthComparison: " ++ show stmt) $ return nsc
        addLine' :: NavalStrengthComparison -> GenericStatement -> NavalStrengthComparison
        addLine' nsc [pdx| $shiptype = !weight |] = do
            let oldweight = nsc_sub_unit_def_weights nsc
            nsc { nsc_sub_unit_def_weights = oldweight ++ [(shiptype, weight)]}
        addLine' nsc [pdx| $other = %_ |] = trace ("unknown section in navalStrengthComparison weights: " ++ show other) nsc
        addLine' nsc stmt = trace ("unknown form in navalStrengthComparison weights: " ++ show stmt) nsc

        ppNSC :: NavalStrengthComparison -> PPT g m ScriptMessage
        ppNSC nsc = do
            otherflag <- flagText (Just HOI4Country) (nsc_other nsc)
            weighttext <- case nsc_sub_unit_def_weights nsc of
                [] -> return ""
                weights -> do
                    weightype <- mapM (\wt -> do
                        let (shiptype, weight) = wt
                        shiploc <- getGameL10n shiptype
                        let weighttxt = Doc.doc2text (plainNum weight)
                        return $ weighttxt <> " for " <> shiploc)
                        weights
                    return $ " with weight " <> T.intercalate ", " weightype
            return $ MsgavalStrengthComparison (nsc_ratio nsc) (nsc_comp nsc) otherflag weighttext
navalStrengthComparison stmt = preStatement stmt

-- unlock decision tooltip

unlockDecisionTooltip :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
unlockDecisionTooltip stmt@[pdx| %_ = $_ |] = locandid MsgUnlockDecisionTooltip stmt
unlockDecisionTooltip stmt@[pdx| %_ = @scr |] = do
    let (mname, _) = extractStmt (matchLhsText "decision") scr
    case mname of
        Just stmt@[pdx| %_ = ?txt |] -> locandid MsgUnlockDecisionTooltip stmt
        _ -> preStatement stmt
unlockDecisionTooltip stmt = preStatement stmt
-- | Handler for @set_division_template_lock@, which stops a template being edited
-- and its divisions being trained or disbanded, or lets them be again.
setDivisionTemplateLock :: (HOI4Info g, Monad m) => StatementHandler g m
setDivisionTemplateLock stmt@[pdx| %_ = @scr |] = case (template, locked) of
    (Just name, Just yn) -> msgToPP (MsgSetDivisionTemplateLock name yn)
    _ -> preStatement stmt
    where
        (template, locked) = foldl' addLine (Nothing, Nothing) scr
        addLine (t, l) [pdx| division_template = ?name |] = (Just name, l)
        addLine (t, l) [pdx| is_locked = yes |] = (t, Just True)
        addLine (t, l) [pdx| is_locked = no |] = (t, Just False)
        addLine acc _ = acc
setDivisionTemplateLock stmt = preStatement stmt

-- | Handler for @clear_division_template_cap@, which lifts the limit on how many
-- divisions of a template the country may hold.
clearDivisionTemplateCap :: (HOI4Info g, Monad m) => StatementHandler g m
clearDivisionTemplateCap stmt@[pdx| %_ = @scr |] =
    case [name | [pdx| division_template = ?name |] <- scr] of
        (name : _) -> msgToPP (MsgClearDivisionTemplateCap name)
        [] -> preStatement stmt
clearDivisionTemplateCap stmt = preStatement stmt

-------------------------------------
-- Handler for has_resources_amount --
-------------------------------------
data HasResAmount = HasResAmount
        {   hra_resource :: Maybe Text
        ,   hra_comp :: Text
        ,   hra_amount :: Double
        ,   hra_state :: Maybe Int
        ,   hra_delivered :: Bool
        }

newHRA :: HasResAmount
newHRA = HasResAmount Nothing "at least" 0 Nothing False

-- | Handler for @has_resources_amount@, which asks how much of a resource a
-- state has. @delivered@ asks after what actually reaches the state's
-- controller, which is what is left once the modifiers on it have applied.
hasResourcesAmount :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasResourcesAmount stmt@[pdx| %_ = @scr |] = case hra_resource hra of
        Just res -> do
            stateloc <- traverse getStateLoc (hra_state hra)
            msgToPP $ MsgHasResourcesAmount (hra_delivered hra) (hra_comp hra)
                        (hra_amount hra) (iconText res) (fromMaybe "" stateloc)
        Nothing -> preStatement stmt
    where
        hra = foldl' addLine newHRA scr
        addLine h [pdx| resource = ?r |] = h { hra_resource = Just r }
        addLine h [pdx| amount > !n |] = h { hra_comp = "more than", hra_amount = n }
        addLine h [pdx| amount < !n |] = h { hra_comp = "less than", hra_amount = n }
        addLine h [pdx| amount = !n |] = h { hra_comp = "at least", hra_amount = n }
        addLine h [pdx| state = !n |] = h { hra_state = Just n }
        addLine h [pdx| delivered = yes |] = h { hra_delivered = True }
        addLine h [pdx| delivered = no |] = h { hra_delivered = False }
        addLine h stmt = trace ("unknown section in has_resources_amount: " ++ show stmt) h
hasResourcesAmount stmt = preStatement stmt

---------------------------------------------
-- Handler for any_province_building_level --
---------------------------------------------
data AnyProvBuilding = AnyProvBuilding
        {   apb_building :: Maybe Text
        ,   apb_comp :: Text
        ,   apb_level :: Double
        ,   apb_provinces :: [Double]
        ,   apb_all :: Bool
        ,   apb_border :: Bool
        ,   apb_coastal :: Bool
        ,   apb_naval :: Bool
        ,   apb_victory_point :: Bool
        }

newAPB :: AnyProvBuilding
newAPB = AnyProvBuilding Nothing "at least" 0 [] False False False False False

-- | Handler for @any_province_building_level@, which asks whether any province
-- of the state, of those the @province@ block picks out, has a building of the
-- level given.
anyProvinceBuildingLevel :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
anyProvinceBuildingLevel stmt@[pdx| %_ = @scr |] = case apb_building apb of
        Just bld -> msgToPP $ MsgAnyProvinceBuildingLevel (iconText bld)
                        (apb_comp apb) (apb_level apb) whichProvinces
        Nothing -> preStatement stmt
    where
        apb = foldl' addLine newAPB scr
        addLine a [pdx| building = $b |] = a { apb_building = Just b }
        addLine a [pdx| level > !n |] = a { apb_comp = "more than", apb_level = n }
        addLine a [pdx| level < !n |] = a { apb_comp = "fewer than", apb_level = n }
        addLine a [pdx| level = !n |] = a { apb_comp = "at least", apb_level = n }
        addLine a [pdx| province = !n |] = a { apb_provinces = apb_provinces a ++ [n] }
        addLine a [pdx| province = @pscr |] = foldl' addProv a pscr
        addLine a stmt = trace ("unknown section in any_province_building_level: " ++ show stmt) a

        addProv a [pdx| id = !n |] = a { apb_provinces = apb_provinces a ++ [n] }
        addProv a [pdx| all_provinces = yes |] = a { apb_all = True }
        addProv a [pdx| limit_to_border = yes |] = a { apb_border = True }
        addProv a [pdx| limit_to_coastal = yes |] = a { apb_coastal = True }
        addProv a [pdx| limit_to_naval_base = yes |] = a { apb_naval = True }
        addProv a [pdx| limit_to_victory_point = yes |] = a { apb_victory_point = True }
        addProv a [pdx| $_ = no |] = a
        addProv a stmt = trace ("unknown section in any_province_building_level@province: " ++ show stmt) a

        -- Which provinces of the state count. Nothing said means all of them,
        -- which is also what @all_provinces@ asks for outright.
        whichProvinces = T.intercalate ", " (filter (not . T.null) [limits, listed])
        limits = T.intercalate " and " $ concat
            [ [ "on the border" | apb_border apb ]
            , [ "on the coast" | apb_coastal apb ]
            , [ "with a naval base" | apb_naval apb ]
            , [ "with victory points" | apb_victory_point apb ]
            ]
        listed = case apb_provinces apb of
            [] -> ""
            provs -> T.pack (concat ["(", intercalate "), (" (map (show . round) provs), ")"])
anyProvinceBuildingLevel stmt = preStatement stmt

---------------------------------------
-- Handler for compare_autonomy_state --
---------------------------------------
-- | Handler for @compare_autonomy_state@, which ranks the subject's autonomy
-- level against a named one. It is written with a @>@ or a @<@ rather than an
-- @=@, so it reads as a comparison and not as a check for that level itself.
compareAutonomyState :: (HOI4Info g, Monad m) => StatementHandler g m
compareAutonomyState stmt = case stmt of
    [pdx| %_ > $lvl |] -> withLevel "more autonomous than" lvl
    [pdx| %_ < $lvl |] -> withLevel "less autonomous than" lvl
    [pdx| %_ = $lvl |] -> withLevel "at least as autonomous as" lvl
    _ -> preStatement stmt
    where
        withLevel comp lvl = do
            loc <- getGameL10n lvl
            msgToPP (MsgCompareAutonomyState comp loc)

-------------------------
-- Handlers for arrays --
-------------------------
-- | The array a statement names and the value it carries, written either as
-- @array = name value = x@ or in the short form that names the array on the
-- left and the value on the right.
arrayAndValue :: GenericScript -> Maybe (Text, Maybe Text)
arrayAndValue scr = case foldl' addLine (Nothing, Nothing, Nothing) scr of
    (Just arr, val, _) -> Just (arr, val)
    (Nothing, _, Just short) -> Just short
    _ -> Nothing
    where
        addLine (arr, val, short) [pdx| array = $a |] = (Just a, val, short)
        addLine (arr, val, short) [pdx| array = ?a |] = (Just a, val, short)
        addLine (arr, val, short) [pdx| value = ?v |] = (arr, Just v, short)
        addLine (arr, val, short) [pdx| value = !v |] =
            (arr, Just (Doc.doc2text (plainNum (v :: Double))), short)
        addLine acc [pdx| index = %_ |] = acc
        -- Short form. Only taken if no @array@ line has claimed the slot, so
        -- that the long form is never read as one of these.
        addLine (arr, val, short) [pdx| $a = ?v |] = (arr, val, Just (a, Just v))
        addLine (arr, val, short) [pdx| $a = !v |] =
            (arr, val, Just (a, Just (Doc.doc2text (plainNum (v :: Double)))))
        addLine acc _ = acc

-- | Handler for @add_to_array@ and @is_in_array@, which take an array and a
-- value. A value left out of @add_to_array@ means whatever is in scope.
arrayValue :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
arrayValue msg stmt@[pdx| %_ = @scr |] = case arrayAndValue scr of
    Just (arr, val) -> msgToPP (msg arr (fromMaybe "" val))
    Nothing -> preStatement stmt
arrayValue _ stmt = preStatement stmt

----------------------------
-- Handler for create_ship --
----------------------------
data CreateShip = CreateShip
        {   cs_type :: Maybe Text
        ,   cs_variant :: Maybe Text
        ,   cs_name :: Maybe Text
        ,   cs_creator :: Maybe (Either Text (Text, Text))
        ,   cs_amount :: Double
        }

newCS :: CreateShip
newCS = CreateShip Nothing Nothing Nothing Nothing 1

-- | Handler for @create_ship@, which puts a ship straight into the reserve
-- fleet. @creator@ names whose design it is built to, which is what decides
-- what the ship can do; left out, it is the country's own.
createShip :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createShip stmt@[pdx| %_ = @scr |] = case cs_type cs of
        Just shiptype -> do
            shiploc <- T.strip <$> getGameL10n shiptype
            creator <- case cs_creator cs of
                Just who -> fromMaybe "" <$> eflag (Just HOI4Country) who
                Nothing -> return ""
            msgToPP $ MsgCreateShip (cs_amount cs) shiploc
                        (fromMaybe "" (cs_variant cs)) (fromMaybe "" (cs_name cs)) creator
        Nothing -> preStatement stmt
    where
        cs = foldl' addLine newCS scr
        addLine c [pdx| type = $t |] = c { cs_type = Just t }
        addLine c [pdx| equipment_variant = ?v |] = c { cs_variant = Just v }
        addLine c [pdx| name = ?n |] = c { cs_name = Just n }
        addLine c [pdx| amount = !n |] = c { cs_amount = n }
        addLine c [pdx| creator = $vartag:$var |] = c { cs_creator = Just (Right (vartag, var)) }
        addLine c [pdx| creator = ?t |] = c { cs_creator = Just (Left t) }
        -- Whether a ship already in for a refit may be picked. Says nothing
        -- about what is created.
        addLine c [pdx| exclude_refitting = %_ |] = c
        addLine c stmt = trace ("unknown section in create_ship: " ++ show stmt) c
createShip stmt = preStatement stmt

------------------------------
-- Handler for transfer_ship --
------------------------------
-- | Handler for @transfer_ship@, which hands one ship of a kind over to another
-- country. @prefer_name@ picks which one, where the fleet has a ship by that
-- name to give.
transferShip :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
transferShip stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing) scr of
        (Just shiptype, target, name) -> do
            shiploc <- T.strip <$> getGameL10n shiptype
            targetloc <- case target of
                Just who -> fromMaybe "" <$> eflag (Just HOI4Country) who
                Nothing -> return ""
            msgToPP $ MsgTransferShip shiploc (fromMaybe "" name) targetloc
        _ -> preStatement stmt
    where
        addLine (t, tg, n) [pdx| type = $ty |] = (Just ty, tg, n)
        addLine (t, tg, n) [pdx| target = $vartag:$var |] = (t, Just (Right (vartag, var)), n)
        addLine (t, tg, n) [pdx| target = ?tgt |] = (t, Just (Left tgt), n)
        addLine (t, tg, n) [pdx| prefer_name = ?nm |] = (t, tg, Just nm)
        addLine acc [pdx| exclude_refitting = %_ |] = acc
        addLine acc stmt = trace ("unknown section in transfer_ship: " ++ show stmt) acc
transferShip stmt = preStatement stmt

-------------------------------------
-- Handler for add_equipment_subsidy --
-------------------------------------
-- | Handler for @add_equipment_subsidy@, which sets aside industrial capacity to
-- pay for equipment bought from someone else. The sellers it may be spent with
-- are named either outright or by a scripted trigger.
addEquipmentSubsidy :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addEquipmentSubsidy stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, 0, [], Nothing) scr of
        (Just eqtype, cic, sellers, mtrigger) -> do
            eqloc <- T.strip <$> getGameL10n eqtype
            sellerlocs <- traverse (flagText (Just HOI4Country)) sellers
            msgToPP $ MsgAddEquipmentSubsidy cic eqloc
                        (joinClauses sellerlocs) (fromMaybe "" mtrigger)
        _ -> preStatement stmt
    where
        addLine (t, c, s, g) [pdx| equipment_type = $ty |] = (Just ty, c, s, g)
        addLine (t, c, s, g) [pdx| cic = !n |] = (t, n, s, g)
        addLine (t, c, s, g) [pdx| seller_tags = @tags |] =
            (t, c, s ++ [tag | StatementBare (GenericLhs tag []) <- tags], g)
        addLine (t, c, s, g) [pdx| seller_trigger = $trg |] = (t, c, s, Just trg)
        addLine acc stmt = trace ("unknown section in add_equipment_subsidy: " ++ show stmt) acc
addEquipmentSubsidy stmt = preStatement stmt

----------------------------------------
-- Handler for add_equipment_production --
----------------------------------------
data AddEquipProd = AddEquipProd
        {   aep_type :: Maybe Text
        ,   aep_creator :: Maybe Text
        ,   aep_version :: Maybe Text
        ,   aep_name :: Maybe Text
        ,   aep_factories :: Maybe Double
        ,   aep_progress :: Maybe Double
        ,   aep_efficiency :: Maybe Double
        }

newAEP :: AddEquipProd
newAEP = AddEquipProd Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Handler for @add_equipment_production@, which opens a production line ready
-- built rather than leaving the player to set one up.
addEquipmentProduction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addEquipmentProduction stmt@[pdx| %_ = @scr |] = case aep_type aep of
        Just eqtype -> do
            eqloc <- T.strip <$> getGameL10n eqtype
            creator <- traverse (flagText (Just HOI4Country)) (aep_creator aep)
            msgToPP $ MsgAddEquipmentProduction
                        (fromMaybe 1 (aep_factories aep)) eqloc
                        (fromMaybe "" (aep_version aep)) (fromMaybe "" creator)
                        (fromMaybe "" (aep_name aep))
                        (fromMaybe 0 (aep_progress aep))
                        (fromMaybe 0 (aep_efficiency aep))
        Nothing -> preStatement stmt
    where
        aep = foldl' addLine newAEP scr
        addLine a [pdx| equipment = @escr |] = foldl' addEquip a escr
        addLine a [pdx| name = ?n |] = a { aep_name = Just n }
        addLine a [pdx| requested_factories = !n |] = a { aep_factories = Just n }
        addLine a [pdx| progress = !n |] = a { aep_progress = Just n }
        addLine a [pdx| efficiency = !n |] = a { aep_efficiency = Just n }
        -- How many of the thing to make. The line runs until it is called off
        -- either way, so it is the factories put on it that are worth reading.
        addLine a [pdx| amount = %_ |] = a
        -- Which organization builds it, which the wiki says nothing about yet.
        addLine a [pdx| industrial_manufacturer = %_ |] = a
        addLine a stmt = trace ("unknown section in add_equipment_production: " ++ show stmt) a

        addEquip a [pdx| type = $t |] = a { aep_type = Just t }
        addEquip a [pdx| creator = ?c |] = a { aep_creator = Just c }
        addEquip a [pdx| version_name = ?v |] = a { aep_version = Just v }
        addEquip a [pdx| version = %_ |] = a
        addEquip a stmt = trace ("unknown section in add_equipment_production@equipment: " ++ show stmt) a
addEquipmentProduction stmt = preStatement stmt

-----------------------------------------
-- Handler for create_production_license --
-----------------------------------------
-- | Handler for @create_production_license@, which lets another country build
-- one of this country's designs.
createProductionLicense :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createProductionLicense stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing, Nothing) scr of
        (Just eqtype, target, version, cost) -> do
            eqloc <- T.strip <$> getGameL10n eqtype
            targetloc <- case target of
                Just who -> fromMaybe "" <$> eflag (Just HOI4Country) who
                Nothing -> return ""
            msgToPP $ MsgCreateProductionLicense targetloc eqloc
                        (fromMaybe "" version) (fromMaybe 1 cost)
        _ -> preStatement stmt
    where
        addLine (t, tg, v, c) [pdx| equipment = @escr |] = foldl' addEquip (t, tg, v, c) escr
        addLine (t, tg, v, c) [pdx| target = $vartag:$var |] = (t, Just (Right (vartag, var)), v, c)
        addLine (t, tg, v, c) [pdx| target = ?tgt |] = (t, Just (Left tgt), v, c)
        addLine (t, tg, v, c) [pdx| cost_factor = !n |] = (t, tg, v, Just n)
        addLine acc [pdx| new_prioritised = %_ |] = acc
        addLine acc stmt = trace ("unknown section in create_production_license: " ++ show stmt) acc

        addEquip (t, tg, v, c) [pdx| type = $ty |] = (Just ty, tg, v, c)
        addEquip (t, tg, v, c) [pdx| version_name = ?vn |] = (t, tg, Just vn, c)
        addEquip acc [pdx| version = %_ |] = acc
        addEquip acc stmt = trace ("unknown section in create_production_license@equipment: " ++ show stmt) acc
createProductionLicense stmt = preStatement stmt

--------------------------------------------
-- Handler for create_faction_from_template --
--------------------------------------------
-- | Handler for @create_faction_from_template@, which starts a faction already
-- set up rather than an empty one. The template is named bare, or given inside a
-- block along with the name the faction is to go by.
createFactionFromTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createFactionFromTemplate stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (Just tmpl, mname) -> do
            nameloc <- traverse getGameL10n mname
            msgToPP (MsgCreateFactionFromTemplate (fromMaybe "" nameloc) tmpl)
        _ -> preStatement stmt
    where
        addLine (t, n) [pdx| template = $tm |] = (Just tm, n)
        addLine (t, n) [pdx| name = $nm |] = (t, Just nm)
        addLine (t, n) [pdx| name = ?nm |] = (t, Just nm)
        addLine acc [pdx| icon = %_ |] = acc
        addLine acc [pdx| color = %_ |] = acc
        addLine acc stmt = trace ("unknown section in create_faction_from_template: " ++ show stmt) acc
createFactionFromTemplate [pdx| %_ = $tmpl |] = msgToPP (MsgCreateFactionFromTemplate "" tmpl)
createFactionFromTemplate stmt = preStatement stmt

----------------------------------------------
-- Handler for add_units_to_division_template --
----------------------------------------------
-- | Handler for @add_units_to_division_template@, which adds battalions or
-- support companies to a template, and so to every division already built to it.
addUnitsToDivisionTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addUnitsToDivisionTemplate stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, []) scr of
        (Just name, units@(_:_)) -> do
            unitlocs <- traverse getGameL10n units
            msgToPP (MsgAddUnitsToDivisionTemplate name (joinClauses unitlocs))
        _ -> preStatement stmt
    where
        addLine (n, us) [pdx| template_name = ?nm |] = (Just nm, us)
        addLine (n, us) [pdx| regiments = @rscr |] = (n, us ++ unitNames rscr)
        addLine (n, us) [pdx| support = @rscr |] = (n, us ++ unitNames rscr)
        addLine (n, us) [pdx| regimental_support = @rscr |] = (n, us ++ unitNames rscr)
        addLine acc stmt = trace ("unknown section in add_units_to_division_template: " ++ show stmt) acc
        -- The number beside each unit is the column it goes in, not how many of
        -- it to add, so there is nothing in it worth reading on the wiki.
        unitNames uscr = [unit | [pdx| $unit = %_ |] <- uscr]
addUnitsToDivisionTemplate stmt = preStatement stmt

------------------------------------------
-- Handler for set_division_template_cap --
------------------------------------------
-- | Handler for @set_division_template_cap@, which limits how many divisions of
-- a template the country may hold. The cap is often a script constant rather
-- than a number written out.
setDivisionTemplateCap :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setDivisionTemplateCap stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing) scr of
        (Just name, mcap, mcapvar) -> do
            constant <- maybe (return Nothing) constantValue mcapvar
            case (mcap, constant, mcapvar) of
                (Just cap, _, _) -> msgToPP (MsgSetDivisionTemplateCap name cap)
                (_, Just cap, _) -> msgToPP (MsgSetDivisionTemplateCap name cap)
                (_, _, Just capvar) -> msgToPP (MsgSetDivisionTemplateCapVar name capvar)
                _ -> msgToPP (MsgSetDivisionTemplateCap name 1)
        _ -> preStatement stmt
    where
        addLine (n, c, cv) [pdx| division_template = ?nm |] = (Just nm, c, cv)
        addLine (n, c, cv) [pdx| division_cap = !amt |] = (n, Just amt, cv)
        addLine (n, c, cv) [pdx| division_cap = $var |] = (n, c, Just var)
        addLine acc stmt = trace ("unknown section in set_division_template_cap: " ++ show stmt) acc
setDivisionTemplateCap stmt = preStatement stmt

-------------------------
-- Handler for set_truce --
-------------------------
-- | Handler for @set_truce@, which bars the two countries from going to war with
-- one another for a time.
setTruce :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setTruce stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (Just target, mdays) -> do
            targetloc <- fromMaybe "" <$> eflag (Just HOI4Country) target
            msgToPP (MsgSetTruce targetloc (fromMaybe 0 mdays))
        _ -> preStatement stmt
    where
        addLine (t, d) [pdx| target = $vartag:$var |] = (Just (Right (vartag, var)), d)
        addLine (t, d) [pdx| target = ?tgt |] = (Just (Left tgt), d)
        addLine (t, d) [pdx| days = !n |] = (t, Just n)
        addLine acc stmt = trace ("unknown section in set_truce: " ++ show stmt) acc
setTruce stmt = preStatement stmt

----------------------------
-- Handler for white_peace --
----------------------------
-- | Handler for @white_peace@, which ends the war between the two countries with
-- neither taking anything. The other side is named bare, or given as @tag@
-- inside a block where a @message@ may name the event that tells it so.
whitePeace :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
whitePeace stmt@[pdx| %_ = @scr |] =
    case [inner | inner@[pdx| tag = %_ |] <- scr] of
        (tagstmt : _) -> withFlag MsgMakeWhitePeace tagstmt
        [] -> preStatement stmt
whitePeace stmt = withFlag MsgMakeWhitePeace stmt

-----------------------
-- Handler for puppet --
-----------------------
-- | Handler for @puppet@, which makes the target a subject. Written as a block
-- it also says whether the subject's wars are ended along with it, which they
-- are unless the script says otherwise.
puppetCountry :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
puppetCountry stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, True, True) scr of
        (Just target, endwars, endcivil) -> do
            targetloc <- fromMaybe "" <$> eflag (Just HOI4Country) target
            msgToPP (MsgPuppetCountry targetloc (endWarText endwars endcivil))
        _ -> preStatement stmt
    where
        addLine (t, w, c) [pdx| target = $vartag:$var |] = (Just (Right (vartag, var)), w, c)
        addLine (t, w, c) [pdx| target = ?tgt |] = (Just (Left tgt), w, c)
        addLine (t, w, c) [pdx| end_wars = no |] = (t, False, c)
        addLine (t, w, c) [pdx| end_wars = yes |] = (t, True, c)
        addLine (t, w, c) [pdx| end_civil_wars = no |] = (t, w, False)
        addLine (t, w, c) [pdx| end_civil_wars = yes |] = (t, w, True)
        addLine acc [pdx| always = %_ |] = acc
        addLine acc stmt = trace ("unknown section in puppet: " ++ show stmt) acc
puppetCountry stmt = withFlag MsgPuppet stmt

-- | How a subject's own wars are dealt with as it is made one.
endWarText :: Bool -> Bool -> Text
endWarText endwars endcivil = case (endwars, endcivil) of
    (True, True) -> ", ending its wars and civil wars"
    (True, False) -> ", ending its wars"
    (False, True) -> ", ending its civil wars"
    (False, False) -> ""

------------------------------------
-- Handler for set_power_balance --
------------------------------------
-- | Handler for @set_power_balance@, which starts a balance of power or moves
-- one already running. Only the fields it is given take effect; whatever it
-- leaves out is left as it stands.
setPowerBalance :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setPowerBalance stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing, Nothing, False) scr of
        (Just bopid, mvalue, mleft, mright, isdefault) -> do
            boploc <- getGameL10n bopid
            leftloc <- traverse getGameL10n mleft
            rightloc <- traverse getGameL10n mright
            basemsg <- msgToPP (MsgSetPowerBalance boploc bopid)
            valmsg <- case mvalue of
                Just val -> indentUp (msgToPP (MsgSetPowerBalanceValue val))
                Nothing -> return []
            sidemsg <- case (leftloc, rightloc) of
                (Nothing, Nothing) -> return []
                _ -> indentUp (msgToPP (MsgSetPowerBalanceSides
                        (fromMaybe "" leftloc) (fromMaybe "" rightloc)))
            defmsg <- if isdefault
                then indentUp (msgToPP MsgSetPowerBalanceDefault)
                else return []
            return (basemsg ++ valmsg ++ sidemsg ++ defmsg)
        _ -> preStatement stmt
    where
        addLine (i, v, l, r, d) [pdx| id = $bopid |] = (Just bopid, v, l, r, d)
        addLine (i, v, l, r, d) [pdx| set_value = !n |] = (i, Just n, l, r, d)
        addLine (i, v, l, r, d) [pdx| left_side = $s |] = (i, v, Just s, r, d)
        addLine (i, v, l, r, d) [pdx| right_side = $s |] = (i, v, l, Just s, d)
        addLine (i, v, l, r, d) [pdx| set_default = yes |] = (i, v, l, r, True)
        addLine acc [pdx| set_default = no |] = acc
        addLine acc stmt = trace ("unknown section in set_power_balance: " ++ show stmt) acc
setPowerBalance stmt = preStatement stmt

-------------------------------------------
-- Handler for get_highest_scored_country --
-------------------------------------------
-- | Handler for @get_highest_scored_country@, which runs a scorer over every
-- country and saves the winner in a variable for the script to read back.
getHighestScoredCountry :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
getHighestScoredCountry stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (Just scorer, mvar) ->
            msgToPP (MsgGetHighestScoredCountry scorer
                        (fromMaybe "highest_scored_country" mvar))
        _ -> preStatement stmt
    where
        addLine (s, v) [pdx| scorer = $sc |] = (Just sc, v)
        addLine (s, v) [pdx| var = $vr |] = (s, Just vr)
        addLine (s, v) [pdx| var = ?vr |] = (s, Just vr)
        addLine acc stmt = trace ("unknown section in get_highest_scored_country: " ++ show stmt) acc
getHighestScoredCountry stmt = preStatement stmt

------------------------------------
-- Handler for add_contested_owner --
------------------------------------
-- | Handler for @add_contested_owner@, which puts a second claimant on a state.
-- It is written from either side, so the one name it takes is a country in a
-- state scope and a state in a country scope.
addContestedOwner :: (HOI4Info g, Monad m) => StatementHandler g m
addContestedOwner stmt@[pdx| %_ = !(_ :: Double) |] = withState MsgAddContestedOwnerState stmt
addContestedOwner stmt = withFlag MsgAddContestedOwner stmt


-------------------------------------
-- Handler for transfer_units_fraction --
-------------------------------------
data TransferUnits = TransferUnits
        {   tu_target :: Maybe (Either Text (Text, Text))
        ,   tu_size :: Maybe Double
        ,   tu_army :: Maybe Double
        ,   tu_navy :: Maybe Double
        ,   tu_air :: Maybe Double
        ,   tu_stockpile :: Maybe Double
        ,   tu_keep_leaders :: Bool
        }

newTU :: TransferUnits
newTU = TransferUnits Nothing Nothing Nothing Nothing Nothing Nothing False

-- | Handler for @transfer_units_fraction@, which hands part of the country's
-- forces to another. @size@ sets the share for everything not given a share of
-- its own, so each arm is written out with whichever of the two applies to it.
transferUnitsFraction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
transferUnitsFraction stmt@[pdx| %_ = @scr |] = case tu_target tu of
        Just target -> do
            targetloc <- fromMaybe "" <$> eflag (Just HOI4Country) target
            basemsg <- msgToPP (MsgTransferUnitsFraction targetloc (tu_keep_leaders tu))
            shares <- indentUp (fold <$> traverse share
                [ ("army", tu_army tu), ("navy", tu_navy tu), ("air force", tu_air tu)
                , ("stockpile", tu_stockpile tu) ])
            return (basemsg ++ shares)
        Nothing -> preStatement stmt
    where
        tu = foldl' addLine newTU scr
        addLine t [pdx| target = $vartag:$var |] = t { tu_target = Just (Right (vartag, var)) }
        addLine t [pdx| target = ?tgt |] = t { tu_target = Just (Left tgt) }
        addLine t [pdx| size = !n |] = t { tu_size = Just n }
        addLine t [pdx| army_ratio = !n |] = t { tu_army = Just n }
        addLine t [pdx| navy_ratio = !n |] = t { tu_navy = Just n }
        addLine t [pdx| air_ratio = !n |] = t { tu_air = Just n }
        addLine t [pdx| stockpile_ratio = !n |] = t { tu_stockpile = Just n }
        addLine t [pdx| keep_unit_leaders = yes |] = t { tu_keep_leaders = True }
        addLine t [pdx| keep_unit_leaders = no |] = t { tu_keep_leaders = False }
        -- Which leaders go along, and which organizations they are moved
        -- between; neither says anything about how much is handed over.
        addLine t [pdx| keep_unit_leaders_trigger = %_ |] = t
        addLine t [pdx| source_organization = %_ |] = t
        addLine t [pdx| target_organization = %_ |] = t
        addLine t stmt = trace ("unknown section in transfer_units_fraction: " ++ show stmt) t

        share (what, mratio) = case mratio <|> tu_size tu of
            Just ratio -> msgToPP (MsgTransferUnitsShare what ratio)
            Nothing -> return []
transferUnitsFraction stmt = preStatement stmt

-------------------------------------
-- Handler for add_resistance_target --
-------------------------------------
-- | Handler for @add_resistance_target@, which moves the resistance a state
-- settles at. The tooltip names the reason the game gives the player for it.
addResistanceTarget :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addResistanceTarget stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing) scr of
        (Just amount, mtip, mdays) -> do
            tiploc <- traverse getGameL10n mtip
            msgToPP (MsgAddResistanceTarget amount (fromMaybe 0 mdays) (fromMaybe "" tiploc))
        _ -> preStatement stmt
    where
        addLine (a, t, d) [pdx| amount = !n |] = (Just n, t, d)
        addLine (a, t, d) [pdx| tooltip = $tip |] = (a, Just tip, d)
        addLine (a, t, d) [pdx| days = !n |] = (a, t, Just n)
        -- Whose occupation the target applies to. The state is in scope either
        -- way, so this narrows which occupier rather than saying anything new.
        addLine acc [pdx| occupier = %_ |] = acc
        addLine acc [pdx| occupied = %_ |] = acc
        addLine acc stmt = trace ("unknown section in add_resistance_target: " ++ show stmt) acc
addResistanceTarget stmt@[pdx| %_ = !amount |] = msgToPP (MsgAddResistanceTarget amount 0 "")
addResistanceTarget stmt = preStatement stmt
