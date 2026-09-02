{-|
Module      : ParseWarnings
Description : Standard warnings and errors for unrecognized script structures

Every feature parser runs into the same situations: a whole category of
scripts fails to parse, one statement in a file is rejected, a section key
inside a structure is not known, or a known key carries a value of an
unexpected shape. This module gives each of those one warning type and one
wording, so every parser reports them the same way and always says what it
was parsing, where, and what it saw.
-}
module ParseWarnings (
        -- * Warning kinds
        ParseWarning (..)
    ,   warn
    ,   warnM
        -- * Rejecting statements
    ,   rejectForm
    ,   onTopLevelCompound
        -- * Parsing a category of script files
    ,   parseScriptFiles
    ,   flattened
    ,   keyedBy
    ) where

import Control.Arrow ((&&&))
import Control.Monad (forM)
import Control.Monad.Except (ExceptT, MonadError (..))

import Data.Maybe (catMaybes)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace (trace, traceM)

import Abstract -- everything
import QQ (pdx)
import SettingsTypes ( PPT
                     , IsGameState (..), GameState (..)
                     , setCurrentFile, withCurrentFile
                     , hoistExceptions)

-- | The discrete kinds of "did not understand this" report. Each carries what
-- was being parsed and what was actually seen, so the message is always
-- specific enough to find the offending script.
data ParseWarning
    = TotalFailure Text Text
        -- ^ Nothing in this category could be parsed: what, error.
    | ErrorInFile Text FilePath Text
        -- ^ One file's statement was rejected: what, file, error.
    | UnknownSection Text GenericStatement
        -- ^ A statement inside a structure had an unrecognized key or shape:
        -- what structure, offending statement.
    | BadValue Text GenericStatement
        -- ^ A known section carried a value of an unexpected shape:
        -- which section (e.g. @\"idea picture\"@), offending statement.
    | StaleEntry Text Text
        -- ^ A hand-kept table names something the game no longer defines:
        -- which table, the entry. The entry does no harm on its own -- a
        -- lookup for it simply never fires -- but it means the game moved
        -- and the table has not.

renderWarning :: ParseWarning -> String
renderWarning (TotalFailure what err) =
    "Completely failed parsing " ++ T.unpack what ++ ": " ++ T.unpack err
renderWarning (ErrorInFile what file err) =
    "Error parsing " ++ T.unpack what ++ " in " ++ file ++ ": " ++ T.unpack err
renderWarning (UnknownSection what stmt) =
    "Unknown section in " ++ T.unpack what ++ ": " ++ show stmt
renderWarning (BadValue what stmt) =
    "Bad value for " ++ T.unpack what ++ ": " ++ show stmt
renderWarning (StaleEntry what entry) =
    "Stale entry in " ++ T.unpack what ++ ": " ++ T.unpack entry

-- | Report a warning from pure code, passing the given value through.
warn :: ParseWarning -> a -> a
warn = trace . renderWarning

-- | Report a warning from monadic code.
warnM :: Monad m => ParseWarning -> m ()
warnM = traceM . renderWarning

-- | Reject a top-level statement whose form was not recognized at all. The
-- error names what was being parsed, the file, and the statement itself,
-- since it surfaces from parsers that abort a whole file's traversal and
-- would otherwise leave no trace of where the trouble was.
rejectForm :: (IsGameState (GameState g), MonadError Text m) =>
    Text -> GenericStatement -> PPT g m a
rejectForm what stmt = withCurrentFile $ \file ->
    throwError $ "unrecognized form for " <> what
        <> " in " <> T.pack file
        <> ": " <> T.pack (show stmt)

-- | Dispatch the standard @<id> = { <sections> }@ top-level form. The handler
-- gets the id and the sections; a variable interpolation (@\@lhs@) is skipped
-- quietly, and every other shape is rejected with the standard error.
onTopLevelCompound :: (IsGameState (GameState g), MonadError Text m) =>
    Text
    -> (Text -> GenericScript -> PPT g m (Either Text (Maybe a)))
    -> GenericStatement
    -> PPT g m (Either Text (Maybe a))
onTopLevelCompound what handler stmt@[pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        GenericLhs name [] -> handler name parts
        AtLhs _ -> return (Right Nothing)
        _ -> rejectForm what stmt
    _ -> rejectForm what stmt
onTopLevelCompound what _ stmt = rejectForm what stmt

-- | Parse one category of script files, reporting every failure through the
-- standard warnings and keeping whatever parsed. The per-file parser gets the
-- file's statements with the current file set, and says per statement whether
-- it parsed something, skipped it, or rejects it (a thrown error rejects the
-- whole category). Results are grouped by source file; see 'flattened' and
-- 'keyedBy' for the common ways of putting them together.
parseScriptFiles :: (IsGameState (GameState g), Monad m) =>
    Text
    -> (GenericScript -> PPT g (ExceptT Text m) [Either Text (Maybe a)])
    -> HashMap String GenericScript
    -> PPT g m (HashMap FilePath [a])
parseScriptFiles what parseFile scripts = do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr -> setCurrentFile sourceFile (parseFile scr))
            scripts
    case tryParse of
        Left err -> do
            warnM (TotalFailure what err)
            return HM.empty
        Right filesOrErrors ->
            flip HM.traverseWithKey filesOrErrors $ \sourceFile results ->
                fmap catMaybes . forM results $ \case
                    Left err -> Nothing <$ warnM (ErrorInFile what sourceFile err)
                    Right mx -> return mx

-- | All parsed items of a category, regardless of file.
flattened :: HashMap FilePath [a] -> [a]
flattened = concat . HM.elems

-- | All parsed items of a category, keyed by name.
keyedBy :: (a -> Text) -> HashMap FilePath [a] -> HashMap Text a
keyedBy key = HM.fromList . map (key &&& id) . flattened
