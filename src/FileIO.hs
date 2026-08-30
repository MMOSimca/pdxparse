{-|
Module      : FileIO
Description : High level I/O for Clausewitz scripts
-}
module FileIO (
        buildPath
    ,   readScript
    ,   readPathScript
    ,   Feature (..)
    ,   Consolidation (..)
    ,   ConsolidatedFeature (..)
    ,   ConsolidationRender
    ,   naturalOrder
    ,   writeFeatures
    ,   writeFeaturesWith
    ) where

import Debug.Trace (trace)

import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.State (gets)
import Control.Exception (try)

import qualified Data.ByteString as B

import Data.Char (isSpace, isDigit)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.Encoding as TE

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory, takeFileName, takeBaseName, dropDrive)
import System.IO (withFile, IOMode (..), hPutStrLn, stderr)

import qualified Data.Attoparsec.Text as Ap
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import SettingsTypes (Settings (..), IsGameData (..), GameData (..), PPT, hoistExceptions)
import Data.List (intercalate, intersperse, sortBy)

-- | Read a file as Text. Unfortunately script files use several incompatible
-- encodings. Try the following encodings in order:
--
-- 1. UTF-8
-- 2. ISO 8859-1
--
-- (Decoding as 8859-1 can't fail, but I don't know if it will always be correct.)
readFileRetry :: FilePath -> IO Text
readFileRetry path = do
    raw <- B.readFile path
    -- Catching exceptions in pure code is a rather convoluted process...
    e <- try (let e = TE.decodeUtf8 raw in e `seq` return e)
    case (e::Either UnicodeException Text) of
        Right result -> return result
        Left _ -> return $ TE.decodeLatin1 raw

-- | Given a path under the game's root directory, build a fully qualified path
-- referring to that file.
--
-- For example, if we're parsing HOI4 on Windows with the usual install
-- location:
--
-- @
--  buildPath settings "events/Baltic.txt" = "C:\Program Files (x86)\Steam\steamapps\common\Hearts of Iron IV\events\Baltic.txt"
-- @
buildPath :: Settings -> FilePath -> FilePath
buildPath settings path =
    if gameOrModFolder settings == gameFolder settings then
        gamePath settings </> path
    else
        gameModPath settings </> path

-------------------------------
-- Reading scripts from file --
-------------------------------

-- | Read and parse a script file. On error, report to standard error and
-- return an empty script.
readScript :: Settings -> FilePath -> IO GenericScript
readScript settings file = do
    let filepath = buildPath settings file
    readPathScript filepath
readPathScript :: FilePath -> IO GenericScript
readPathScript filepath = do
    contents <- readFileRetry filepath
    case runparserAndAddClosingCurlyBrackets filepath contents of
        -- this case probably can't happen with our parser
        Ap.Fail _ context err -> do
            hPutStrLn stderr $ "Couldn't parse " ++ filepath ++ ": " ++ err
            hPutStrLn stderr $ "Context: " ++ intercalate ", " context
            return []
        Ap.Partial _ -> do
            hPutStrLn stderr $ "Got partial from file: " ++ filepath ++ " This should not happen"
            return []
        -- no result, but also not leftovers
        Ap.Done "" [] -> do
            hPutStrLn stderr $ "The file " ++ filepath ++ " seems to only contain whitespace and comments"
            return []
        Ap.Done "" result -> return result
        Ap.Done leftover result -> do
            hPutStrLn stderr $ "Warning: The file \"" ++ filepath ++ "\" was not fully parsed. The following lines were discarded:"
            hPutStrLn stderr $ T.unpack leftover
            return result
    where
        runparser contents = case Ap.parse
                (Ap.option undefined (Ap.char '\xFEFF') *> skipSpace
                   *> genericScript
                   -- discard extra whitespace and comments at the end of the file. This is needed so that
                   -- a successful parse has no leftovers
                   <* skipSpace
                   ) contents of
            -- pass an empty string to the partial to tell it that there is no more input
            Ap.Partial f -> f ""
            -- just pass on all other cases
            x -> x

        runparserAndAddClosingCurlyBrackets filepath contents = case runparser contents of
            -- no leftovers means that the parsing was successful (result could be empty in case of a file which just has whitespace and comments)
            Ap.Done "" result -> Ap.Done "" result
            -- if the leftover is nothing but extra closing brackets (and whitespace),
            -- the file parsed fine apart from surplus } at the end; ignore them
            Ap.Done originalLeftover originalResult
                | T.all (\c -> isSpace c || c == '}') originalLeftover ->
                    trace ( "File " ++ filepath ++ ": Extra closing curly bracket(s) at end, ignored" )
                        Ap.Done "" originalResult
            -- we have some leftover, so the file was either not parsed at all or just partially parsed
            -- so we try again with an extra closing }
            Ap.Done originalLeftover originalResult -> case runparser (contents<>"}") of
                -- good, it worked now and we can return the new result
                Ap.Done "" newResult -> do
                    trace ( "File " ++ filepath ++ ": Missing closing curly bracket, applied fix" )
                        Ap.Done "" newResult
                -- still didn't work, so we try with two }
                Ap.Done _newLeftover _newResult -> case runparser (contents<>"}}") of
                    -- good, it worked now and we can return the new result
                    Ap.Done "" newResult -> do
                        trace ("File " ++ filepath ++ ": Missing 2 closing curly brackets, applied fix")
                            Ap.Done "" newResult
                    -- to not make matters worse, we return the original results if adding } didn't result in a successful parse
                    _ -> Ap.Done originalLeftover originalResult
                _ -> Ap.Done originalLeftover originalResult
            -- just pass on all other cases
            x -> x

------------------------------
-- Writing features to file --
------------------------------

-- | An individual game feature. For example, a value for this exists for each
-- event, one for each national focus, one for each decision, etc.
--
-- The parameter is a type containing data relevant to that feature, or an
-- error message from processing.
data Feature a = Feature {
        featureId :: Maybe Text
    ,   featurePath :: Maybe FilePath
    ,   theFeature :: Either Text a
    } deriving (Show)

-- TODO: allow writing to a different output directory
-- | Write a parsed and presented feature to the given file under the directory
-- @./output@. If the filename includes directories, create them first.
writeFeature :: FilePath -> String -> Doc -> IO ()
writeFeature path gamefold output = do
    let destinationGame = "output" </> gamefold
        destinationFile = destinationGame </> dropDrive path
        destinationDir  = takeDirectory destinationFile
    createDirectoryIfMissing True (takeDirectory destinationGame)
    createDirectoryIfMissing True destinationDir
    withFile destinationFile WriteMode $ \h -> do
        result <- try $
            PP.displayIO h (PP.renderPretty 0.9 80 output)
        case result of
            Right () -> return ()
            Left err -> hPutStrLn stderr $
                "Error writing " ++ show (err::IOError)

-- | A feature as it goes into a consolidated file.
data ConsolidatedFeature a = ConsolidatedFeature
    {   cfDir :: FilePath -- ^ Directory the feature's own file was written to
    ,   cfId :: Text      -- ^ Feature id, as its own file was named
    ,   cfFeature :: a    -- ^ The feature itself
    ,   cfDoc :: Doc      -- ^ Its rendering
    }

-- | Build the body of one consolidated file, given the directory it will be
-- written to and every feature belonging in it, in no particular order.
-- 'Nothing' means the file has nothing to say -- e.g. everything in it is
-- debug-only -- and is not written at all.
type ConsolidationRender g m a =
    FilePath -> [ConsolidatedFeature a] -> PPT g (ExceptT Text m) (Maybe Doc)

-- | How 'writeFeaturesWith' should build consolidated @_all.txt@ files. The
-- constructor says where each one goes; the function it carries decides
-- everything about what goes in it.
data Consolidation g m a
    = ConsolidateHere (ConsolidationRender g m a)
        -- ^ One file per feature folder, named after that folder.
    | ConsolidateInParent (ConsolidationRender g m a)
        -- ^ One file in the parent of each feature folder, named after the
        -- parent, holding the features of all of its subfolders. For features
        -- written into a subfolder per group -- decisions, in a folder per
        -- category -- this puts the whole script file on one page, with the
        -- subfolders left to become its sections.

-- | Given a list of features, present them and output to the appropriate files
-- under the directory @./output@.
writeFeatures :: (IsGameData (GameData g), MonadIO m) =>
    Text -- ^ Name of feature (e.g. "idea groups")
        -> [Feature a]
        -> (a -> PPT g (ExceptT Text m) Doc) -- ^ Rendering function
        -- PPT g (ExceptT Text IO) = StateT Settings (ReaderT GameState (ExceptT Text IO))
        -> PPT g m ()
writeFeatures featureName features pprint =
    writeFeaturesWith featureName features pprint Nothing

-- | Like 'writeFeatures', but when given a 'Consolidation', also write
-- consolidated @\<dirname\>_all.txt@ files holding every feature of a
-- directory sorted by id, with wiki headers so the page gets a table of
-- contents when put on the wiki.
writeFeaturesWith :: (IsGameData (GameData g), MonadIO m) =>
    Text -- ^ Name of feature (e.g. "idea groups")
        -> [Feature a]
        -> (a -> PPT g (ExceptT Text m) Doc) -- ^ Rendering function
        -> Maybe (Consolidation g m a) -- ^ Whether and how to consolidate
        -> PPT g m ()
writeFeaturesWith featureName features pprint mconsolidate = do
    gamefoldr <- gets (gameOrModFolder . getSettings)
    efeatures_pathed_pp'd <- forM features $ \feature ->
        case theFeature feature of
            Left err ->
                -- Error was passed to us - report it
                return (feature {
                        theFeature = Left $ "Error while parsing" <> featureName <> ":" <> err
                    }, Nothing)
            Right thing -> case (featurePath feature, featureId feature) of
                (Just _, Just _) -> do
                    doc <- hoistExceptions (pprint thing)
                    return (feature { theFeature = Right doc }, Just thing)
                (Nothing, Nothing) -> return (feature {
                        theFeature = Left $ "Error while writing " <> featureName
                                        <> ": missing path and id"
                    }, Nothing)
                (Nothing, Just oid) -> return (feature {
                        theFeature = Left $ "Error while writing " <> featureName
                                        <> " " <> oid <> ": missing path"
                    }, Nothing)
                (Just path, Nothing) -> return (feature {
                        theFeature = Left $ "Error while writing " <> featureName
                                        <> " " <> T.pack path <> ": missing id"
                    }, Nothing)
    liftIO $ forM_ (map fst efeatures_pathed_pp'd) $ \feature -> case theFeature feature of
        Right (Right output) -> case (featurePath feature, featureId feature) of
            (Just sourcePath, Just feature_id) ->
                writeFeature (sourcePath </> T.unpack feature_id) gamefoldr output
            (Just sourcePath, Nothing) -> liftIO . TIO.putStrLn $
                "Error while writing " <> featureName <> " in " <> T.pack sourcePath <> ": missing id"
            (Nothing, Just fid) -> liftIO . TIO.putStrLn $
                "Error while writing " <> featureName <> " " <> fid <> ": missing source path"
            (Nothing, Nothing) -> liftIO . TIO.putStrLn $
                "Error while writing " <> featureName <> ": missing source path and id"
        e -> TIO.putStrLn (eitherError e) where
            eitherError (Right (Right _)) = error "impossible: fall through in writeFeatures"
            eitherError (Right (Left err)) = err
            eitherError (Left err) = err
    -- Consolidated files.
    let rendered = [ ConsolidatedFeature path fid thing output
                   | (feature, Just thing) <- efeatures_pathed_pp'd
                   , Right (Right output) <- [theFeature feature]
                   , Just path <- [featurePath feature]
                   , Just fid <- [featureId feature]
                   ]
    case mconsolidate of
        Nothing -> return ()
        Just consolidation -> do
            let (dirOf, render) = case consolidation of
                    ConsolidateHere r -> (id, r)
                    ConsolidateInParent r -> (takeDirectory, r)
                entries = HM.fromListWith (++)
                    [(dirOf (cfDir cf), [cf]) | cf <- rendered]
            forM_ (HM.toList entries) $ \(dir, cfs) -> do
                -- The body is built here rather than in IO because it needs
                -- game data: localization, the version, and the like.
                ebody <- hoistExceptions (render dir cfs)
                case ebody of
                    Left err -> liftIO . TIO.putStrLn $
                        "Error while writing consolidated " <> featureName
                            <> " in " <> T.pack dir <> ": " <> err
                    Right Nothing -> return ()
                    Right (Just body) -> liftIO $
                        writeFeature (dir </> aggFileName dir) gamefoldr body

-- | Name of the consolidated file for an output directory.
aggFileName :: FilePath -> FilePath
aggFileName path =
    let base = takeBaseName path
    in (if null base then "all" else base ++ "_all") ++ ".txt"

-- | Compare two feature ids so that runs of digits sort by value:
-- "germany.2" before "germany.10".
naturalOrder :: Text -> Text -> Ordering
naturalOrder a b = compare (key a) (key b)
    where
        key :: Text -> [Either Integer Text]
        key = map toChunk . T.groupBy (\x y -> isDigit x == isDigit y) . T.toLower
        toChunk g
            | T.all isDigit g = Left (read (T.unpack g))
            | otherwise = Right g
