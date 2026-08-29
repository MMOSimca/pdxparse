{-|
Module      : Localization
Description : Load localization files
-}
module Localization (
        readL10n -- :: Settings -> IO L10n
    ,   L10n -- re-exported from Yaml
    ) where

import Control.Monad (filterM, forM)

import Data.List (isInfixOf, foldl')
import qualified Data.HashMap.Strict.InsOrd as HMO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory (doesFileExist, getDirectoryContents, doesDirectoryExist)
import System.Directory.Recursive (getSubdirsRecursive)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import SettingsTypes (Settings (..), concatMapM)
import Yaml (L10n, L10nO, parseLocFile, mergeLangs)

-- | Read and parse localization files for the current game.
--
-- The files are in a quasi-YAML format, one file per language:
--
-- >  l_language: (e.g. l_english)
-- >   KEY:0 "Content with "unescaped" quotation marks"
--
-- where the 0 is a version number. We discard languages we don't care about.
readL10n :: Settings -> IO (L10n, L10nO)
readL10n settings = do
    let dir           = gamePath settings
                        </> languageFolder settings
        dirmod        = gameModPath settings
                        </> languageFolder settings
        dirifYAMLmod  = gameModPath settings
                        </> "localisation"
                        </> "replace"
                        </> justLanguage settings
        dirifYAMLmodr  = gameModPath settings
                        </> "localisation"
                        </> "replace"
    modexist <- doesDirectoryExist dirmod
    dirmod' <- if modexist then do
        dirmodsub <- getSubdirsRecursive dirmod
        return $ dirmod : dirmodsub else return []
    replaceexist <- doesDirectoryExist dirifYAMLmod
    replaceexistr <- doesDirectoryExist dirifYAMLmodr
    let dirifYAMLmod'
          | replaceexist = [dirifYAMLmod]
          | replaceexistr = [dirifYAMLmodr]
          | otherwise = []
        dirs = dirifYAMLmod' ++ dirmod'++ [dir]

    files <- concatMapM (readL10nDirs settings) dirs
    hmolist <- forM files $ \file -> do
        parseResult <- parseLocFile <$> TIO.readFile file
        case parseResult of
            Left exc -> do
                hPutStrLn stderr $ "Parsing localisation file " ++ file ++ " failed: " ++ show exc
                return HMO.empty
            Right contents -> return contents
    let merged = foldl' mergeLangs HMO.empty hmolist
    return (HMO.toHashMap (HMO.map HMO.toHashMap merged), merged)

readL10nDirs :: Settings -> FilePath -> IO [FilePath]
readL10nDirs settings dirs = filterM doesFileExist
                . map (dirs </>)
                . filter (T.unpack (language settings) `isInfixOf`)
                    =<< getDirectoryContents dirs
