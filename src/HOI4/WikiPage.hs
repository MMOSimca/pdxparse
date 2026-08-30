{-|
Module      : HOI4.WikiPage
Description : Shared pieces of the consolidated wiki pages

Events and decisions are each written out twice: once per feature, and once
per script file as a whole page ready to be put on the wiki. This module holds
what those whole-page writings have in common -- the opening lines that say
what the page holds, and the wrapper that makes a run of feature boxes flow
side by side under one heading.
-}
module HOI4.WikiPage (
        CountryIndex
    ,   buildCountryIndex
    ,   ppPageIntro
    ,   ppSectionHeader
    ,   boxWrapper
    ,   pageCountry
    ,   pageExpansion
    ,   requiresDebug
    ) where

import Control.Monad (forM)
import Control.Monad.State (gets)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath (takeBaseName)

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract (GenericScript)
import qualified Doc
import HOI4.CountryNames (casualName, casualNameTag, looselyNamed)
import HOI4.Localization (flagText)
import HOI4.Messages (wikifyLocColours)
import HOI4.Types
import HOI4.WikiTables (expansionOfPrefix)
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), IsGame (..), IsGameData (..)
                     , getGameL10nIfPresent)

-- | The name each country's script files are likely to be named after, to the
-- tag that country goes by. Built once and carried into every page, since
-- working it out means a pass over every country there is.
type CountryIndex = HashMap Text Text

-- | Index the countries by the names their script files might use. The wiki's
-- own names are already indexed by 'casualNameTag', which is tried first, so
-- these only fill in the plain names it doesn't use -- @GER@ is \"Germany\" to
-- a file name where the wiki calls it the German Reich.
buildCountryIndex :: (HOI4Info g, Monad m) => PPT g m CountryIndex
buildCountryIndex = do
    tags <- HM.keys <$> getCountryHistory
    paths <- HM.keys <$> getCountryHistoryScripts
    named <- forM tags $ \tag ->
        fmap ((, tag) . looselyNamed) <$> getGameL10nIfPresent tag
    -- Each country's history file is named for that one country, so those
    -- names tell apart countries the game's own names can't.
    let byHistoryFile = onlyOneEach (mapMaybe historyFileCountry paths)
    -- Where the game does name several countries alike -- Germany, West
    -- Germany and East Germany are each just "Germany" -- there is no telling
    -- which a file means, so such a name names none of them.
        byName = onlyOneEach (catMaybes named)
    return $ HM.union byHistoryFile byName

-- | The country a history file is named after. They are named
-- @\<TAG\> - \<Name\>.txt@.
historyFileCountry :: FilePath -> Maybe (Text, Text)
historyFileCountry path = case T.splitOn " - " (T.pack (takeBaseName path)) of
    [tag, name] | T.length tag == 3 -> Just (looselyNamed name, T.toUpper tag)
    _ -> Nothing

-- | Index by name, keeping only the names just one country answers to.
onlyOneEach :: [(Text, Text)] -> CountryIndex
onlyOneEach entries = HM.mapMaybe onlyOne $
        HM.fromListWith (++) [(name, [tag]) | (name, tag) <- entries]
    where
        onlyOne [tag] = Just tag
        onlyOne _ = Nothing

-- | The opening of a consolidated page: the version it was written from, the
-- expansion its content belongs to where its file names one, and a line
-- saying whose features these are and which script file they come from.
ppPageIntro :: (HOI4Info g, Monad m) =>
    CountryIndex -- ^ The countries, by the names their files go by
    -> Text      -- ^ What the page lists, as the wiki links it (e.g. @events@)
    -> FilePath  -- ^ Path of the script file the page is written from
    -> PPT g m Doc
ppPageIntro countries featureName srcPath = do
    version <- gets (gameVersion . getSettings)
    let base = T.pack (takeBaseName srcPath)
    mflag <- traverse (flagText (Just HOI4Country)) (pageCountry countries base)
    return . mconcat $
        ["{{Version|", Doc.strictText version, "}}", PP.line] ++
        maybe []
              (\expac -> ["{{Expansion|", Doc.strictText expac, "|small=no}}", PP.line])
              (pageExpansion base) ++
        ["This is a list of "] ++
        maybe [] (\flag -> [Doc.strictText flag, "'s "]) mflag ++
        ["[[", Doc.strictText featureName, "]] (from {{path|"
        ,Doc.strictText (wikiPath srcPath), "}})."] ++
        [PP.line]

-- | A wiki section heading, with the id it was made from in a comment beneath
-- it so a reader can find the script the section came from.
ppSectionHeader :: Text -> Maybe Text -> Doc
ppSectionHeader title mid = mconcat $
    ["== ", Doc.strictText (Doc.nl2br (wikifyLocColours title)), " =="] ++
    maybe [] (\cid -> [PP.line, "<!-- ", Doc.strictText cid, " -->"]) mid

-- | Wrap a run of feature boxes so the wiki lays them out side by side,
-- filling the width under their heading, instead of each starting a new row.
boxWrapper :: [Doc] -> Doc
boxWrapper docs = mconcat $
    ["{{Box wrapper}}", PP.line]
    ++ intersperse PP.line docs
    ++ ["{{End box wrapper}}", PP.line]

-- | The country a script file is about, worked out from its name: either the
-- tag itself (@CZE.txt@) or the name the wiki knows the country by, with any
-- expansion prefix dropped first (@MUN_Czechoslovakia.txt@). Files not named
-- after a country -- @Baltic.txt@, @PoliticalEvents.txt@ -- have none.
pageCountry :: CountryIndex -> Text -> Maybe Text
pageCountry countries base =
    let name = fromMaybe base (dropExpansionPrefix base)
        tag = T.toUpper name
    in if isJust (casualName tag)
        then Just tag
        else case casualNameTag name of
            Just found -> Just found
            Nothing -> HM.lookup (looselyNamed name) countries

-- | The expansion a script file's content belongs to, from the prefix its
-- name starts with.
pageExpansion :: Text -> Maybe Text
pageExpansion base = expansionOfPrefix . fst $ T.breakOn "_" base

-- | Drop a leading expansion prefix from a file name, where it has one.
dropExpansionPrefix :: Text -> Maybe Text
dropExpansionPrefix base =
    let (prefix, rest) = T.breakOn "_" base
    in if isJust (expansionOfPrefix prefix) && not (T.null rest)
        then Just (T.drop 1 rest)
        else Nothing

-- | A path as the wiki writes one, whatever this platform's separator is.
wikiPath :: FilePath -> Text
wikiPath = T.replace "\\" "/" . T.pack

-- | Whether a trigger script requires the game to be running in debug mode.
-- Features gated this way exist for testing and are never seen in a normal
-- game, so the consolidated pages leave them out. Only a requirement that
-- always holds counts: an @is_debug@ under an @OR@ or a @NOT@ doesn't make
-- the feature debug-only, so only blocks that keep AND semantics are looked
-- into.
requiresDebug :: Maybe GenericScript -> Bool
requiresDebug = maybe False (any requiring)
    where
        requiring [pdx| $lhs = yes |] = T.toLower lhs == "is_debug"
        requiring [pdx| $lhs = @scr |]
            | T.toLower lhs `elem` ["and", "hidden_trigger"] = any requiring scr
        requiring _ = False
