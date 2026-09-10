{-|
Module      : HOI4.FocusModules
Description : The wiki's Lua lookup modules for national focuses

The wiki names a focus with @{{Focus|\<tag\>|\<id\>}}@, and the template gets
the focus's name, its icon and the page it is written up on out of a set of Lua
modules, one per expansion: @Module:Focus/gtd@ holds Gotterdammerung's countries,
@Module:Focus/aat@ Arms Against Tyranny's, and so on. This writes those modules
out, so that the lookup table and the pages it points at are made from the same
reading of the game files.

Which expansion a country belongs to is the wiki's own filing and nothing the
game says, so it is kept by hand in "HOI4.WikiTables"; a new expansion is added
there.
-}
module HOI4.FocusModules (
        writeHOI4FocusModules
    ) where

import Control.Monad (forM_, unless)
import Control.Monad.State (gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Function (on)
import Data.List (groupBy, nub, sortOn)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import System.FilePath (takeDirectory, takeFileName, (</>))

import Abstract -- everything
import qualified Doc
import FileIO (Feature (..), writeFeatures)
import ParseWarnings (ParseWarning (..), warnM)
import QQ (pdx)
import SettingsTypes (PPT, Settings (..), IsGameData (..), getGameInterface)

import HOI4.Localization (getGameL10n)
import HOI4.Messages (wikifyLocColours)
import HOI4.NationalFocus (focusIcon, gfxKey)
import HOI4.Types -- everything
import HOI4.WikiTables ( focusPage, focusPageTag, focusModuleOf, focusModuleIds
                       , focusSuffix, focusSuffixes)

-- | One focus as a module writes it down: everything the @{{Focus}}@ template
-- is given about it, and where in the file it goes.
data FocusEntry = FocusEntry
    {   fe_key :: Text -- ^ Its id in lower case, the way the modules key entries
    ,   fe_name :: Text -- ^ Its name, which is also the anchor a link lands on
    ,   fe_icons :: [Text] -- ^ Every image it is drawn with, the usual one first
    ,   fe_suffix :: Maybe Text -- ^ What tells it from a focus of the same name
    ,   fe_continuous :: Bool -- ^ Whether it is a continuous focus
    ,   fe_order :: (FilePath, Int) -- ^ Its file, and where in it it stands
    }

-- | The file a focus was read from, named the way a reader of the module would
-- look for it: the folder it sits in and its own name.
fe_file :: FocusEntry -> Text
fe_file = scriptFile . fst . fe_order

-- | A script file named as the game names it, whichever way the platform we
-- read it on writes a path.
scriptFile :: FilePath -> Text
scriptFile path = T.replace "\\" "/" . T.pack $
    takeFileName (takeDirectory path) </> takeFileName path

-- | Write out one @Module:Focus/\<expansion\>@ per expansion.
writeHOI4FocusModules :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4FocusModules = do
    version <- gets (gameVersion . getSettings)
    focuses <- getNationalFocus
    scripts <- getNationalFocusScripts
    extras <- getExtraScripts

    -- A focus on no page of the wiki has nowhere to be looked up, and a page in
    -- no module has no file to be written in. Both are the hand-kept tables
    -- falling behind the game, and both are reported against the table to add
    -- the missing entry to.
    forM_ (nub [ scriptFile (nf_path nf) | nf <- HM.elems focuses
                                         , isNothing (focusPage focuses nf) ]) $ \file ->
        warnM (MissingEntry "focusPages (HOI4.WikiTables)" file)

    treeEntries <- mapM (\(page, nf) -> (,) page <$> treeEntry nf)
        [ (page, nf) | nf <- HM.elems focuses, Just page <- [focusPage focuses nf] ]
    contEntries <- map ((,) "generic")
        <$> continuousEntries (HM.lookupDefault HM.empty "continuous_focus" extras)
    let entries = treeEntries ++ contEntries

    forM_ (nub [ focusPageTag page | (page, _) <- entries
                                   , isNothing (focusModuleOf (focusPageTag page)) ]) $ \tag ->
        warnM (MissingEntry "focusModules (HOI4.WikiTables)" tag)

    -- An entry in the suffix table for a focus the game no longer has does no
    -- harm, but it means the game has moved on and the table has not.
    let ids = HS.fromList (map (fe_key . snd) entries)
    forM_ (HM.keys focusSuffixes) $ \key ->
        unless (key `HS.member` ids) $
            warnM (StaleEntry "focusSuffixes (HOI4.WikiTables)" key)

    -- Every page, its focuses in the order its file writes them, and the pages
    -- themselves in the order their files were written.
    let pages = sortOn (minimum . map fe_order . snd)
            [ (page, sortOn fe_order es)
            | (page, es) <- HM.toList (HM.fromListWith (++)
                                        [ (page, [e]) | (page, e) <- entries ]) ]
        shares = sharedUses focuses scripts
        mods = [ (modId, inModule, mergesOf modId inModule)
               | modId <- focusModuleIds
               , let inModule = [ pe | pe@(page, _) <- pages
                                     , focusModuleOf (focusPageTag page) == Just modId ]
               , not (null inModule) ]
        -- A tag is looked up as one table, so the pages written under it are
        -- merged into one, and so are the shared trees its country takes its
        -- focuses from -- as long as those are written in the same module,
        -- since a module cannot reach into another one's tables.
        mergesOf modId inModule =
            [ (tag, own ++ shared)
            | tag <- nub [ focusPageTag page | (page, _) <- inModule ]
            , let own = [ page | (page, _) <- inModule, focusPageTag page == tag ]
            , let shared = [ page | (page, _) <- inModule
                                  , (user, used) <- shares
                                  , user == tag, used == page
                                  , focusPageTag page /= tag ]
            ]

    forM_ mods $ \(_, inModule, merges) -> warnAmbiguous inModule merges

    writeFeatures "focus module"
        [ Feature { featurePath = Just "wiki_modules"
                  , featureId = Just ("Module_Focus_" <> modId <> ".lua")
                  , theFeature = Right (ppFocusModule version modId inModule merges) }
        | (modId, inModule, merges) <- mods ]
        return

-- | One focus of a tree, as its module writes it.
treeEntry :: (HOI4Info g, Monad m) => HOI4NationalFocus -> PPT g m FocusEntry
treeEntry nf = do
    usual <- focusIcon nf
    -- The icons a focus shows in place of its usual one are listed behind it,
    -- so that a page can ask for one of them by number.
    variants <- mapM (getGameInterface "goal_unknown" . fst) (nf_icon_variants nf)
    return FocusEntry
        {   fe_key = T.toLower (nf_id nf)
        ,   fe_name = nf_name_loc nf
        ,   fe_icons = nub (usual : variants)
        ,   fe_suffix = focusSuffix (nf_id nf)
        ,   fe_continuous = False
        ,   fe_order = (nf_path nf, nf_ordinal nf)
        }

-- | The continuous focuses, which the game keeps in a folder of their own and
-- the wiki writes up on one page of its own. Only what the module says about a
-- focus is read here -- its id, its icon and the name they get it -- since
-- nothing else about a continuous focus is wanted.
continuousEntries :: (HOI4Info g, Monad m) =>
    HashMap FilePath GenericScript -> PPT g m [FocusEntry]
continuousEntries scripts =
    concat <$> mapM ofFile (sortOn fst (HM.toList scripts))
    where
        ofFile (path, scr) =
            mapM (ofFocus path) (zip [0..] (concatMap palette scr))
        palette [pdx| continuous_focus_palette = @scr |] =
            [ stmt | stmt@[pdx| focus = @_ |] <- scr ]
        palette _ = []
        ofFocus path (ordinal, stmt@[pdx| %_ = @scr |]) = do
            let fid = fromMaybe "(unknown)" (listToMaybe [ i | [pdx| id = $i |] <- scr ])
                icon = fromMaybe ("GFX_focus_" <> fid)
                        (listToMaybe [ gfxKey i | [pdx| icon = $i |] <- scr ])
            name <- wikifyLocColours <$> getGameL10n fid
            image <- getGameInterface "goal_unknown" icon
            return FocusEntry
                {   fe_key = T.toLower fid
                ,   fe_name = name
                ,   fe_icons = [image]
                ,   fe_suffix = focusSuffix fid
                ,   fe_continuous = True
                ,   fe_order = (path, ordinal)
                }
        ofFocus path (ordinal, stmt) = do
            warnM (BadValue "continuous focus" stmt)
            return FocusEntry
                {   fe_key = "(unknown)"
                ,   fe_name = "(unknown)"
                ,   fe_icons = ["goal_unknown"]
                ,   fe_suffix = Nothing
                ,   fe_continuous = True
                ,   fe_order = (path, ordinal)
                }

-- | Which shared tree each country's tree takes focuses from, as the page each
-- is written up on. A tree names the shared branches it opens with, and the
-- rest of a branch hangs off the one it names; the tree's own page is that of
-- the focuses standing in it, which is what tells Spain's two trees apart.
sharedUses :: HashMap Text HOI4NationalFocus -> HashMap String GenericScript
    -> [(Text, Text)]
sharedUses focuses scripts = nub
    [ (tag, used)
    | scr <- HM.elems scripts
    , [pdx| focus_tree = @tree |] <- scr
    , Just tag <- [listToMaybe (mapMaybe (fmap focusPageTag . pageOf) (ownIds tree))]
    , sid <- sharedIds tree
    , Just used <- [pageOf sid]
    , focusPageTag used /= tag
    ]
    where
        pageOf theid = focusPage focuses =<< HM.lookup theid focuses
        ownIds tree = [ i | [pdx| focus = @f |] <- tree, [pdx| id = $i |] <- f ]
        sharedIds tree = [ i | [pdx| shared_focus = $i |] <- tree ]

-- | Report the focuses a link cannot be aimed at. A tag is looked up by name as
-- well as by id, so two of its focuses that share a name and are told apart by
-- nothing leave one of them unreachable and both of them landing on the same
-- anchor. Only the pages written under the tag itself are compared: a shared
-- tree merged into a country's table is written up elsewhere, and the wiki does
-- not disambiguate against it.
warnAmbiguous :: Monad m => [(Text, [FocusEntry])] -> [(Text, [Text])] -> PPT g m ()
warnAmbiguous inModule merges =
    forM_ merges $ \(tag, _) -> do
        let own = concat [ es | (page, es) <- inModule, focusPageTag page == tag ]
            clashing = [ grp | grp <- groupBy ((==) `on` linkName) (sortOn linkName own)
                             , length grp > 1 ]
        forM_ (concat clashing) $ \e ->
            warnM (MissingEntry "focusSuffixes (HOI4.WikiTables)"
                    (fe_key e <> " (" <> fe_name e <> ")"))
    where
        -- What the template makes of an entry: the name it is looked up by and
        -- the anchor it links to, both of which take the suffix.
        linkName e = T.toLower (fe_name e)
            <> maybe "" (\s -> " " <> T.toLower s) (fe_suffix e)

-- | One module, written out.
ppFocusModule :: Text -> Text -> [(Text, [FocusEntry])] -> [(Text, [Text])] -> Doc
ppFocusModule version modId inModule merges = luaDoc $
    [ "-- Module:Focus/" <> modId <> ", written out by pdxparse from Hearts of Iron IV " <> version <> "."
    , "local util = require(\"Module:Util\")"
    , "local utilF = require(\"Module:Focus/util\")"
    , ""
    , "local p = {"
    ] ++
    [ "    " <> tag <> " = {}," | (tag, _) <- merges ] ++
    [ "}"
    , ""
    ] ++
    [ "local " <> page <> " = {}" | (page, _) <- inModule ] ++
    concat
    [ "" : ("--" <> fe_file (head run)) : [ entryLine page e | e <- run ]
    | (page, es) <- inModule, run <- groupBy ((==) `on` fe_file) es ] ++
    [ ""
    , "--Names"
    ] ++
    [ page <> " = utilF.name_as_id(" <> page <> ")" | (page, _) <- inModule ] ++
    [ ""
    , "--Pages"
    ] ++
    [ page <> " = utilF.setPage(" <> page <> ", \"" <> page <> "\")"
    | (page, _) <- inModule ] ++
    [ ""
    , "--Merge"
    ] ++
    [ "p." <> tag <> " = " <> case pages of
        [one] -> one
        many -> "util.table.merge({" <> T.intercalate ", " many <> "})"
    | (tag, pages) <- merges ] ++
    [ ""
    , "return p"
    ]

-- | One focus, written into the table its page is read out of.
entryLine :: Text -> FocusEntry -> Text
entryLine page e = T.concat
    [ page, "['", fe_key e, "'] = { name = ", luaString (fe_name e)
    , ", icon = ", icons
    , maybe "" (\s -> ", suffix = " <> luaString s) (fe_suffix e)
    , if fe_continuous e then ", continuous = true" else ""
    , " }"
    ]
    where
        -- A focus drawn with the one image names it; one that changes what it
        -- is drawn with lists every image, the one it usually shows first,
        -- which is the one the template falls back on.
        icons = case fe_icons e of
            [one] -> luaString one
            many -> "{ " <> T.intercalate ", " (map luaString many) <> " }"

luaString :: Text -> Text
luaString txt = "\"" <> T.concatMap esc txt <> "\""
    where
        esc '\\' = "\\\\"
        esc '"' = "\\\""
        esc c = T.singleton c

luaDoc :: [Text] -> Doc
luaDoc ls = mconcat [ Doc.strictText l <> PP.line | l <- ls ]
