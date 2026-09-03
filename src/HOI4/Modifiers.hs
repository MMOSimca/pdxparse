{-|
Module      : HOI4.Modifiers
Description : Country, ruler, dynamic and opinion modifiers
-}
module HOI4.Modifiers (
--        parseHOI4Modifiers, writeHOI4Modifiers,
        parseHOI4OpinionModifiers, writeHOI4OpinionModifiers, writeHOI4OpinionModifiers'
    ,   parseHOI4DynamicModifiers, writeHOI4DynamicModifiers
    ,   parseHOI4Modifiers
    ) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.State (gets)

import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.List ( sortOn, foldl', intercalate )
import Data.Set (toList, fromList)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..){-, Game (..)-}
                     {-, IsGame (..)-}, IsGameData (..), IsGameState (..), GameState (..)
                     , withCurrentFile, withCurrentIndent
                     , hoistErrors
                     , getGameInterfaceNamed)
import ParseWarnings
import HOI4.Types -- everything
import HOI4.Localization
import HOI4.Common (ppMany, HOI4OpinionModifier (HOI4OpinionModifier))
import FileIO (Feature (..), writeFeatures)
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Doc
import HOI4.Messages
import MessageTools

import HOI4.Handlers.Modifiers (modifierMSG)
import System.FilePath (takeBaseName)

parseHOI4OpinionModifiers :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4OpinionModifier)
parseHOI4OpinionModifiers scripts = keyedBy omodName <$>
    parseScriptFiles "opinion modifiers"
        (\scr -> mapM parseHOI4OpinionModifier (unwrapOpinionModifiers scr))
        scripts

-- | The statements of an opinion modifiers file, with the customary
-- @opinion_modifiers = { ... }@ wrapper taken off.
unwrapOpinionModifiers :: GenericScript -> GenericScript
unwrapOpinionModifiers scr = concatMap (\case
    [pdx| opinion_modifiers = @mods |] -> mods
    _ -> scr)
    scr

newHOI4OpinionModifier :: Text -> Maybe Text -> FilePath -> HOI4OpinionModifier
newHOI4OpinionModifier id locid path = HOI4OpinionModifier id locid path Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Parse a statement in an opinion modifiers file. Some statements aren't
-- modifiers; for those, and for any obvious errors, return Right Nothing.
parseHOI4OpinionModifier :: (HOI4Info g, MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4OpinionModifier))
parseHOI4OpinionModifier = onTopLevelCompound "opinion modifier" $ \id parts ->
    withCurrentFile $ \file -> do
        locid <- getGameL10nIfPresent id
        hoistErrors $ foldM opinionModifierAddSection
                            (Just (newHOI4OpinionModifier id locid file))
                            parts

-- | Interpret one section of an opinion modifier. If understood, add it to the
-- event data. If not understood, throw an exception.
opinionModifierAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4OpinionModifier -> GenericStatement -> PPT g m (Maybe HOI4OpinionModifier)
opinionModifierAddSection Nothing _ = return Nothing
opinionModifierAddSection mmod stmt
    = sequence (opinionModifierAddSection' <$> mmod <*> pure stmt)
    where
        opinionModifierAddSection' mod stmt@[pdx| value = !rhs |]
            = return (mod { omodValue = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| max_trust = !rhs |]
            = return (mod { omodMax = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| min_trust = !rhs |]
            = return (mod { omodMin = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| decay = !rhs |]
            = return (mod { omodDecay = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| days = !rhs |]
            = return (mod { omodDays = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| months = !rhs |]
            = return (mod { omodMonths = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| years = !rhs |]
            = return (mod { omodYears = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| trade = %rhs |] = case rhs of
            GenericRhs "yes" [] -> return mod { omodTrade = Just True }
            -- no is the default, so I don't think this is ever used
            GenericRhs "no" [] -> return mod { omodTrade = Just False }
            _ -> throwError "bad trade opinion"
        opinionModifierAddSection' mod stmt@[pdx| target = %rhs |] = case rhs of
            GenericRhs "yes" [] -> return mod { omodTarget = Just True }
            -- no is the default, so I don't think this is ever used
            GenericRhs "no" [] -> return mod { omodTarget = Just False }
            _ -> throwError "bad target opinion"
        opinionModifierAddSection' mod stmt
            = warn (UnknownSection "opinion modifier" stmt) $ return mod

writeHOI4OpinionModifiers :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4OpinionModifiers = do
    opinionModifiers <- getOpinionModifiers
    writeFeatures "opinion_modifiers"
                  [Feature { featurePath = Just "modules"
                           , featureId = Just "opinion_list.lua"
                           , theFeature = Right (HM.elems opinionModifiers)
                           }]
                  ppOpinionModifiers

writeHOI4OpinionModifiers' :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4OpinionModifiers' = do
    opinionModifiers <- getOpinionModifierScripts
    pathOmod <- parseHOI4NationalFocusesPath opinionModifiers
    let pathedOmods :: [Feature [HOI4OpinionModifier]]
        pathedOmods = map (\omods -> Feature {
                                    featurePath = Just "modules"
                                ,   featureId = Just (T.pack $ takeBaseName $ omodPath $ head omods) <> Just ".lua"
                                ,   theFeature = Right omods })
                            (HM.elems pathOmod)
    writeFeatures "opinion_modifiers"
                  pathedOmods
                  ppOpinionModifiers

parseHOI4NationalFocusesPath :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap FilePath [HOI4OpinionModifier])
parseHOI4NationalFocusesPath scripts = HM.filter (not . null) <$>
    parseScriptFiles "opinion modifiers"
        (\scr -> mapM parseHOI4OpinionModifier (unwrapOpinionModifiers scr))
        scripts

-- | The data behind [[Template:Opinion]]. The template itself is now a stub
-- calling [[Module:Opinion]], which does all the wording and colouring and
-- takes its numbers from [[Module:Opinion/List]] -- the table written here.
-- The module looks a modifier up by the lowercased id it is handed, so the keys
-- are lowercased to match.
ppOpinionModifiers :: (HOI4Info g, Monad m) => [HOI4OpinionModifier] -> PPT g m Doc
ppOpinionModifiers modifiers = do
    version <- gets (gameVersion . getSettings)
    modifiers_pp'd <- mapM ppOpinionModifier (sortOn (T.toCaseFold . omodName) modifiers)
    return . mconcat $
        ["--[[", PP.line
        ,"This module compiles the lists of all opinion modifiers.", PP.line
        ,"It's meant to be required by \"Module:Opinion\"", PP.line
        , PP.line
        ,"Automatically generated for ", Doc.strictText version
        ," from the file(s): "
        , Doc.strictText $ T.pack $ intercalate ", " ((toList . fromList) (map omodPath modifiers))
        , PP.line
        ,"--]]", PP.line
        , PP.line
        ,"local p = {", PP.line]
        ++ modifiers_pp'd ++
        ["}", PP.line
        , PP.line
        ,"return p", PP.line
        ]

ppOpinionModifier :: (HOI4Info g, Monad m) => HOI4OpinionModifier -> PPT g m Doc
ppOpinionModifier mod = do
    locName <- wikifyLocColours <$> getGameL10n (omodName mod)
    return . mconcat $
        [ " "
        , Doc.strictText $ T.toLower (omodName mod)
        , " = { loc = \""
        , Doc.strictText $ luaString locName
        , "\", "
        ] ++
        -- A trade relation is told apart by this flag alone, so it is written
        -- even though it carries no number of its own.
        [ "trade = true, " | fromMaybe False (omodTrade mod) ] ++
        field "value" (omodValue mod) ++
        field "max_trust" (omodMax mod) ++
        field "min_trust" (omodMin mod) ++
        field "days" (durationDays mod) ++
        field "decay" (omodDecay mod) ++
        [ "},"
        , PP.line
        ]

    where
        field :: Text -> Maybe Double -> [Doc]
        field name (Just val) = [Doc.strictText name, " = ", Doc.ppFloat val, ", "]
        field _ Nothing = []

-- | How long the modifier lasts, in the one unit the module knows. Script says
-- days, months or years, but [[Module:Opinion]] holds only a @days@ field and
-- breaks it up again itself, counting a year as 365 days and a month as 30.
-- Converting the same way round means what it prints is what the script asked
-- for: a year for every twelve months, and thirty days for each month over.
durationDays :: HOI4OpinionModifier -> Maybe Double
durationDays mod = if total == 0 then Nothing else Just total
    where
        months = floor (fromMaybe 0 (omodMonths mod)) :: Int
        years = floor (fromMaybe 0 (omodYears mod)) :: Int
        (yearsOfMonths, monthsLeft) = months `divMod` 12
        total = fromIntegral ((years + yearsOfMonths) * 365 + monthsLeft * 30)
                    + fromMaybe 0 (omodDays mod)

-- | Make a localized name safe to sit inside a Lua double-quoted string. A name
-- written over two lines is flattened, since the module prints it as one phrase.
luaString :: Text -> Text
luaString = T.concatMap escape
    where
        escape '\\' = "\\\\"
        escape '"' = "\\\""
        escape '\n' = " "
        escape '\r' = ""
        escape c = T.singleton c

parseHOI4DynamicModifiers :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4DynamicModifier)
parseHOI4DynamicModifiers scripts = keyedBy dmodName <$>
    parseScriptFiles "dynamic modifiers" (mapM parseHOI4DynamicModifier) scripts

parseHOI4DynamicModifier :: (HOI4Info g, MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4DynamicModifier))
parseHOI4DynamicModifier [pdx| $modid = @effects |]
    = withCurrentFile $ \file -> do
        mlocid <- fmap wikifyLocColours <$> getGameL10nIfPresent modid
        let dmd = foldl' addSection (HOI4DynamicModifier {
                dmodName = modid
            ,   dmodLocName = mlocid
            ,   dmodPath = file
            ,   dmodIcon = Nothing
            ,   dmodEffects = []
            ,   dmodEnable = []
            ,   dmodRemoveTrigger = Nothing
            }) effects
        return $ Right (Just dmd)
    where
        addSection :: HOI4DynamicModifier -> GenericStatement -> HOI4DynamicModifier
        addSection dmd stmt@[pdx| $lhs = @scr |] = case lhs of
            "enable"       -> dmd { dmodEnable = scr }
            "remove_trigger" -> dmd { dmodRemoveTrigger = Just scr }
            _ -> warn (UnknownSection "dynamic modifier" stmt) dmd
        addSection dmd stmt@[pdx| icon = $txt |] = dmd  { dmodIcon = Just txt }
         -- Must be an effect
        addSection dmd stmt = dmd { dmodEffects = dmodEffects dmd ++ [stmt] }
parseHOI4DynamicModifier stmt = rejectForm "dynamic modifier" stmt

writeHOI4DynamicModifiers :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4DynamicModifiers = do
    dynamicModifiers <- getDynamicModifiers
    writeFeatures "dynamic_modifiers"
                  [Feature { featurePath = Just "tables"
                           , featureId = Just "dynamic_modifiers.txt"
                           , theFeature = Right (HM.elems dynamicModifiers)
                           }]
                  pp_dynamic_modifiers
    where
        pp_dynamic_modifiers :: (HOI4Info g, Monad m) => [HOI4DynamicModifier] -> PPT g m Doc
        pp_dynamic_modifiers mods = do
            version <- gets (gameVersion . getSettings)
            modDoc <- mapM pp_dynamic_modifier (sortOn (sortName . dmodLocName) mods)
            return $ mconcat $
                [ "{{Version|", Doc.strictText version, "}}", PP.line
                , "{| class=\"mildtable\"", PP.line
                , "! ", PP.line
                , "! style=\"min-width:260px; text-align:center\" | Name", PP.line
                , "! style=\"text-align:center\" | Requirements", PP.line
                , "! style=\"min-width:260px; text-align:center\" | Effects", PP.line
                ] ++ modDoc ++
                [ "|}", PP.line
                ]

        sortName (Just n) =
            let ln = T.toLower n
                nn = T.stripPrefix "the " ln
            in fromMaybe ln nn
        sortName _ = ""

        pp_dynamic_modifier :: (HOI4Info g, Monad m) => HOI4DynamicModifier -> PPT g m Doc
        pp_dynamic_modifier mod = do
            req <- imsg2doc =<< ppMany (dmodEnable mod)
            icon <- maybe (return mempty) (\i -> do
                icond <- getGameInterfaceNamed i
                return $ "[[File:" <> icond <> ".png|22px]]") (dmodIcon mod)
            loc <- do
                mloc <- fmap wikifyLocColours <$> getGameL10nIfPresent (dmodName mod <> "_desc")
                case mloc of
                    Just locd -> do
                        let docloc = Doc.strictText locd
                        return $ mconcat [docloc, PP.line]
                    _ -> return ""
            eff <- withCurrentIndent $ \_ -> do imsg2doc . fold =<< traverse (modifierMSG False "") (dmodEffects mod)
            return $ mconcat
                [ "|- style=\"vertical-align:top;\"", PP.line
                , "| ", Doc.strictText icon, PP.line
                , "| ", PP.line
                , "==== ", Doc.strictText $ fromMaybe (dmodName mod) (dmodLocName mod) , " ===="
                , " <!-- ", Doc.strictText (dmodName mod), " -->", PP.line
                , loc
                , "| ", PP.line
                , req , PP.line
                , "|", PP.line
                , eff, PP.line
                ]


parseHOI4Modifiers :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Modifier)
parseHOI4Modifiers scripts = keyedBy modName <$>
    parseScriptFiles "modifiers" (mapM parseHOI4Modifier) scripts

parseHOI4Modifier :: (HOI4Info g, MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4Modifier))
parseHOI4Modifier [pdx| $modid = @effects |]
    = withCurrentFile $ \file -> do
        mlocid <- fmap wikifyLocColours <$> getGameL10nIfPresent modid
        let modi = foldl' addSection (HOI4Modifier {
                modName = modid
            ,   modLocName = mlocid
            ,   modPath = file
            ,   modIcon = Nothing
            ,   modEffects = []
            ,   modRemoveTrigger = Nothing
            }) effects
        return $ Right (Just modi)
    where
        addSection :: HOI4Modifier -> GenericStatement -> HOI4Modifier
        addSection modi stmt@[pdx| $lhs = @scr |] = case lhs of
            "valid_relation_trigger" -> modi { modRemoveTrigger = Just scr }
            _ -> warn (UnknownSection "modifier" stmt) modi
        addSection modi stmt@[pdx| icon = $txt |] = modi  { modIcon = Just txt }
         -- Must be an effect
        addSection modi stmt = modi { modEffects = modEffects modi ++ [stmt] }
parseHOI4Modifier stmt = rejectForm "modifier" stmt