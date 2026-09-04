{-
Module      : HOI4.TechAndEquipment
Description : Feature handler for tech and equipment features in Hearts of Iron IV
-}
{-# LANGUAGE MultiWayIf #-}
module HOI4.TechAndEquipment (
        parseHOI4TechnologiesPath, writeHOI4Technologies,
        parseHOI4UnitTags, parseHOI4Units
    ) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.List ( foldl', sortOn )
import Data.Maybe (fromMaybe, mapMaybe, isJust, listToMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import System.FilePath ((</>), takeBaseName)

import Abstract -- everything
import qualified Doc
import FileIO (Feature (..), writeFeatures)
import HOI4.Messages -- everything
import MessageTools
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , withCurrentIndent, getGameInterface
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, getGameInterfaceIfPresent, concatMapM, indentUp)
import ParseWarnings
import HOI4.Common -- everything
import HOI4.Localization
import Data.Char (toLower)
import HOI4.ModifierTable (modifiersTable)


parseHOI4UnitTags :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m [Text]
parseHOI4UnitTags scripts = flattened <$>
    parseScriptFiles "unit tags" (mapM processUnitTags . concatMap getcat) scripts
    where
        getcat = \case
            [pdx| sub_unit_categories = @cat |] -> cat
            _ -> []

processUnitTags :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe Text))
processUnitTags (StatementBare (GenericLhs e [])) = return (Right (Just e))
processUnitTags stmt = onTopLevelCompound "unit tags" (\_ _ -> return (Right Nothing)) stmt

parseHOI4Units :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m [Text]
parseHOI4Units scripts = flattened <$>
    parseScriptFiles "units" (mapM processUnits . concatMap getcat) scripts
    where
        getcat = \case
            [pdx| sub_units = @unit |] -> unit
            _ -> []

processUnits :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe Text))
processUnits = onTopLevelCompound "unit" $ \id _ -> return (Right (Just id))

parseHOI4TechnologiesPath :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap FilePath [HOI4Technology])
parseHOI4TechnologiesPath scripts = HM.filter (not . null) <$>
    parseScriptFiles "technologies"
        (\scr -> mapM parseHOI4Technology $ concatMap (\case
            [pdx| technologies = @techs |] -> techs
            _ -> scr)
            scr)
        scripts

newHOI4Technology :: Text -> Text -> FilePath -> HOI4Technology
newHOI4Technology id locid = HOI4Technology id locid Nothing "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0 0 False Nothing Nothing Nothing

-- | Parse a statement in an opinion modifiers file. Some statements aren't
-- modifiers; for those, and for any obvious errors, return Right Nothing.
parseHOI4Technology :: (HOI4Info g, MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4Technology))
parseHOI4Technology stmt@[pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> rejectForm "technology" stmt
        IntLhs _ -> rejectForm "technology" stmt
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] -> withCurrentFile $ \file -> do
            locid <- getGameL10n id
            hoistErrors $ foldM technologyAddSection
                                (Just (newHOI4Technology id locid file))
                                parts
        _ -> rejectForm "technology" stmt
    _ -> return (Right Nothing)
parseHOI4Technology stmt = rejectForm "technology" stmt

-- | Interpret one section of an opinion modifier. If understood, add it to the
-- event data. If not understood, throw an exception.
technologyAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4Technology -> GenericStatement -> PPT g m (Maybe HOI4Technology)
technologyAddSection Nothing _ = return Nothing
technologyAddSection ttech stmt
    = return $ (`technologyAddSection'` stmt) <$> ttech
    where
        technologyAddSection' tech stmt@[pdx| $lhs = %rhs |] = case T.map toLower lhs of
            "dependencies" -> tech
            "research_cost" -> case rhs of
                (floatRhs -> Just num) -> tech { tech_cost = num }
                _ -> warn (BadValue "technology research_cost" stmt) tech
            "start_year" -> case rhs of
                (floatRhs -> Just num) -> tech { tech_start_year = num }
                _ -> warn (BadValue "technology start_year" stmt) tech
            "desc" -> case rhs of
                GenericRhs txt [] -> tech { tech_desc = Just txt }
                StringRhs txt -> tech { tech_desc = Just txt }
                _-> warn (BadValue "technology desc" stmt) tech
            "folder" -> tech
            "ai_will_do" -> tech
            "categories" -> tech
            "path" -> tech
            "enable_equipments" -> case rhs of
                CompoundRhs [] ->  tech
                CompoundRhs scr -> let equips = mapMaybe getbareTech scr in
                    tech { tech_equipment = Just equips }
                _-> warn (BadValue "technology equipment list" stmt) tech
            "enable_equipment_modules" -> case rhs of
                CompoundRhs [] ->  tech
                CompoundRhs scr -> let modules = mapMaybe getbareTech scr in
                    tech { tech_modules = Just modules }
                _-> warn (BadValue "technology equipment list" stmt) tech
            "enable_subunits" -> case rhs of
                CompoundRhs [] ->  tech
                CompoundRhs scr -> let units = mapMaybe getbareTech scr in
                    tech { tech_units = Just units }
                _-> warn (BadValue "technology equipment list" stmt) tech
            "allow" -> tech
            "allow_branch" -> tech
            "ai_research_weights" -> tech
            "on_research_complete" -> tech
            "on_research_complete_limit" -> tech
            "doctrine" -> case rhs of
                GenericRhs "yes" [] -> tech { tech_doctrine = True }
                -- no is the default, so I don't think this is ever used
                GenericRhs "no" [] -> tech { tech_doctrine = False }
                _ -> warn (BadValue "technology doctrine" stmt) tech
            "xor" -> tech
            "xp_research_type" -> tech
            "xp_boost_tech" -> tech
            "xp_research_bonus" -> tech
            "enable_building" -> case rhs of
                CompoundRhs [] -> tech
                CompoundRhs scr -> tech { tech_buildings = mconcat [tech_buildings tech, Just [scr]] }
                _-> warn (BadValue "technology equipment list" stmt) tech
            other -> do
                case HM.lookup other modifiersTable of
                    Just mod -> tech { tech_globalmod = mconcat [tech_globalmod tech, Just [stmt]] }
                    Nothing -> tech { tech_sortrest = mconcat [tech_sortrest tech, Just [stmt]] }
                            -- trace ("unknown tech section: " ++ T.unpack other) tech 


        technologyAddSection' tech stmt
            = warn (UnknownSection "technology" stmt) tech

        getbareTech (StatementBare (GenericLhs e [])) = Just e
        getbareTech stmt@[pdx| $lhs = %rhs |] = case T.map toLower lhs of
            "limit" -> Nothing
            _ -> warn (UnknownSection "technology array" stmt) Nothing
        getbareTech stmt = warn (UnknownSection "technology array" stmt) Nothing



writeHOI4Technologies :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Technologies = do
    pathTech <- getTechnologies
    let pathedTechnologies :: [Feature [HOI4Technology]]
        pathedTechnologies = map (\techs -> Feature {
                                        featurePath = Just $ tech_filepath $ head techs
                                    ,   featureId = Just (T.pack $ takeBaseName $ tech_filepath $ head techs) <> Just ".txt"
                                    ,   theFeature = Right techs })
                              (HM.elems pathTech)
    writeFeatures "technologies"
                  pathedTechnologies
                  ppTechnologies




ppTechnologies :: forall g m. (HOI4Info g, Monad m) => [HOI4Technology] -> PPT g m Doc
ppTechnologies techs = do
    version <- gets (gameVersion . getSettings)
    techDoc <- mapM ppTechnology techs
    return . mconcat $
        [ "{{Version|", Doc.strictText version, "}}", PP.line
        , "{| class=\"wikitable\"", PP.line
        , "! style=\"width:146px;\" | Icon (generic)", PP.line
        , "! style=\"width:15em;\" | Technology", PP.line
        , "! style=\"width:1px;\" | Year", PP.line
        , "! style=\"width:67px;\" | Base cost", PP.line
        , "! style=\"width:13em;\" | Prerequisites", PP.line
        , "! Description", PP.line
        , "! style=\"width:526.5px;\" | Statistics", PP.line
        ] ++ techDoc ++
        [ "|}", PP.line
        ]



ppTechnology :: forall g m. (HOI4Info g, Monad m) => HOI4Technology -> PPT g m Doc
ppTechnology tech = setCurrentFile (tech_filepath tech) $ do
    let nfArg :: (HOI4Technology -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        nfArg field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        [content_pp'd
                        ,PP.line])
            (field tech)
        nfArgExtra :: Doc -> (HOI4Technology -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        nfArgExtra extra field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        ["{{",extra,"|",PP.line
                        ,content_pp'd
                        ,"}}"
                        ,PP.line])
            (field tech)
        nfArgClari :: Doc -> (HOI4Technology -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        nfArgClari extra field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        [extra, PP.line
                        ,content_pp'd
                        ,PP.line])
            (field tech)
        unitdesc = case tech_units tech of
            Just unit ->
                wikifyLocColours <$> getGameL10nDefault "" (head unit <> "_desc")
            Nothing -> return ""

    if tech_doctrine tech then return . mconcat $ [] else do
        desc <- case tech_desc tech of
            Just spec -> wikifyLocColours <$> getGameL10n spec
            Nothing -> do
                mdesc <- fmap wikifyLocColours <$> getGameL10nIfPresent (tech_id tech <> "_desc")
                case mdesc of
                    Just desc -> return desc
                    Nothing -> do
                        case tech_equipment tech of
                            Just equ -> do
                                mdesc <- fmap wikifyLocColours <$> getGameL10nIfPresent (head equ <> "_desc")
                                maybe unitdesc return mdesc
                            Nothing -> unitdesc

        gfx <- do
            let techgfx = do
                    mgfx <- getGameInterfaceIfPresent ("GFX_" <> tech_id tech <> "_medium")
                    return $ fromMaybe "" mgfx
            case tech_equipment tech of
                Just equ -> do
                    mgfx <- getGameInterfaceIfPresent ("GFX_" <> head equ <> "_medium")
                    maybe techgfx return mgfx
                Nothing -> techgfx
        let equiptech = fromMaybe [] (tech_equipment tech)
            moduletech = fromMaybe [] (tech_modules tech)
            unittech = fromMaybe [] (tech_units tech)
            buildingtech = fromMaybe [] (tech_buildings tech)
            globaltech = fromMaybe [] (tech_globalmod tech)
            sorttech = fromMaybe [] (tech_sortrest tech)
        unitscheck <- getUnit
        units <- do
            return $ mapMaybe (\case
                            stmt@[pdx| $rhs = @scr |] -> if rhs `elem` unitscheck then Just stmt else Nothing
                            _ -> Nothing ) sorttech
        unittag <- do
            unittagscheck <- getUnitTag
            return $ mapMaybe (\case
                            stmt@[pdx| $rhs = @scr |] -> if rhs `elem` unittagscheck then Just stmt else Nothing
                            _ -> Nothing ) sorttech
        let unitmodtech = fromMaybe [] (tech_unitmod tech) ++ units
            catmodtech = fromMaybe [] (tech_catmod tech) ++ unittag      
        techEffDoc <- ppTechEffects tech equiptech moduletech unittech buildingtech unitmodtech catmodtech globaltech
        return . mconcat $
            [ "|- ", PP.line
            , "| style=\"width:1px; text-align: center;\" | [[File:", Doc.strictText gfx, ".png|", Doc.strictText (tech_loc tech),"]]", PP.line
            , "! ", Doc.strictText (tech_loc tech), "<!-- ", Doc.strictText (tech_id tech), " -->", PP.line
            , "| ", Doc.strictText (T.pack $ show $ tech_start_year tech), PP.line
            , "| ", Doc.strictText (T.pack $ show $ round (tech_cost tech * 100)), " days",PP.line
            , "| ", PP.line
            , "| ", italic $ Doc.strictText desc,PP.line
            , "| <!-- research effects here -->", PP.line
            , techEffDoc,PP.line,PP.line]

ppTechEffects :: forall g m. (HOI4Info g, Monad m) => HOI4Technology  -> [Text] -> [Text] -> [Text] -> [GenericScript] -> [GenericStatement] -> [GenericStatement] -> [GenericStatement] -> PPT g m Doc
ppTechEffects tech equip modul units buildings unitmod catmod globals = if all null [equip, modul, units] && null buildings && all null [unitmod, catmod, globals] then return . mconcat $ [] else do

    equipDoc <- (if null equip then return [] else concatMapM (equipthing (tech_id tech)) equip)
    modulDoc <- (if null equip then return [] else concatMapM modulething modul)
    unlockDoc <- (if null units && null buildings then return [] else do
        unitDoc <- concatMapM unitthing units
        buildingDoc <- concatMapM buildingthing buildings
        return $
            ["|-", PP.line
            ,"| colspan=\"2\" style=\"text-align: center; font-size: 150%; border: none;\" | <br>Unlocks", PP.line] ++
            unitDoc++
            buildingDoc)
    unitcatmodDoc <- (if null unitmod && null catmod then return [] else do
        unitmodDoc <- if null unitmod then return [] else unitmodthing (head units)
        catmodDoc <- concatMapM catmodthing catmod
        return $
            ["|-", PP.line
            ,"| colspan=\"2\" style=\"text-align: center; font-size: 150%; border: none;\" | Upgrades", PP.line] ++
            unitmodDoc ++
            catmodDoc)
    globalDoc <- (if null globals then return [] else do
            globald <- globalthing (head globals)
            return $
                ["|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; font-size: 150%; border: none;\" | Global stats", PP.line] ++
                globald)
    return . mconcat $
        ["{| class=\"wikitable mw-collapsible innercollapse mw-collapsed\" style=\"margin-right:2px\"", PP.line
        ,"! style=\"width:551px;\" colspan=\"2\" |", PP.line]++
        equipDoc++
        modulDoc++
        unlockDoc++
        unitcatmodDoc++
        globalDoc++
        ["|}"]
    where
        equipthing :: forall g m. (HOI4Info g, Monad m) => Text -> Text -> PPT g m [Doc]
        equipthing equ tech = do
            gfx <- do
                techgfx <- do
                    mgfx <- getGameInterfaceIfPresent ("GFX_" <> tech <> "_medium")
                    return $ fromMaybe "" mgfx
                mequgfx <- getGameInterfaceIfPresent ("GFX_" <> equ <> "_medium")
                return $ fromMaybe techgfx mequgfx
            equname <- getGameL10n equ
            return
                ["|-", PP.line
                ,"| colspan=\"2\" style=\"border: none;\" | <!-- resource costs like steel -->", PP.line
                ,"|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" | [[File:", Doc.strictText gfx,".png|", Doc.strictText equname,"]]", PP.line
                ,"|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" |", PP.line
                ,"|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" |", PP.line
                ,"|-", PP.line
                ,"| style=\"border: none; width: 50%; padding-right: 10px;\"| '''<!--statname here-->:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line
                ,"| style=\"border: none; width: 50%; padding-left: 10px;\"| '''<!--statname here-->:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line
                ,"|-", PP.line
                ,"| style=\"border: none; width: 50%; padding-right: 10px;\"| {{icon|production cost}} '''Production Cost:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line
                ,"| style=\"border: none; width: 50%; padding-left: 10px;\"|", PP.line]

        modulething :: forall g m. (HOI4Info g, Monad m) => Text -> PPT g m [Doc]
        modulething modul = do
            gfx <- do
                mgfx <- getGameInterfaceIfPresent ("GFX_EMI_" <> modul )
                return $ fromMaybe "" mgfx
            modulname <- getGameL10n modul
            return
                ["|-"
                ,"| colspan=\"2\" style=\"text-align: center; font-size: 150%; border: none;\" | ", PP.line
                ,"|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" |  [[File:", Doc.strictText gfx,".png|link=]] <!-- ", Doc.strictText modul, "-->", Doc.strictText modulname, PP.line
                ,"|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" | <!-- {{expansion|nsb}} -->", PP.line
                ,"|-", PP.line
                ,"| style=\"border: none; width: 50%; padding-right: 10px;\"| '''<!--statname here-->:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line
                ,"| style=\"border: none; width: 50%; padding-left: 10px;\"| '''<!--statname here-->:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line
                ,"|-", PP.line
                ,"| style=\"border: none; width: 50%; padding-right: 10px;\"| '''<!--statname here-->:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line
                ,"| style=\"border: none; width: 50%; padding-left: 10px;\"| {{icon|production cost}} '''Production Cost:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line]

        unitthing :: forall g m. (HOI4Info g, Monad m) => Text -> PPT g m [Doc]
        unitthing unit = do
            unitname <- getGameL10n unit
            return
                ["|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" | [[File:<!--uniticonnamehere-->.png|link=<!--linknamehere-->]] <!--", Doc.strictText unit, " -->", Doc.strictText unitname, PP.line
                ,"|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" |", PP.line
                ,"|-", PP.line
                ,"| style=\"border: none; width: 50%; padding-right: 10px;\"| '''<!--statname here-->:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line
                ,"| style=\"border: none; width: 50%; padding-left: 10px;\"| '''<!--statname here-->:''' <span style=\"float:right;\"><!-- number here --></span>", PP.line]
        -- A building the technology makes available, and up to what level.
        buildingthing :: forall g m. (HOI4Info g, Monad m) => GenericScript -> PPT g m [Doc]
        buildingthing build = do
            let mbld = listToMaybe [ bld | [pdx| building = $bld |] <- build ]
                mlevel = listToMaybe [ level | [pdx| level = !level |] <- build ] :: Maybe Double
            shown <- case mbld of
                Just bld -> do
                    icon <- buildingIcon bld
                    return $ icon <> "<!-- " <> bld <> " -->"
                        <> maybe "" (\level -> " up to level " <> T.pack (show (round level :: Int))) mlevel
                Nothing -> return "<!-- CHECK SCRIPT: enable_building without a building -->"
            return
                ["|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" | ", Doc.strictText shown, PP.line]

        unitmodthing unitmod = do
            return
                ["|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" | [[File:<!--uniticonnamehere-->.png]] ", PP.line
                ,"|-", PP.line
                ,"| style=\"border: none; width: margin:140px; padding-left:4.5em;\"| '''<!--statname here-->:'''", PP.line
                ,"| style=\"border: none; width: 50%; padding-left: 10px;\"| <span style=\"float:right;\"><!-- number here --></span>", PP.line]

        catmodthing catmod = do
            let rhs = case catmod of
                    [pdx| $rhs = @scr |] -> rhs
                    _ -> ""
            gfx <- do
                mgfx <- getGameInterfaceIfPresent ("GFX_unit_" <> rhs )
                return $ fromMaybe "" mgfx
            catmodname <- getGameL10n rhs
            return
                ["|-", PP.line
                ,"| colspan=\"2\" style=\"text-align: center; border: none;\" | [[File:", Doc.strictText gfx,".png]] <!-- ", Doc.strictText rhs, " -->", Doc.strictText catmodname, PP.line
                ,"|-", PP.line
                ,"| style=\"border: none; width: margin:140px; padding-left:4.5em;\"| '''<!--statname here-->:'''", PP.line
                ,"| style=\"border: none; width: 50%; padding-left: 10px;\"| <span style=\"float:right;\"><!-- number here --></span>", PP.line]

        globalthing :: forall g m. (HOI4Info g, Monad m) => GenericStatement -> PPT g m [Doc]
        globalthing global = do
            return
                ["|-", PP.line
                ,"| style=\"border: none; width: margin:140px; padding-left:4.5em;\"| '''<!--statname here-->:'''", PP.line
                ,"| style=\"border: none; width: 50%; padding-left: 10px;\"| <span style=\"float:right;\"><!-- number here --></span>", PP.line]
