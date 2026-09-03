{-
Module      : HOI4.Misc
Description : Feature handler for miscellaneous features in Hearts of Iron IV
-}
{-# LANGUAGE LambdaCase #-}
module HOI4.Misc (
         parseHOI4CountryHistory
        ,parseHOI4Terrain
        ,parseHOI4Ideology
        ,parseHOI4Effects
        ,parseHOI4Triggers
        ,parseHOI4BopRanges
        ,parseHOI4ModifierDefinitions
        ,parseHOI4Buildings
        ,parseHOI4MioNames
        ,parseHOI4ScriptedLoc
        ,parseHOI4ScriptConstants
        ,parseHOI4LocKeys
    ) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (..))

import Data.Char (isUpper, isAlphaNum)
import Data.List ( sortOn, foldl', elemIndex )
import Data.Maybe (listToMaybe, mapMaybe)

import System.FilePath (takeFileName)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
 -- everything
import QQ (pdx)
import SettingsTypes ( PPT
                     , IsGame (..), IsGameData (..), IsGameState (..)

                     , withCurrentFile
                     , hoistErrors)
import ParseWarnings
import HOI4.Common -- everything
import StatementUtils -- everything
import HOI4.ModifierTable (modifiersTable)
import HOI4.Messages (ScriptMessage (..), ModifierDisplay, modYesNo, modNoYes)

newHOI4CountryHistory :: Maybe Text -> Text -> HOI4CountryHistory
newHOI4CountryHistory cosmetic chtag = HOI4CountryHistory chtag chtag cosmetic

-- | What the country history says about each country: which party rules it, and
-- alongside that the value every variable the history writes to starts the game
-- holding. The two are read off the same files in the one pass over them.
parseHOI4CountryHistory :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript
        -> PPT g m (HashMap Text HOI4CountryHistory, HashMap Text Double)
parseHOI4CountryHistory scripts = (, initialVariables) . keyedBy chTag <$>
    parseScriptFiles "country history"
        (\scr -> case (cosmeticTag scr, concatMap mapHisto scr) of
            (cosmetic@(Just _), []) -> withCurrentFile $ \sourceFile -> return
                [Right (Just (newHOI4CountryHistory cosmetic (fileTag sourceFile)))]
            (cosmetic, stmts) -> mapM (processPolitics cosmetic) stmts)
        scripts
    where
        mapHisto scr = case scr of
            stmt@[pdx| set_politics = @pol |] -> [stmt]
            _ -> []

        -- A country whose history sets a cosmetic tag goes by the name that tag
        -- gives rather than the one its own tag would: this is how Indonesia
        -- starts out as the Dutch East Indies. Only a tag set at the top level
        -- counts, since one set inside an @if@ or under a later date is a name
        -- the country may come to have and not the one it starts with.
        cosmeticTag :: GenericScript -> Maybe Text
        cosmeticTag scr = listToMaybe [t | [pdx| set_cosmetic_tag = $t |] <- scr]

        -- Each history file is named for the country it is about, and its tag
        -- is what the name starts with.
        fileTag :: FilePath -> Text
        fileTag = T.pack . take 3 . takeFileName

        -- Script keeps the numbers behind a dynamic modifier in variables, so
        -- that it can raise and lower what the modifier grants as the game goes
        -- on, and it gives them their opening values here. A variable no history
        -- writes to is not in this table at all, which is the same as holding
        -- zero.
        --
        -- Only the plain writes are read: a variable set from another variable,
        -- or worked out with arithmetic, has no one number to name it by. A name
        -- two countries start with different values under is dropped as well,
        -- since nothing here says which of them is meant; these are written with
        -- the country's tag on the front by convention, so there are only a
        -- handful.
        initialVariables :: HashMap Text Double
        initialVariables = HM.mapMaybe id $ HM.fromListWith agree
            [ (var, Just val)
            | stmt <- concat (HM.elems scripts), (var, val) <- setVariables stmt ]
        agree new old = if new == old then old else Nothing

        -- History puts the opening state of a country inside date blocks and
        -- inside conditionals on which packs are installed, so every level has
        -- to be followed down into.
        setVariables :: GenericStatement -> [(Text, Double)]
        setVariables [pdx| set_variable = @scr |] = maybe [] pure (assignment scr)
        setVariables [pdx| %_ = @scr |] = concatMap setVariables scr
        setVariables _ = []

        -- The two spellings of a write: the variable named on the left with the
        -- number on the right, or both spelled out as @var@ and @value@.
        assignment :: GenericScript -> Maybe (Text, Double)
        assignment scr = case (mapMaybe varName scr, mapMaybe varValue scr) of
            (var:_, val:_) -> Just (var, val)
            _ -> case mapMaybe plainAssignment scr of
                [pair] -> Just pair
                _ -> Nothing
        varName [pdx| var = ?v |] = Just v
        varName _ = Nothing
        varValue [pdx| value = !v |] = Just v
        varValue _ = Nothing
        plainAssignment [pdx| $v = !n |] = Just (v, n)
        plainAssignment _ = Nothing

processPolitics :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    Maybe Text -> GenericStatement -> PPT g m (Either Text (Maybe HOI4CountryHistory))
processPolitics cosmetic = onTopLevelCompound "set_politics" $ \_ parts ->
    withCurrentFile $ \file -> do
        let chtag = T.pack $ take 3 $ takeFileName file
        hoistErrors $ foldM processPoliticsAddSection
                            (Just (newHOI4CountryHistory cosmetic chtag))
                            parts

processPoliticsAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4CountryHistory -> GenericStatement -> PPT g m (Maybe HOI4CountryHistory)
processPoliticsAddSection Nothing _ = return Nothing
processPoliticsAddSection cohi stmt
    = sequence (processPoliticsAddSection' <$> cohi <*> pure stmt)
    where
        processPoliticsAddSection' cohi stmt@[pdx| ruling_party = $id |] =
            let tag = chTag cohi in
            return cohi { chRulingTag = T.pack (concat[T.unpack tag , "_" , T.unpack id])}
        processPoliticsAddSection' cohi stmt@[pdx| last_election = %_ |]
            = return cohi
        processPoliticsAddSection' cohi stmt@[pdx| election_frequency = %_ |]
            = return cohi
        processPoliticsAddSection' cohi stmt@[pdx| elections_allowed = %_ |]
            = return cohi
        processPoliticsAddSection' cohi stmt
            = warn (UnknownSection "set_politics" stmt) $ return cohi

-- | The value each variable holds when the game starts. Script keeps the numbers
-- behind a dynamic modifier in variables, so that it can raise and lower what the
-- modifier grants as the game goes on, and it gives them their opening values in
-- the country history. A variable no history writes to is not in here at all,
-- which is the same as holding zero.
--
-- Only country history is read, and only the plain writes in it: a variable set
-- from another variable, or worked out with arithmetic, has no one number to name
-- it by. A name two countries start with different values under is dropped as
-- well, since nothing here says which of them is meant; these are written with
-- the country's tag on the front by convention, so there are only a handful.
parseHOI4InitialVariables :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text Double)
parseHOI4InitialVariables scripts = return $ HM.mapMaybe id $
    HM.fromListWith agree
        [ (var, Just val)
        | stmt <- concat (HM.elems scripts), (var, val) <- setVariables stmt ]
    where
        agree new old = if new == old then old else Nothing

        -- History puts the opening state of a country inside date blocks and
        -- inside conditionals on which packs are installed, so every level has
        -- to be followed down into.
        setVariables :: GenericStatement -> [(Text, Double)]
        setVariables [pdx| set_variable = @scr |] = maybe [] pure (assignment scr)
        setVariables [pdx| %_ = @scr |] = concatMap setVariables scr
        setVariables _ = []

        -- The two spellings of a write: the variable named on the left with the
        -- number on the right, or both spelled out as @var@ and @value@.
        assignment :: GenericScript -> Maybe (Text, Double)
        assignment scr = case (mapMaybe varName scr, mapMaybe varValue scr) of
            (var:_, val:_) -> Just (var, val)
            _ -> case mapMaybe plainAssignment scr of
                [pair] -> Just pair
                _ -> Nothing
        varName [pdx| var = ?v |] = Just v
        varName _ = Nothing
        varValue [pdx| value = !v |] = Just v
        varValue _ = Nothing
        plainAssignment [pdx| $v = !n |] = Just (v, n)
        plainAssignment _ = Nothing

-------------
-- terrain --
-------------

parseHOI4Terrain :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m [Text]
parseHOI4Terrain scripts = flattened <$>
    parseScriptFiles "terrain" (mapM processTerrain . concatMap getcat) scripts
    where
        getcat = \case
            [pdx| categories = @cat |] -> cat
            _ -> []

processTerrain :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe Text))
processTerrain = onTopLevelCompound "terrain" $ \id _ -> return (Right (Just id))

----------------
-- ideologies --
----------------

parseHOI4Ideology :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text Text)
parseHOI4Ideology scripts = mkIdeoMap . flattened <$>
    parseScriptFiles "ideology" (mapM processIdeology . concatMap getideo) scripts
    where
        mkIdeoMap :: [(Text,[Text])] -> HashMap Text Text
        mkIdeoMap subideolist = HM.fromList $ concatMap switchideos subideolist
        switchideos :: (Text,[Text]) -> [(Text, Text)]
        switchideos (ideo, subideo) = map (switcheroo ideo) subideo
        switcheroo :: Text -> Text -> (Text, Text)
        switcheroo ideo subideo = (subideo, ideo)

        getideo = \case
            [pdx| ideologies = @cat |] -> cat
            _ -> []

processIdeology :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe (Text,[Text])))
processIdeology = onTopLevelCompound "ideology" $ \id parts -> do
    let subideos = concat $ mapMaybe (\case
            [pdx| types = @scr |] -> Just $ mapMaybe getsubs scr
            _-> Nothing) parts
    return (Right (Just (id , subideos)))
    where
        getsubs [pdx| $subideo = @_|] = Just subideo
        getsubs _ = Nothing


-----------------
-- scripted_<> --
-----------------

parseHOI4Effects :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text GenericStatement)
parseHOI4Effects scripts = HM.fromList . flattened <$>
    parseScriptFiles "scripted effects" (mapM parseHOI4Effect . concatMap onlyscripts) scripts
    where
        onlyscripts = \case
            stmt@[pdx| %_ = @_|] -> [stmt]
            _ -> []

parseHOI4Effect :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe (Text, GenericStatement)))
parseHOI4Effect stmt = onTopLevelCompound "scripted effect"
    (\id _ -> return (Right (Just (id , stmt))))
    stmt

parseHOI4Triggers :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text GenericStatement)
parseHOI4Triggers scripts = HM.fromList . flattened <$>
    parseScriptFiles "scripted triggers" (mapM parseHOI4Trigger . concatMap onlyscripts) scripts
    where
        onlyscripts = \case
            stmt@[pdx| %_ = @_|] -> [stmt]
            _ -> []

parseHOI4Trigger :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe (Text, GenericStatement)))
parseHOI4Trigger stmt = onTopLevelCompound "scripted trigger"
    (\id _ -> return (Right (Just (id , stmt))))
    stmt

------------------------------
-- Balance of power rangers --
------------------------------

parseHOI4BopRanges :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4BopRange)
parseHOI4BopRanges scripts = keyedBy bop_id <$>
    parseScriptFiles "bop ranges" (mapM parseHOI4BopRange . concatMap getRanges) scripts
    where
        getRanges :: GenericStatement -> [GenericStatement]
        getRanges = \case
            [pdx| %_ = @scrs |] -> concatMap (\case
                stmt@[pdx| range = @_ |] -> [stmt]
                stmt@[pdx| side = @scr |] -> getRanges stmt
                _ -> [])
                scrs
            _ -> []

parseHOI4BopRange :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement ->PPT g m (Either Text (Maybe HOI4BopRange))
parseHOI4BopRange stmt@[pdx| range = @scr |]
    = withCurrentFile $ \file ->
        let bop = foldl' addSection (HOI4BopRange {
                bop_id = ""
            ,   bop_on_activate = Nothing
            ,   bop_on_deactivate = Nothing
            ,   bop_path = file
            }) scr in
        return $ Right (Just bop)
    where
        addSection :: HOI4BopRange -> GenericStatement -> HOI4BopRange
        addSection bop [pdx| id = $txt |] = bop { bop_id = txt }
        addSection bop stmt@[pdx| on_activate = %rhs |] = case rhs of
                CompoundRhs [] -> bop
                CompoundRhs scr -> bop { bop_on_activate = Just scr }
                _-> warn (BadValue "bop on_activate" stmt) bop
        addSection bop stmt@[pdx| on_deactivate = %rhs |] = case rhs of
                CompoundRhs [] -> bop
                CompoundRhs scr -> bop { bop_on_deactivate = Just scr }
                _-> warn (BadValue "bop on_deactivate" stmt) bop
        addSection bop [pdx| min = %_ |] = bop
        addSection bop [pdx| max = %_ |] = bop
        addSection bop [pdx| modifier = %_ |] = bop
        addSection bop stmt = warn (UnknownSection "bop range" stmt) bop
parseHOI4BopRange stmt = rejectForm "bop range" stmt

-- Modifier Definitions

parseHOI4ModifierDefinitions scripts = mkModDefMap . flattened <$>
    parseScriptFiles "modifier definitions" (mapM parseHOI4ModifierDefinition . concatMap onlyscripts) scripts
    where
        mkModDefMap :: [ModifierDisplay] -> HashMap Text ModifierDisplay
        mkModDefMap ds = HM.fromList [(key, d) | d@(key,_,_) <- ds]
        onlyscripts = \case
            stmt@[pdx| %_ = @_|] -> [stmt]
            _ -> []

data ModDef = ModDef
        {   mdef_color_type :: Text
        ,   mdef_value_type :: Text
        ,   mdef_precision :: Double
        ,   mdef_postfix :: Maybe Text
        }

newMDF :: ModDef
newMDF = ModDef "<!--Check Script-->" "<!--Check Script-->" 0 Nothing

parseHOI4ModifierDefinition :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe ModifierDisplay))
parseHOI4ModifierDefinition = onTopLevelCompound "modifier definition" $ \id parts -> do
    let mdefdata = getscrmess id =<< foldM addSection newMDF parts
    return $ Right mdefdata
    where
        addSection mdf [pdx| color_type = $ctype |] = return $ mdf { mdef_color_type = ctype }
        addSection mdf [pdx| value_type = $vtype |] = return $ mdf { mdef_value_type = vtype }
        addSection mdf [pdx| category = %_ |] = return mdf
        addSection mdf [pdx| precision = !precision |] = return $ mdf { mdef_precision = precision }
        addSection mdf [pdx| postfix = $psfix |] = return mdf { mdef_postfix = Just psfix }
        addSection mdf stmt = return $ warn (UnknownSection "modifier definition" stmt) mdf

        getscrmess :: Text -> ModDef -> Maybe ModifierDisplay
        getscrmess mdid mdf = withPrecision <$> case T.toLower $ mdef_color_type mdf of
            "good" -> case T.toLower $ mdef_value_type mdf of
                "number" -> Just MsgModifierColourPos
                "percentage" -> Just MsgModifierPcPosReduced
                "percentage_in_hundred" -> Just MsgModifierPcPos
                "yes_no" -> Just modYesNo
                _ -> Nothing
            "neutral" -> case T.toLower $ mdef_value_type mdf of
                "number" -> Just MsgModifierSign
                "percentage" -> Just MsgModifierPcReducedSign
                "percentage_in_hundred" -> Just MsgModifierPcSign
                "yes_no" -> Just modYesNo
                _ -> Nothing
            "bad" -> case T.toLower $ mdef_value_type mdf of
                "number" -> Just MsgModifierColourNeg
                "percentage" -> Just MsgModifierPcNegReduced
                "percentage_in_hundred" -> Just MsgModifierPcNeg
                "yes_no" -> Just modNoYes
                _ -> Nothing
            _ -> Nothing
            where
                -- The definition says how many decimal places to write the value
                -- to, the same as the documentation does for a built-in modifier.
                withPrecision msg = (mdid, msg, Just (round (mdef_precision mdf)))


-- | The modifier localization keys that read as headings (all-caps), in the
-- order the given key list puts them; 'HOI4.Handlers.Modifiers.sortmods' sorts
-- modifier blocks by them.
parseHOI4LocKeys :: Monad m => [Text] -> PPT g m [Text]
parseHOI4LocKeys order = return $ map fst (sortOn (\x -> elemIndex (snd x) order) . filter (modchk . snd) . HM.toList . HM.map (\(loc,_,_) -> loc) $ modifiersTable)
    where modchk = T.all (\xs -> isUpper xs || not (isAlphaNum xs))

---------------
-- buildings --
---------------

-- | The buildings, keyed on the token script names them by. Only the modifiers a
-- building gives the state it stands in are kept: those are what the game shows
-- when script points at a building by name, and the rest of a building's entry
-- says how to build it rather than what it does.
parseHOI4Buildings :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Building)
parseHOI4Buildings scripts = return $ HM.unions
    [ HM.fromList [(bld_id bld, bld) | bld <- mapMaybe (building sourceFile) (concatMap named scr)]
    | (sourceFile, scr) <- HM.toList scripts ]
    where
        named = \case
            [pdx| buildings = @blds |] -> blds
            _ -> []
        building file [pdx| $bid = @scr |] = Just HOI4Building
            {   bld_id = bid
            ,   bld_state_modifiers = fst (extractStmt (matchLhsText "state_modifiers") scr)
            ,   bld_filepath = file
            }
        building _ _ = Nothing

---------------------------------------
-- military industrial organizations --
---------------------------------------

-- | The localization key that names each military industrial organization and
-- each of the department traits it can take on, keyed on the token script refers
-- to it by. Most are localized under their own token, but a fair number are not
-- and only their entry says which key to use.
-- Alongside the names, the archetype each organization is built out of, which
-- is the only thing that says what kind of manufacturer it is.
parseHOI4MioNames :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text Text, HashMap Text Text)
parseHOI4MioNames scripts = return (HM.unions (map organization orgs), HM.unions (map archetype orgs))
    where
        orgs = concat (HM.elems scripts)
        organization [pdx| $token = @scr |] =
            HM.fromList (nameOf token scr ++ concatMap trait scr)
        organization _ = HM.empty
        archetype [pdx| $token = @scr |] = case fst (extractStmt (matchLhsText "include") scr) of
            Just [pdx| %_ = $base |] -> HM.singleton token base
            _ -> HM.empty
        archetype _ = HM.empty
        -- A trait carries its own token, so the block it is written in says both
        -- halves; a trait added by one organization and altered by another is
        -- named the same either way, so the later entry may simply win.
        trait stmt = case stmt of
            [pdx| add_trait = @scr |] -> tokenNameOf scr
            [pdx| trait = @scr |] -> tokenNameOf scr
            [pdx| override_trait = @scr |] -> tokenNameOf scr
            _ -> []
        tokenNameOf scr = case fst (extractStmt (matchLhsText "token") scr) of
            Just [pdx| %_ = $token |] -> nameOf token scr
            _ -> []
        nameOf token scr = case fst (extractStmt (matchLhsText "name") scr) of
            Just [pdx| %_ = $name |] -> [(token, name)]
            Just [pdx| %_ = ?name |] -> [(token, name)]
            _ -> []

----------------------
-- script constants --
----------------------

-- | The numbers script can name instead of writing out, keyed on the dotted path
-- that names each, e.g. @sp_breakthrough_progress.medium@. A category may hold
-- its numbers directly or group them into blocks a further level down, and both
-- are named by the whole path down to the number, so the tree is flattened into
-- one entry per number. Nothing but numbers is kept: the other categories hold
-- lists of countries or states, which script uses in ways that have nothing to do
-- with a value.
-- | Read the scripted localizations: the texts the game picks between as it
-- draws a piece of localization, written as a @defined_text@ naming the key to
-- read under each set of conditions. Localization refers to one of these by
-- putting its name in brackets, e.g. @[CZE_continue_with_snejdareks_plan]@.
--
-- The texts are kept in the order they are written, since that is the order the
-- game tries them in and the last of them is usually the one it settles on.
parseHOI4ScriptedLoc :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text [HOI4ScriptedLocText])
parseHOI4ScriptedLoc scripts = return $ HM.fromList
    (mapMaybe definedText (concat (HM.elems scripts)))
    where
        definedText [pdx| defined_text = @scr |] = case getName scr of
            Just name -> Just (name, mapMaybe locText scr)
            Nothing -> Nothing
        definedText _ = Nothing
        getName scr = listToMaybe [nm | [pdx| name = $nm |] <- scr]
        locText [pdx| text = @txt |] = case getKey txt of
            Just key -> Just HOI4ScriptedLocText
                {   sloc_key = key
                ,   sloc_trigger = listToMaybe [trig | [pdx| trigger = @trig |] <- txt]
                }
            Nothing -> Nothing
        locText _ = Nothing
        getKey txt = listToMaybe $
            [key | [pdx| localization_key = $key |] <- txt]
            ++ [key | [pdx| localization_key = ?key |] <- txt]

parseHOI4ScriptConstants :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text Double)
parseHOI4ScriptConstants scripts = return $ HM.fromList
    (concatMap (constant "") (concat (HM.elems scripts)))
    where
        -- The schema says what shape a category's entries take rather than being
        -- one of them, and its own keys would read as constants named "data" and
        -- "any_key" if it were followed into.
        constant _ [pdx| schema = %_ |] = []
        constant prefix [pdx| $key = @scr |] = concatMap (constant (prefix <> key <> ".")) scr
        constant prefix [pdx| $key = !num |] = [(prefix <> key, num)]
        constant _ _ = []
