{-
Module      : HOI4.NationalFocus
Description : Feature handler for Hearts of Iron IV national focuses
-}
module HOI4.NationalFocus (
        parseHOI4NationalFocuses
        ,writeHOI4NationalFocuses
    ) where

import System.FilePath (takeBaseName)

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Char (isSpace, toLower)
import Data.List (nub)
import Control.Arrow (first)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import FileIO (Feature (..), writeFeatures)
 -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , setCurrentFile, withCurrentFile
                     , hoistErrors
                     , indentUp
                     , getGameInterface, getGameInterfaceIfPresent)
import HOI4.Common -- everything
import HOI4.Localization
import HOI4.Messages (wikifyLocColours, messageText)
import ParseWarnings

-- | Empty national focus. Starts off Nothing/empty everywhere, except id and name
-- (which should get filled in immediately).
newHOI4NationalFocus :: HOI4NationalFocus
newHOI4NationalFocus = HOI4NationalFocus "(Unknown)" "(Unknown)" Nothing Nothing "GFX_goal_unknown" Nothing [] [] undefined Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing undefined 0 Nothing

-- | One icon out of a focus's @icon@ block: either the icon it falls back on,
-- written @yes@, or an icon with the conditions it is shown under. An entry we
-- cannot read is dropped, since the focus still has its other icons to show.
iconVariant :: GenericStatement -> Maybe (Either Text (Text, GenericScript))
iconVariant [pdx| $key = yes |] = Just (Left (gfxKey key))
iconVariant [pdx| $key = @scr |] = Just (Right (gfxKey key, scr))
iconVariant _ = Nothing

-- | The name an icon goes by in the interface files, which is what a lookup
-- wants. Script writes it either with the @GFX_@ on the front or without.
gfxKey :: Text -> Text
gfxKey txt = if "GFX_" `T.isPrefixOf` txt then txt else "GFX_" <> txt


-- | Take the decisions scripts from game data and parse them into decision
-- data structures.
parseHOI4NationalFocuses :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4NationalFocus)
parseHOI4NationalFocuses scripts = keyedBy nf_id <$>
    parseScriptFiles "national focus"
        (\scr -> mapM (\(ordinal, (country, stmt)) ->
                        parseHOI4NationalFocus (fileVars scr) country ordinal stmt)
                      (zip [0..] (concatMap mapTree scr)))
        scripts
    where
        -- A tree holds all the focuses standing in it, and says which country
        -- it is for; a focus shared between trees stands on its own and belongs
        -- to no one country. Either way the order they come out in is the order
        -- the file writes them in.
        mapTree scr = case scr of
            [pdx| focus_tree = @focus |] -> map ((,) (treeCountry focus)) focus
            [pdx| shared_focus = @_ |] -> [(Nothing, scr)]
            [pdx| joint_focus = @_ |] -> [(Nothing, scr)]
            _ -> []

-- | Parse a statement in an national focus file. Some statements aren't
-- national focus'; for those, and for any obvious errors, return Right Nothing.
-- | The country a focus tree is written for, where it is written for just the
-- one. A tree scores in its @country@ block how likely each country is to be
-- given it, and a tree meant for a single country names only that one there. A
-- tree shared between a set of releasable nations names all of them, and none
-- of those is the country its page is about.
treeCountry :: GenericScript -> Maybe Text
treeCountry tree = case nub (tagsIn scoring) of
    [tag] -> Just tag
    _ -> Nothing
    where
        scoring = concat [scr | [pdx| country = @scr |] <- tree]
        tagsIn = concatMap tagOf
        tagOf [pdx| tag = $tag |] = [tag]
        tagOf [pdx| original_tag = $tag |] = [tag]
        tagOf [pdx| %_ = @inner |] = tagsIn inner
        tagOf _ = []

parseHOI4NationalFocus :: (HOI4Info g, MonadError Text m) =>
    HashMap Text Double -> Maybe Text -> Int -> GenericStatement -> PPT g m (Either Text (Maybe HOI4NationalFocus))
parseHOI4NationalFocus vars country ordinal stmt@[pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> rejectForm "national focus" stmt
        IntLhs _ -> rejectForm "national focus" stmt
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] ->
            if not (id == "focus" || id == "shared_focus" || id == "joint_focus") then
                return (Right Nothing)
            else
                withCurrentFile $ \file -> do
                    let nameKey = fromMaybe (getNFId parts) (getNFTxt parts)
                        -- A focus standing in one country's tree is written for
                        -- that country, and says ROOT where its name belongs.
                        -- One shared between trees has no country of its own,
                        -- and 'country' is already Nothing for those.
                        root = if id == "focus" then country else Nothing
                    nfNameLoc <- wikifyLocColours <$> getGameL10nFor root nameKey
                    nfNameDesc <- fmap wikifyLocColours
                                    <$> getGameL10nIfPresentFor root (nameKey <> "_desc")
                    -- A focus given several names points its localization at a
                    -- scripted one, which names a text for each set of
                    -- conditions and one to fall back on. The fallback is the
                    -- name the focus is listed under, so the rest are the names
                    -- it goes by instead.
                    nfNameVariants <- map (first wikifyLocColours)
                                        <$> scriptedLocVariants root nameKey
                    hoistErrors $ foldM (nationalFocusAddSection vars)
                                        (Just newHOI4NationalFocus {nf_path = file
                                                                    ,nf_name_loc = nfNameLoc
                                                                    ,nf_name_desc = nfNameDesc
                                                                    ,nf_name_variants = nfNameVariants
                                                                    ,nf_ordinal = ordinal
                                                                    ,nf_country = country})
                                        parts
        _ -> rejectForm "national focus" stmt
    _ -> return (Right Nothing)
    where
        getNFId ([pdx| id = $nfname|]:_) = nfname
        getNFId (_:xs) = getNFId xs
        getNFId [] = "(unknown)"
        getNFTxt ([pdx| text = $nfname|]:_) = Just nfname
        getNFTxt (_:xs) = getNFTxt xs
        getNFTxt [] = Nothing
parseHOI4NationalFocus _ _ _ stmt = rejectForm "national focus" stmt

-- | Interpret one section of an national focus. If understood, add it to the
-- event data. If not understood, throw an exception.
nationalFocusAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    HashMap Text Double -> Maybe HOI4NationalFocus -> GenericStatement -> PPT g m (Maybe HOI4NationalFocus)
nationalFocusAddSection _ Nothing _ = return Nothing
nationalFocusAddSection vars nf stmt
    = return $ (`nationalFocusAddSection'` stmt) <$> nf
    where
        nationalFocusAddSection' nf stmt@[pdx| $lhs = %rhs |] = case T.map toLower lhs of
            "id" -> case rhs of
                GenericRhs txt [] -> nf { nf_id = txt}
                _-> warn (BadValue "national focus id" stmt) nf
            "text" -> case rhs of
                GenericRhs txt [] -> nf { nf_text = Just txt}
                _-> warn (BadValue "national focus text" stmt) nf
            "completion_reward" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf { nf_completion_reward = Just scr }
                _-> warn (BadValue "national focus completion_reward" stmt) nf
            "icon" -> case rhs of
                GenericRhs txt [] ->
                    nf { nf_icon = gfxKey txt}
                StringRhs txt ->
                    nf { nf_icon = gfxKey txt}
                -- A focus can name several icons rather than one, each written
                -- with the conditions the game shows it under, and one written
                -- @yes@ that it falls back on when none of the others apply.
                -- That last one leads, since it is what the focus shows unless
                -- something makes it show one of the others.
                CompoundRhs scr ->
                    let (fallback, conditional) = partitionEithers (mapMaybe iconVariant scr) in
                    nf { nf_icon = fromMaybe (nf_icon nf) (listToMaybe fallback)
                       , nf_icon_variants = conditional}
                _-> warn (BadValue "national focus icon" stmt) nf
            "alternate_icon" -> case rhs of
                GenericRhs txt [] ->
                    let txtd = if "GFX_" `T.isPrefixOf` txt then txt else "GFX_" <> txt in
                    nf { nf_alt_icon = Just txtd}
                StringRhs txt ->
                    let txtd = if "GFX_" `T.isPrefixOf` txt then txt else "GFX_" <> txt in
                    nf { nf_alt_icon = Just txtd}
                CompoundRhs _ ->
                    nf { nf_alt_icon = Just "Multiple Pictures possible, check script."}
                _-> warn (BadValue "national focus alternate_icon" stmt) nf
            "cost" -> case rhs of
                (floatRhs -> Just num) -> nf {nf_cost = num}
                GenericRhs var []
                    | Just num <- HM.lookup (fromMaybe var (T.stripPrefix "@" var)) vars
                    -> nf {nf_cost = num}
                _ -> warn (BadValue "national focus cost" stmt) nf
            "allow_branch" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf { nf_allow_branch = Just scr }
                _-> warn (BadValue "national focus allow_branch" stmt) nf
            "x" -> nf
            "y" -> nf
            "prerequisite" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr ->
                    nf { nf_prerequisite = nf_prerequisite nf ++ [Just scr] }
                _-> warn (BadValue "national focus prerequisite" stmt) nf
            "mutually_exclusive" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr ->
                    nf { nf_mutually_exclusive = Just scr }
                _-> warn (BadValue "national focus mutually_exclusive" stmt) nf
            "available" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf { nf_available = Just scr }
                _-> warn (BadValue "national focus available" stmt) nf
            "bypass" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf { nf_bypass = Just scr }
                _-> warn (BadValue "national focus bypass" stmt) nf
            "completion_reward_joint_originator" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf { nf_joint_complete_origin = Just scr }
                _-> warn (BadValue "national focus completion_reward_joint_originator" stmt) nf
            "completion_reward_joint_member" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf { nf_joint_complete_member = Just scr }
                _-> warn (BadValue "national focus completion_reward_joint_member" stmt) nf
            "joint_trigger" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf { nf_joint_trigger = Just scr }
                _-> warn (BadValue "national focus joint_trigger" stmt) nf
            "cancel" -> nf
            "cancelable" -> nf --bool
            "historical_ai" -> nf
            "available_if_capitulated" -> nf --bool
            "cancel_if_invalid" -> nf --bool
            "continue_if_invalid" -> nf --bool
            "will_lead_to_war_with" ->  nf
            "search_filters" -> nf
            "select_effect" -> case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf {nf_select_effect = Just scr}
                _-> warn (BadValue "national focus select_effect" stmt) nf
            "ai_will_do" -> nf --Do we want to deal with aistuff with focus' ?
            "complete_tooltip" ->  case rhs of
                CompoundRhs [] ->
                    nf
                CompoundRhs scr -> nf { nf_complete_tooltip = Just scr }
                _-> warn (BadValue "national focus complete_tooltip" stmt) nf
            "offset" -> nf
            "relative_position_id" -> nf
            "text_icon" -> nf -- unknown what it does for now. AAT
            "dynamic" -> nf
            "enable_automatic_bypass" -> nf --bool
            "bypass_if_unavailable" -> nf --bool
            "bypass_effect" -> nf -- effect executed when the focus is bypassed
            "overlay" -> nf -- graphical overlay on the focus icon
            _ -> warn (UnknownSection "national focus" stmt) nf
        nationalFocusAddSection' nf stmt
            = warn (UnknownSection "national focus" stmt) nf



writeHOI4NationalFocuses :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4NationalFocuses = do
--    nationalFocuses <- getNationalFocus
    nationalFocuses <- getNationalFocusScripts
    pathNF <- parseHOI4NationalFocusesPath nationalFocuses --mkNfPathMap $ HM.elems nationalFocuses
    let pathedNationalFocus :: [Feature [HOI4NationalFocus]]
        pathedNationalFocus = map (\nf -> Feature {
                                        featurePath = Just $ nf_path $ head nf
                                    ,   featureId = Just (T.pack $ takeBaseName $ nf_path $ head nf) <> Just ".txt"
                                    ,   theFeature = Right nf })
                              (HM.elems pathNF)
    writeFeatures "national_focus"
                  pathedNationalFocus
                  ppNationalFocuses
--    where
--        mkNfPathMap :: [HOI4NationalFocus] -> HashMap FilePath [HOI4NationalFocus]
--        mkNfPathMap nf =
--            let xs = reverse $ map (nf_path &&& id) nf in
--            HM.fromListWith (++) [ (k, [v]) | (k, v) <- xs ]

parseHOI4NationalFocusesPath :: (HOI4Info g, Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap FilePath [HOI4NationalFocus])
parseHOI4NationalFocusesPath scripts = HM.filter (not . null) <$>
    parseScriptFiles "national focus"
        (\scr -> mapM (\(ordinal, (country, stmt)) ->
                        parseHOI4NationalFocus (fileVars scr) country ordinal stmt)
                      (zip [0..] (concatMap mapTree scr)))
        scripts
    where
        -- A tree holds all the focuses standing in it, and says which country
        -- it is for; a focus shared between trees stands on its own and belongs
        -- to no one country. Either way the order they come out in is the order
        -- the file writes them in.
        mapTree scr = case scr of
            [pdx| focus_tree = @focus |] -> map ((,) (treeCountry focus)) focus
            [pdx| shared_focus = @_ |] -> [(Nothing, scr)]
            [pdx| joint_focus = @_ |] -> [(Nothing, scr)]
            _ -> []

ppNationalFocuses :: forall g m. (HOI4Info g, Monad m) => [HOI4NationalFocus] -> PPT g m Doc
ppNationalFocuses nfs = do
    version <- gets (gameVersion . getSettings)
    nfDoc <- mapM (scope HOI4Country . ppNationalFocus) nfs -- Better to leave unsorted? (sortOn (sortName . nf_name_loc) nfs)
    return . mconcat $
        [ "{{Version|", Doc.strictText version, "}}", PP.line
        , "{| class=\"mildtable\" {{buffer}}", PP.line
        , "! style=\"width: 30%;\" | Focus", PP.line
        , "! style=\"width: 30%;\" | Prerequisites", PP.line
        , "! style=\"width: 40%;\" | Effects", PP.line
        ] ++ nfDoc ++
        [ "|}", PP.line
        ]

--sortName :: Text -> Text
--sortName n =
--    let ln = T.toLower n
--        nn = T.stripPrefix "the " ln
--    in fromMaybe ln nn

ppNationalFocus :: forall g m. (HOI4Info g, Monad m) => HOI4NationalFocus -> PPT g m Doc
ppNationalFocus nf = setCurrentFile (nf_path nf) $ withFocusIdents nf $ do
    let nfArg :: (HOI4NationalFocus -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        nfArg field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        [content_pp'd
                        ,PP.line])
            (field nf)
        nfArgExtra :: Doc -> (HOI4NationalFocus -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        nfArgExtra extra field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        ["{{",extra,"|",PP.line
                        ,content_pp'd
                        ,"}}"
                        ,PP.line])
            (field nf)
        nfArgClari :: Doc -> (HOI4NationalFocus -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
        nfArgClari extra field fmt
            = maybe (return [])
                (\field_content -> do
                    content_pp'd <- fmt field_content
                    return
                        [extra, PP.line
                        ,content_pp'd
                        ,PP.line])
            (field nf)
    icon_pp <- do
        -- The icon a focus names is the one the game draws it with, so a focus
        -- that names one is shown with it -- including the one it falls back on
        -- where it names several. Only a focus that names none of its own is
        -- shown with the icon named after it, which is the convention the game
        -- files follow but not a rule the game itself goes by.
        micon <- if nf_icon nf == nf_icon newHOI4NationalFocus
                    then getGameInterfaceIfPresent ("GFX_focus_" <> nf_id nf)
                    else return Nothing
        case micon of
            Nothing -> getGameInterface "goal_unknown" (nf_icon nf)
            Just idicon -> return idicon
    alt_icon_pp <- do
        case nf_alt_icon nf of
            Nothing -> return ""
            Just idicon -> return " <!-- ALT icon presentt, check script -->"
    alternatives_pp <- ppAlternatives (nf_name_variants nf) (nf_icon_variants nf)
    prerequisite_pp <- ppPrereq $ catMaybes $ nf_prerequisite nf
    allowBranch_pp <- ppAllowBranch $ nf_allow_branch nf
    mutuallyExclusive_pp <- ppMutuallyExclusive $ nf_mutually_exclusive nf
    available_pp <- nfArg nf_available ppScript
    joint_trigger_pp <- nfArgClari "<!-- joint_trigger -->Requirements for joint rewards:" nf_joint_trigger ppScript
    joint_reward_member_pp <- nfArgClari "Reward for joint member:" nf_joint_complete_member ppScript
    joint_reward_origin_pp <- nfArgClari "Reward for joint country that completed:" nf_joint_complete_origin ppScript
    complete_tool_pp <- nfArgClari "<!-- Tooltip shown for completion ->Completion tooltip:" nf_complete_tooltip ppScript
    bypass_pp <- nfArgExtra "bypass" nf_bypass ppScript
    completionReward_pp <- setIsInEffect True $ nfArg nf_completion_reward ppScript
    selectEffect_pp <- setIsInEffect True $
        nfArgClari "Effects on selecting the focus:" nf_select_effect ppScript
    -- The wiki heads each column with the country the tree is for, standing
    -- outside the list as the scope everything under it is read in. A focus
    -- shared between trees has no one country to name, and a column with
    -- nothing in it gets no heading of its own. Nothing here asks for the
    -- name in any particular form, so the heading is whatever the wiki calls
    -- the country day to day.
    countryHeading <- case nf_country nf of
        Nothing -> return []
        Just tag -> do
            name <- getCountryName tag
            return [Doc.strictText name, ":", PP.line]
    let headed col
            | T.all isSpace (Doc.doc2text (mconcat col)) = col
            | otherwise = countryHeading ++ col
    return . mconcat $
        [ "|- id = \"", Doc.strictText (nf_name_loc nf),"\"" , PP.line
        , "| {{iconbox|image=", Doc.strictText icon_pp, ".png ", PP.line
        , "| ", Doc.strictText (nf_name_loc nf) , "<!-- ", Doc.strictText (nf_id nf), " -->", PP.line
        , "| ",maybe mempty (Doc.strictText . Doc.nl2br) (nf_name_desc nf), PP.line , "}}", Doc.strictText alt_icon_pp, alternatives_pp, PP.line
        , "| ", PP.line]++
        headed (allowBranch_pp ++
                prerequisite_pp ++
                mutuallyExclusive_pp ++
                available_pp ++
                joint_trigger_pp ++
                bypass_pp) ++
        [ "| ", PP.line]++
        headed (completionReward_pp ++
                joint_reward_origin_pp ++
                joint_reward_member_pp ++
--                complete_tool_pp ++
                selectEffect_pp)

-- | The names and icons a focus shows in place of its usual ones, each under
-- the conditions the game shows it under. They go behind a fold: a reader wants
-- what the focus usually is, and only then what else it can be.
--
-- A focus that changes its name and its icon together writes the same conditions
-- for both, so where the two match they are shown as the one thing the focus
-- turns into rather than as two unrelated changes. The conditions are written
-- above what they bring about; a box's own fields are the focus's name and
-- description, and the description belongs to the focus however it is named.
ppAlternatives :: (HOI4Info g, Monad m) =>
    [(Text, GenericScript)] -> [(Text, GenericScript)] -> PPT g m Doc
ppAlternatives [] [] = return mempty
ppAlternatives names icons = do
    alternatives_pp'd <- mapM ppAlternative alternatives
    return . mconcat $
        ["'''", title, "'''{{collapse|", PP.line]
        ++ alternatives_pp'd ++
        ["}}"]
    where
        title
            | null icons = "Alternative Names"
            | null names = "Alternative Images"
            | otherwise = "Alternative Names and Images"
        -- Every icon, with the name that shares its conditions where there is
        -- one, and then the names left over.
        alternatives =
            [ (scr, lookup scr namesByCondition, Just theicon) | (theicon, scr) <- icons ]
            ++ [ (scr, Just nm, Nothing)
               | (nm, scr) <- names, scr `notElem` map snd icons ]
        namesByCondition = [(scr, nm) | (nm, scr) <- names]
        ppAlternative (scr, mname, micon) = do
            shown <- ppOneLine scr
            body <- case micon of
                Nothing -> return [named, PP.line]
                Just theicon -> do
                    iconfile <- getGameInterface "goal_unknown" theicon
                    return
                        [ "{{iconbox|image=", Doc.strictText iconfile, ".png", PP.line
                        , "| ", named, PP.line
                        , "|  }}", PP.line
                        ]
            return . mconcat $ [shown, "<br/>", PP.line] ++ body
            where named = maybe mempty (\nm -> mconcat ["'''", Doc.strictText nm, "'''"]) mname

-- | A trigger written as a single line, for a label with no room for a list.
ppOneLine :: (HOI4Info g, Monad m) => GenericScript -> PPT g m Doc
ppOneLine scr = do
    msgs <- ppMany scr
    texts <- mapM (\(i, msg) -> (,) i . T.strip <$> messageText msg) msgs
    return . Doc.strictText . joined $ filter (not . T.null . snd) texts
    where
        -- A list says what belongs to what by how far it is indented, which a
        -- single line has to spell out instead. A step inward reads as the line
        -- before it introducing what follows; anything else is one more thing
        -- alongside what came before.
        joined [] = ""
        joined (first:rest) = snd first <> go first rest
        go _ [] = ""
        go (outer, before) ((i, t):rest) = sep <> t <> go (i, t) rest
            where
                sep | i <= outer = ", "
                    | ":" `T.isSuffixOf` before = " "
                    | otherwise = ": "

ppPrereq :: (HOI4Info g, Monad m) => [GenericScript] -> PPT g m [Doc]
ppPrereq [] = return [""]
ppPrereq prereqs = mapM ppTitle prereqs
    where
        ppTitle prereq = do
            let reqfol = if length prereq == 1 then
                    [Doc.strictText "* Requires the following:", PP.line]
                else
                    [Doc.strictText "* Requires ''one'' of the following:", PP.line]
            reqs <- sequenceA
                [indentUp (ppScript prereq), pure PP.line
                ]
            return . mconcat $ reqfol ++ reqs

ppMutuallyExclusive :: (HOI4Info g, Monad m) => Maybe GenericScript -> PPT g m [Doc]
ppMutuallyExclusive Nothing = return [""]
ppMutuallyExclusive (Just mex) = ppTitle mex
    where
        ppTitle mexc = do
            let mexfol = mconcat [Doc.strictText "* {{icon|ExclusiveM}} Mutually exclusive with:", PP.line]
            mexcpp <- indentUp (ppScript mexc)
            let excl = [mexfol, mexcpp, PP.line]
            return excl

ppAllowBranch :: (HOI4Info g, Monad m) => Maybe GenericScript -> PPT g m [Doc]
ppAllowBranch Nothing = return [""]
ppAllowBranch (Just abr) = ppTitle abr
    where
        ppTitle awbr = do
            let awbrfol = mconcat [Doc.strictText "* Allow Branch if:", PP.line]
            awbrpp <- indentUp (ppScript awbr)
            let allwbr = [awbrfol, awbrpp, PP.line]
            return allwbr
