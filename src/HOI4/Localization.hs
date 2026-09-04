{-|
Module      : HOI4.Localization
Description : Localization, with the names the game looks up for itself filled in

A good deal of the game's text is written with the name of a country or a state
left out of it, to be looked up as the text is drawn: @[SOV.GetAdjective]@ where
"Soviet" belongs. The words around such a reference are written to be read with
the name in place, so text with the references left in it is no use to a reader.

This module wraps the localization lookups in "SettingsTypes" so that every piece
of text HOI4 reads comes back with those names already filled in, and no caller
has to remember to ask. Import these in place of the "SettingsTypes" ones.
-}
module HOI4.Localization (
        getGameL10n
    ,   getGameL10nArgs
    ,   getGameL10nDefault
    ,   getGameL10nIfPresent
    ,   getGameL10nFor
    ,   getGameL10nIfPresentFor
    ,   fillLocScopes
    ,   fillLocScopesFor
    ,   scriptedLocVariants
    ,   getCountryName
    -- * Script atoms: tags, flags, pronouns, icons
    ,   constantValue
    ,   icon
    ,   iconText
    ,   buildingIcon
    ,   isLandmark
    ,   isTag
    ,   isPronoun
    ,   flag
    ,   flagText
    ,   eflag
    ,   tagged
    ,   allowPronoun
    ,   pronoun
    ,   scopeValText
    ,   getStateLoc
    ,   getProvinceLoc
    ,   getRegionLoc
    ,   eGetState
    ,   eGetStateText
    ,   tryLoc
    ,   tryLocAndIcon
    ,   tryLocAndIconTitle
    ,   tryLocMaybe
    ,   flagMaybeText
    ,   fillConstants
    ,   showLocVariables
    ,   getCharacterName
    ,   getCharacterRole
    ,   advisorName
    ,   mioName
    ,   mioKind
    ) where

import Control.Arrow (first)

import Data.Char (isAlpha, isAlphaNum, isDigit, isUpper, toLower, toUpper)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as T
import qualified Data.Trie as Tr
import Data.Trie (Trie)

import Abstract (GenericScript)
import SettingsTypes (PPT, LocArg (..), IsGame (..), withCurrentFile)
import qualified SettingsTypes as S
import Doc (Doc)
import qualified Doc
import MessageTools (boldText, ifThenElseT, plainNum, template, typewriterText)
import HOI4.CountryNames (casualName)
import HOI4.Messages (message, messageText, isLandmark, ScriptMessage (..))
import HOI4.Types -- everything
import HOI4.WikiTables (iconTerm, scriptIconFileTable, tagAliases)

-- | As 'S.getGameL10n', with the names the text asks the game for filled in.
getGameL10n :: (HOI4Info g, Monad m) => Text -> PPT g m Text
getGameL10n key = fillLocScopes =<< S.getGameL10n key

-- | As 'S.getGameL10nArgs', with the names the text asks the game for filled in.
getGameL10nArgs :: (HOI4Info g, Monad m) =>
    HashMap Text LocArg -> Text -> PPT g m Text
getGameL10nArgs args key = fillLocScopes =<< S.getGameL10nArgs args key

-- | As 'S.getGameL10nDefault', with the names the text asks the game for filled
-- in.
getGameL10nDefault :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m Text
getGameL10nDefault def key = fillLocScopes =<< S.getGameL10nDefault def key

-- | As 'S.getGameL10nIfPresent', with the names the text asks the game for
-- filled in.
getGameL10nIfPresent :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
getGameL10nIfPresent key = traverse fillLocScopes =<< S.getGameL10nIfPresent key

-- | As 'getGameL10n', reading @ROOT@ in what it finds as the country given. See
-- 'fillLocScopesFor'.
getGameL10nFor :: (HOI4Info g, Monad m) => Maybe Text -> Text -> PPT g m Text
getGameL10nFor mroot key = fillLocScopesFor mroot =<< S.getGameL10n key

-- | As 'getGameL10nIfPresent', reading @ROOT@ in what it finds as the country
-- given. See 'fillLocScopesFor'.
getGameL10nIfPresentFor :: (HOI4Info g, Monad m) =>
    Maybe Text -> Text -> PPT g m (Maybe Text)
getGameL10nIfPresentFor mroot key =
    traverse (fillLocScopesFor mroot) =<< S.getGameL10nIfPresent key

-- | Fill in the names a piece of localization asks the game to look up for it,
-- written as a scope and one of the game's name commands in brackets, e.g.
-- @[SOV.GetAdjective]@.
--
-- Only a scope that names one particular thing is filled in: a country tag or a
-- state id. A pronoun such as @ROOT@ means whichever country the text is drawn
-- for, which nothing outside the game can say, so those are left alone -- as is
-- any command we don't know, and the @[?constant:...]@ references that
-- 'fillConstants' deals with instead. Text with nothing of
-- ours in it comes back untouched, brackets and all, so this is safe to put in
-- front of every lookup.
fillLocScopes :: (HOI4Info g, Monad m) => Text -> PPT g m Text
fillLocScopes = fillLocScopesFor Nothing

-- | As 'fillLocScopes', reading @ROOT@ as the country given. Text the game draws
-- for one country in particular says @ROOT@ where that country's name belongs,
-- so knowing which country it is drawn for is as good as having the name
-- written out. A country can be made to go by another name while keeping the
-- same tree, but only late and rarely, and a name from the wrong end of that is
-- still plainly the same country -- which the pronoun on its own is not.
--
-- The icons the text draws from a sprite are settled here too, the two being
-- the same kind of thing: something the game fills in for itself as it draws,
-- which is no use to a reader left as written.
fillLocScopesFor :: (HOI4Info g, Monad m) => Maybe Text -> Text -> PPT g m Text
fillLocScopesFor mroot text = fillIconFrames <$> fillLocScopesTo mroot 4 text

-- | The resource each frame of the game's resource sprite stands for, from the
-- @icon_frame@ each is given in @common/resources@.
resourceFrames :: HashMap Text Text
resourceFrames = HM.fromList
    [("1", "oil")
    ,("2", "aluminium")
    ,("3", "rubber")
    ,("4", "tungsten")
    ,("5", "steel")
    ,("6", "chromium")
    ,("7", "coal")
    ]

-- | Fill in the icons a piece of text draws from a sprite. Localization points
-- at one frame of a strip -- @£resources_strip|3@, where the third frame is
-- rubber -- and the wiki has an icon of its own for the thing the frame shows.
-- A sprite or a frame we know nothing about is left as it is written.
fillIconFrames :: Text -> Text
fillIconFrames = go
    where
        go t = case T.breakOn marker t of
            (before, rest) | T.null rest -> before
            (before, rest) ->
                let body = T.drop 1 rest
                    sprite = T.takeWhile isNameChar body
                    -- The frame is written after the sprite's name and is a
                    -- number, so whatever follows the digits is the text going
                    -- on around the reference rather than part of it.
                    (frame, after) = case T.stripPrefix "|" (T.drop (T.length sprite) body) of
                        Just digits -> T.span isDigit digits
                        Nothing -> ("", T.drop (T.length sprite) body)
                in before <> named sprite frame <> go after
        marker = T.singleton '\xa3'
        isNameChar c = isAlphaNum c || c `elem` ("._-" :: String)
        named sprite frame
            -- Script writes the sprite's name with or without the @GFX_@ the
            -- game files give it.
            | fromMaybe sprite (T.stripPrefix "GFX_" sprite) == "resources_strip"
            , Just res <- HM.lookup frame resourceFrames
                = Doc.doc2text (template "icon" [res, "1"])
            | otherwise = marker <> sprite
                            <> ifThenElseT (T.null frame) "" ("|" <> frame)

-- | As 'fillLocScopesFor', counting down how many times a text filled in may
-- name a scripted localization of its own. Script nests them a level or two -- a
-- tooltip naming a modifier whose own name is worked out the same way -- and the
-- count is what stops a name written in terms of itself going round forever.
fillLocScopesTo :: (HOI4Info g, Monad m) => Maybe Text -> Int -> Text -> PPT g m Text
fillLocScopesTo mroot depth text = case T.breakOn "[" text of
    (_, rest) | T.null rest -> return text
    (before, rest) -> case T.stripPrefix "]" closing of
        -- Unterminated bracket: nothing sensible to do, leave the rest as it is.
        Nothing -> return text
        Just after -> do
            mname <- scopeName mroot depth inner
            filled <- fillLocScopesTo mroot depth after
            return $ before <> fromMaybe ("[" <> inner <> "]") mname <> filled
        where (inner, closing) = T.breakOn "]" (T.drop 1 rest)

-- | What one bracketed scope command comes to, or 'Nothing' for one we cannot
-- work out. See 'fillLocScopes'.
scopeName :: (HOI4Info g, Monad m) => Maybe Text -> Int -> Text -> PPT g m (Maybe Text)
scopeName mroot depth inner
    -- A bracket opening with @?@ names a value the game works out as it draws
    -- the text, which nothing outside the game can say -- except where what is
    -- named is a number written out in the script itself, which is the same
    -- number however it is read. The format after the @|@ says how the game
    -- writes it: @[?-5|%%-]@ is five percent off.
    | Just asked <- T.stripPrefix "?" inner
    , (value, fmt) <- T.breakOn "|" asked
    , Just n <- readNumber value
    = return . Just . S.formatLocNumber (T.drop 1 fmt) $ n
scopeName mroot depth inner = case T.stripPrefix "." cmdrest of
    -- Nothing is being asked of a scope, so the brackets may instead name a
    -- scripted localization: the game picking between several texts as it draws
    -- this one.
    Nothing -> scriptedLocDefault mroot depth inner
    -- A state is named by the id it is scripted under, and has only the one
    -- name: no ruling party to vary it and no article to put in front of it.
    -- Anything else asked of a state is a scripted localization worked out in
    -- it, as it is for a country.
    Just cmd | not (T.null target), T.all isDigit target ->
        if T.toLower cmd `elem` ["getname", "getnamecap", "getnamedef", "getnamedefcap"]
            then S.getGameL10nIfPresent ("STATE_" <> target)
            else scriptedLocDefault mroot depth (slocNamed cmd)
    Just cmd -> do
        mtag <- countryTag target
        named <- case mtag of
            Just tag -> countryLoc tag cmd
            -- A pronoun means whatever the game put there as it draws the
            -- text, which in general nothing outside the game can say. But
            -- within a decision or an event the writers say what the pronouns
            -- stand for where they can ('withRootIdent' and its kin), and a
            -- ROOT given by the caller outranks even that. A pronoun known
            -- only by its role is left as written: its wording names the role
            -- where the pronoun stands in a list, but spliced into running
            -- text it would read as a name, which it is not.
            Nothing -> do
                mval <- case T.toLower target of
                    "root" -> maybe getRootIdent (return . Just . ScopeValTag) mroot
                    "from" -> getFromIdent
                    "this" -> getThisIdent
                    "prev" -> getPrevIdent
                    _ -> return Nothing
                case mval of
                    Just (ScopeValTag tag) -> countryLoc tag cmd
                    Just (ScopeValState n)
                        | T.toLower cmd `elem` ["getname", "getnamecap", "getnamedef", "getnamedefcap"]
                        -> S.getGameL10nIfPresent ("STATE_" <> T.pack (show n))
                    _ -> return Nothing
        -- What is asked of the scope may be a scripted localization rather
        -- than a name: @[THIS.GetAntiSovietFocusName]@ works its text out in
        -- the scope written in front of it. Which scope that is settles which
        -- of the texts the game picks, not what any of them says, so the one
        -- it settles on can be given whether or not we know the scope.
        case named of
            Just name -> return (Just name)
            Nothing -> scriptedLocDefault mroot depth (slocNamed cmd)
    where
        (target, cmdrest) = T.breakOn "." inner
        -- Script steps through a scope or two on the way to what it asks for
        -- -- @[THIS.OWNER.X]@ -- and no scripted localization is named with a
        -- dot in it, so the name is whatever follows the last one.
        slocNamed = snd . T.breakOnEnd "."

-- | The number a piece of text spells out, if the whole of it is one.
readNumber :: Text -> Maybe Double
readNumber t = case T.signed T.double t of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

-- | The text a scripted localization settles on -- the one the game shows when
-- none of the conditions written for the others hold.
scriptedLocDefault :: (HOI4Info g, Monad m) =>
    Maybe Text -> Int -> Text -> PPT g m (Maybe Text)
scriptedLocDefault mroot depth name
    | depth <= 0 = return Nothing
    | otherwise = do
        slocs <- getScriptedLoc
        case scriptedLocFallback =<< HM.lookup name slocs of
            Nothing -> return Nothing
            -- A text written with no key of its own is the game showing nothing
            -- where it stands: the words around it are meant to be read with
            -- the gap, not with the name of the lookup sitting in it.
            Just txt | T.null (sloc_key txt) -> return (Just "")
                     | otherwise -> traverse (fillLocScopesTo mroot (depth - 1))
                                        =<< S.getGameL10nIfPresent (sloc_key txt)

-- | Which of a scripted localization's texts is the one it settles on: the
-- first that names no conditions of its own, since the game reads them in the
-- order they are written and stops at the first whose conditions hold.
--
-- Where every one of them names conditions, the game shows nothing at all
-- unless a set of them holds, and script is written with the ordinary case
-- first: @GetTheUSSRName@ asks whether the Soviets are communist before it
-- asks whether they are not, and they are, at the start of every game. So the
-- first is the one to give.
scriptedLocFallback :: [HOI4ScriptedLocText] -> Maybe HOI4ScriptedLocText
scriptedLocFallback texts = case filter (isNothing . sloc_trigger) texts of
    (txt:_) -> Just txt
    [] -> listToMaybe texts

-- | The texts a piece of localization can come to besides the one it settles
-- on, each with the conditions the game uses it under. A piece of localization
-- has these only where the whole of it is a reference to a scripted
-- localization, e.g. @[CZE_continue_with_snejdareks_plan]@; anything else says
-- the one thing however it is read.
scriptedLocVariants :: (HOI4Info g, Monad m) =>
    Maybe Text -> Text -> PPT g m [(Text, GenericScript)]
scriptedLocVariants mroot key = do
    raw <- S.getGameL10n key
    case T.stripSuffix "]" =<< T.stripPrefix "[" (T.strip raw) of
        Nothing -> return []
        -- Script writes the scope it is worked out in before the name where it
        -- writes one at all, and no scripted localization is named with a dot
        -- in it, so whatever follows the last one is the name.
        Just inner -> do
            let name = snd (T.breakOnEnd "." inner)
            slocs <- getScriptedLoc
            let conditional = mapMaybe withTrigger (HM.lookupDefault [] name slocs)
            mapMaybe sequenceLoc <$> traverse localize conditional
    where
        withTrigger txt = (,) (sloc_key txt) <$> sloc_trigger txt
        localize (lockey, trig) = (,)
            <$> (traverse (fillLocScopesFor mroot) =<< S.getGameL10nIfPresent lockey)
            <*> pure trig
        sequenceLoc (mloc, trig) = (,) <$> mloc <*> pure trig

-- | The country a bracketed scope names, if it names one. Script writes a tag in
-- whatever case the writer felt like and the game reads it either way, so the
-- case is not what tells us: a three-letter word the wiki's table or the game
-- knows a country by is.
-- Asking that much keeps an ordinary word that happens to be three letters long
-- from being read as a tag on the strength of a localization key alone.
countryTag :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
countryTag target
    | T.length target /= 3 || not (T.all isAlpha target) = return Nothing
    | otherwise = do
        inHistory <- HM.member tag <$> getCountryHistory
        named <- traverse S.getGameL10nIfPresent [tag <> "_DEF", tag <> "_ADJ"]
        return $ if isJust (casualName tag)
                    || inHistory || any (maybe False (not . T.null)) named
                    then Just tag
                    else Nothing
    where tag = T.toUpper target

-- | The name a country goes by, in the form the given name command asks for. A
-- country is named for the party that rules it at the start of the game, which
-- is the name the game itself shows until the party changes, so the key that
-- party gives is tried before the plain one. Each form falls back on the ones
-- before it, since a country the game has little to say about has only its plain
-- name.
--
-- A command that asks only that the country be named -- by name, by flag, or
-- both -- is answered from the wiki's own table first ('casualName'), where its
-- editors keep what they call each country. A command that asks for one
-- particular form of the name -- an adjective, the name apart from any
-- ideology -- is the game being specific, and keeps to the game's localization.
--
-- The game reads these command names without regard to case, and the script is
-- written every which way, so they are matched the same way here.
countryLoc :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m (Maybe Text)
countryLoc tag cmd
    | lcmd == "gettag" = return (Just tag)
    | otherwise = do
        ideoTag <- getCoHi tag
        let withIdeology suffix = [ideoTag <> suffix, tag <> suffix]
            plain = withIdeology ""
            keys = case form of
                "getname" -> plain
                "getnamedef" -> withIdeology "_DEF" ++ plain
                "getadjective" -> withIdeology "_ADJ" ++ plain
                -- The name a country goes by whoever rules it, which is what the
                -- ideology-specific ones are variations on.
                "getnonideologyname" -> [tag]
                "getnonideologyadjective" -> [tag <> "_ADJ", tag]
                "getflag" -> plain
                "getnamewithflag" -> plain
                _ -> []
        mcasual <- casualLoc ideoTag
        mloc <- maybe (firstLoc keys) (return . Just) mcasual
        return $ flip fmap mloc $ \loc -> case form of
            -- The wiki's flag template draws a country's flag and its name, the
            -- same two things the game draws for a name with a flag. Its @0@
            -- argument leaves the name off, for the flag on its own.
            "getnamewithflag" -> Doc.doc2text (template "flag" [loc])
            "getflag" -> Doc.doc2text (template "flag" [loc, "0"])
            _ | capitalized -> capitalize loc
              | otherwise -> loc
    where
        lcmd = T.toLower cmd
        -- The commands ending in "Cap" ask for the same name with a capital on
        -- the front, for one that opens a sentence: the definite form starts
        -- with the article where the country takes one ("the Soviet Union").
        (form, capitalized) = case T.stripSuffix "cap" lcmd of
            Just stem -> (stem, True)
            Nothing -> (lcmd, False)
        -- The table's name for the country, in the form asked for, where the
        -- command is one the table answers and the table has an entry. The
        -- definite form is the table's name with its article on the front,
        -- found by way of whichever key the game writes that name under; a
        -- name the game never writes has no article to find, and a country
        -- whose name carries no article is written the same with or without
        -- one, so the bare name serves for both.
        casualLoc ideoTag = case casualName tag of
            Nothing -> return Nothing
            Just name
                | form `elem` ["getname", "getflag", "getnamewithflag"] ->
                    return (Just name)
                | form == "getnamedef" -> do
                    mkey <- keyNaming name
                        (ideoTag : tag : map (\i -> tag <> "_" <> i) ideologies)
                    case mkey of
                        Nothing -> return (Just name)
                        Just key -> Just . fromMaybe name
                                        <$> firstLoc [key <> "_DEF"]
                | otherwise -> return Nothing
        keyNaming _ [] = return Nothing
        keyNaming name (key:rest) = do
            mloc <- S.getGameL10nIfPresent key
            if mloc == Just name
                then return (Just key)
                else keyNaming name rest
        -- The raw lookup is used throughout, since filling one name in is no
        -- reason to go looking for another inside it.
        firstLoc [] = return Nothing
        firstLoc (key:rest) = do
            mloc <- S.getGameL10nIfPresent key
            case mloc of
                Just loc | not (T.null loc) -> return (Just loc)
                _ -> firstLoc rest
        capitalize t = case T.uncons t of
            Just (c, rest) -> T.cons (toUpper c) rest
            Nothing -> t

-- | The party keys a country's names may be written under, one per ideology.
ideologies :: [Text]
ideologies = ["democratic", "neutrality", "fascism", "communism"]

-- | The name to put to a country where the text is ours rather than the
-- game's: a flag template, a heading over a tree. The wiki's table has the
-- first word; a tag it has no entry for goes by the name it starts the game
-- under.
getCountryName :: (HOI4Info g, Monad m) => Text -> PPT g m Text
getCountryName tag = case casualName tag of
    Just name -> return name
    Nothing -> S.getGameL10n =<< getCoHi tag

-- | The key a country's name is written under at the start of the game.
--
-- A country whose history gives it a cosmetic tag goes by the name that tag
-- names -- the Dutch East Indies rather than Indonesia -- and either that name
-- or the country's own may vary by the party in power, so the party's key is
-- tried before the plain one on both. What is left is the tag itself.
getCoHi :: (Monad m, HOI4Info g) =>
    Text -> PPT g m Text
getCoHi name = do
    chistories <- getCountryHistory
    case HM.lookup name chistories of
        Nothing -> return name
        Just chistory -> firstNamed (candidates chistory)
    where
        firstNamed [] = return name
        firstNamed (key:rest) = do
            mloc <- S.getGameL10nIfPresent key
            case mloc of
                Just loc | not (T.null loc) -> return key
                _ -> firstNamed rest
        candidates chistory = concat
            [ [ cosmetic <> party | cosmetic <- cosmetics ]
            , cosmetics
            -- A history that sets no ruling party leaves us with none to ask
            -- for, and a cosmetic name is often written under one party alone.
            -- Whichever of them the game files spell out, a name the tag was
            -- given is nearer the mark than the country's own.
            , [ cosmetic <> "_" <> ideology | cosmetic <- cosmetics, ideology <- ideologies ]
            , [ chRulingTag chistory ]
            ]
            where
                cosmetics = maybe [] (:[]) (chCosmeticTag chistory)
                party = fromMaybe "" (T.stripPrefix name (chRulingTag chistory))


-------------------------------------------------------------
-- Tags, flags, pronouns and other script-atom localization --
-------------------------------------------------------------

-- | The number script names by naming a script constant, or 'Nothing' if the name
-- is not a constant holding one. An effect that documents a constant for one of
-- its fields takes the name bare; anywhere a variable would go, script writes the
-- @constant:@ prefix instead, and both name the same thing.
constantValue :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Double)
constantValue name = HM.lookup path <$> getScriptConstants
    where path = fromMaybe name (T.stripPrefix "constant:" name)

-- Emit icon template.
icon :: Text -> Doc
icon what = case HM.lookup what scriptIconFileTable of
    Just "" -> Doc.strictText $ "[[File:" <> what <> ".png|28px]]" -- shorthand notation
    Just file -> Doc.strictText $ "[[File:" <> file <> ".png|28px]]"
    _ ->  if isPronoun what then
            ""
        else
            -- The "1" parameter makes the wiki template append its localized
            -- term for the icon, so callers must not add the name themselves.
            template "icon" [iconTerm what, "1"]
iconText :: Text -> Text
iconText = Doc.doc2text . icon

-- | How a building is shown wherever script names one: its icon, which on the
-- wiki carries the building's name with it. Every handler that names a
-- building goes through here, so they all show one the same way.
--
-- The landmarks are the exception: the wiki draws them all with one icon, so
-- a landmark is shown as that icon followed by its own name.
buildingIcon :: (HOI4Info g, Monad m) => Text -> PPT g m Text
buildingIcon bld
    | isLandmark bld = do
        name <- getGameL10n bld
        return $ Doc.doc2text (template "icon" ["landmark"]) <> " " <> name
    | otherwise = return (iconText bld)

-- Argument may be a tag or a tagged variable. Emit a flag in the former case,
-- and localize in the latter case.
eflag :: (HOI4Info g, Monad m) =>
            Maybe HOI4Scope -> Either Text (Text, Text) -> PPT g m (Maybe Text)
eflag expectScope = \case
    Left name -> Just <$> flagText expectScope name
    Right (vartag, var) -> tagged vartag var

-- | Look up the message corresponding to a tagged atom.
--
-- For example, to localize @event_target:some_name@, call
-- @tagged "event_target" "some_name"@.
tagged :: (HOI4Info g, Monad m) =>
    Text -> Text -> PPT g m (Maybe Text)
tagged vartag var = case flip Tr.lookup varTags . TE.encodeUtf8 $ vartag of
    Just msg -> Just <$> messageText (msg var)
    Nothing -> return $ Just $ typewriterText (vartag <> ":" <> var) -- just let it pass

flagText :: (HOI4Info g, Monad m) =>
    Maybe HOI4Scope -> Text -> PPT g m Text
flagText expectScope = fmap Doc.doc2text . flag expectScope

-- Emit an appropriate phrase if the given text is a pronoun, otherwise use the
-- provided localization function.
allowPronoun :: (HOI4Info g, Monad m) =>
    Maybe HOI4Scope -> (Text -> PPT g m Doc) -> Text -> PPT g m Doc
allowPronoun expectedScope getLoc name =
    if isPronoun name
        then pronoun expectedScope name
        else getLoc name

-- | Emit flag template if the argument is a tag, or an appropriate phrase if
-- it's a pronoun.
flag :: (HOI4Info g, Monad m) =>
    Maybe HOI4Scope -> Text -> PPT g m Doc
flag expectscope = allowPronoun expectscope $ \name ->
    case HM.lookup name tagAliases of
        -- A tag alias is not a country of its own -- the game resolves it to
        -- one as the script runs -- so the table says how the wiki names what
        -- it stands for.
        Just aliastext -> return $ Doc.strictText aliastext
        Nothing -> template "flag" . (:[]) <$> getCountryName name

-- | Emit an appropriate phrase for a pronoun.
-- If a scope is passed, that is the type the current command expects. If they
-- don't match, it's a synecdoche; adjust the wording appropriately.
--
-- All handlers in this module that take an argument of type 'Maybe HOI4Scope'
-- call this function. Use whichever scope corresponds to what you expect to
-- appear on the RHS. If it can be one of several (e.g. either a country or a
-- province), use HOI4From. If it doesn't correspond to any scope, use Nothing.
-- | The wiki text for what a pronoun stands for, as 'getFromIdent' and its
-- kin know it: the named country or state, or the wording for its role.
scopeValText :: (HOI4Info g, Monad m) => HOI4ScopeVal -> PPT g m Text
scopeValText (ScopeValTag tag) = flagText (Just HOI4Country) tag
scopeValText (ScopeValState n) = getStateLoc n
scopeValText (ScopeValRole _ txt) = return txt

pronoun :: (HOI4Info g, Monad m) =>
    Maybe HOI4Scope -> Text -> PPT g m Doc
pronoun expectedScope name = withCurrentFile $ \f -> case T.toLower name of
    "root" -> message MsgROOTCountry
    "prev" -> getPrevIdent >>= \case
      Just val -> Doc.strictText <$> scopeValText val
      Nothing ->
        getPrevScope >>= \case -- will need editing
            Just HOI4Country
                | expectedScope `matchScope` HOI4Country -> message MsgPREVCountry
                | otherwise                             -> return "PREV"
            Just HOI4ScopeState
                | expectedScope `matchScope` HOI4ScopeState -> message MsgPREVState
                | otherwise                             -> return "PREV"
            Just HOI4UnitLeader
                | expectedScope `matchScope` HOI4UnitLeader -> message MsgPREVUnitLeader
                | otherwise                             -> return "PREV"
            Just HOI4Operative
                | expectedScope `matchScope` HOI4Operative -> message MsgPREVOperative
                | otherwise                             -> return "PREV"
            Just HOI4ScopeCharacter
                | expectedScope `matchScope` HOI4ScopeCharacter -> message MsgPREVCharacter
                | otherwise                             -> return "PREV"
            Just HOI4Division
                | expectedScope `matchScope` HOI4Division -> message MsgPREVDivision
                | otherwise                             -> return "PREV"
            Just HOI4From -> message MsgPREVFROM
            Just HOI4Misc -> message MsgMISC
            Just HOI4Custom -> message MsgPREVCustom
            _ -> return "PREV"
    "this" -> getCurrentScope >>= \case -- will need editing
        Just HOI4Country
            | expectedScope `matchScope` HOI4Country -> message MsgTHISCountry
            | otherwise                             -> return "THIS"
        Just HOI4ScopeState
            | expectedScope `matchScope` HOI4ScopeState -> message MsgTHISState
            | otherwise                             -> return "THIS"
        Just HOI4UnitLeader
            | expectedScope `matchScope` HOI4UnitLeader -> message MsgTHISUnitLeader
            | otherwise                             -> return "THIS"
        Just HOI4Operative
            | expectedScope `matchScope` HOI4Operative -> message MsgTHISOperative
            | otherwise                             -> return "THIS"
        Just HOI4ScopeCharacter
            | expectedScope `matchScope` HOI4ScopeCharacter -> message MsgTHISCharacter
            | otherwise                             -> return "THIS"
        Just HOI4Division
            | expectedScope `matchScope` HOI4Division -> message MsgTHISDivision
            | otherwise                             -> return "THIS"
        Just HOI4Misc -> message MsgMISC
        Just HOI4Custom -> message MsgPREVCustom
        _ -> return "THIS"
    "overlord" -> message MsgOverlord
    "faction_leader" -> message MsgFactionLeader
    "owner" -> getCurrentScope >>= \case
        Just HOI4ScopeState -> message MsgOwnerState
        Just HOI4UnitLeader -> message MsgOwnerUnit
        Just HOI4Operative -> message MsgOwnerUnit
        Just HOI4ScopeCharacter -> message MsgOwnerUnit
        _ -> message MsgOwner
    "controller" -> message MsgController
    "capital_scope" -> message MsgCapital
    -- Who FROM is depends on where the script stands -- the target of a
    -- targeted decision, whoever fired the event -- and the writers of those
    -- features say so where they can ('withFromIdent').
    "from" -> getFromIdent >>= \case
        Just val -> Doc.strictText <$> scopeValText val
        Nothing -> message MsgFROM
    proscope
        | any (`T.isSuffixOf` proscope) [".overlord",".OVERLORD",".Overlord"] -> do
            let labelstrip
                    | ".overlord" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".overlord" proscope)
                    | ".Overlord" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".Overlord" proscope)
                    | ".OVERLORD" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".OVERLORD" proscope)
                    | otherwise = proscope
                tagorpro = if T.length labelstrip == 3 then T.toUpper labelstrip else labelstrip
            tagloc <- do
                mflag <- eflag (Just HOI4Country) (Left tagorpro)
                return $ fromMaybe "<!--CHECK SCRIPT-->" mflag
            message $ MsgOverlordOf tagloc
        | any (`T.isSuffixOf` proscope) [".faction_leader",".FACTION_LEADER",".Faction_leader"] -> do
            let labelstrip
                    | ".faction_leader" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".faction_leader" proscope)
                    | ".Faction_leader" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".Faction_leader" proscope)
                    | ".FACTION_LEADER" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".FACTION_LEADER" proscope)
                    | otherwise = proscope
                tagorpro = if T.length labelstrip == 3 then T.toUpper labelstrip else labelstrip
            tagloc <- do
                mflag <- eflag (Just HOI4Country) (Left tagorpro)
                return $ fromMaybe "<!--CHECK SCRIPT-->" mflag
            message $ MsgFactionLeaderOf tagloc
        | any (`T.isSuffixOf` proscope) [".owner",".OWNER",".Owner"] -> do
            let labelstrip
                    | ".owner" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".owner" proscope)
                    | ".Owner" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".Owner" proscope)
                    | ".OWNER" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".OWNER" proscope)
                    | otherwise = proscope
            stateloc <-
                if all isDigit $ T.unpack labelstrip
                then getStateLoc $ read (T.unpack labelstrip)
                else do
                mstate <- eGetState (Left labelstrip)
                return $ fromMaybe "<!--CHECK SCRIPT-->" mstate
            message $ MsgOwnerOf stateloc
        | any (`T.isSuffixOf` proscope) [".controller",".CONTROLLER",".Controller"] -> do
            let labelstrip
                    | ".controller" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".controller" proscope)
                    | ".Controller" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".Controller" proscope)
                    | ".CONTROLLER" `T.isSuffixOf` proscope = fromMaybe "<!--CHECK SCRIPT-->" (T.stripSuffix ".CONTROLLER" proscope)
                    | otherwise = proscope
            stateloc <-
                if all isDigit $ T.unpack labelstrip
                then getStateLoc $ read (T.unpack labelstrip)
                else do
                mstate <- eGetState (Left labelstrip)
                return $ fromMaybe "<!--CHECK SCRIPT-->" mstate
            message $ MsgControllerOf stateloc
        | otherwise -> return $ Doc.strictText name -- something else; regurgitate untouched
    where
        Nothing `matchScope` _ = True
        Just expect `matchScope` actual
            | expect == actual = True
            | otherwise        = False

isTag :: Text -> Bool
isTag s = T.length s == 3 && T.all isUpper s

-- Tagged messages
varTags :: Trie (Text -> ScriptMessage)
varTags = Tr.fromList . map (first TE.encodeUtf8) $
    [("event_target", MsgEventTargetVar)
    ,("var"         , MsgVariable)
    ]

isPronoun :: Text -> Bool
isPronoun s = T.map toLower s `Set.member` pronouns || (\ls -> ".owner" `T.isSuffixOf` ls || ".controller" `T.isSuffixOf` ls || ".faction_leader" `T.isSuffixOf` ls || ".overlord" `T.isSuffixOf` ls || ".prev" `T.isSuffixOf` ls || ".from" `T.isSuffixOf` ls) (T.toLower s)
    where
        pronouns = Set.fromList
            ["root"
            ,"prev"
            ,"this"
            ,"from"
            ,"overlord"
            ,"faction_leader"
            ,"owner"
            ,"controller"
            ,"capital_scope"
            ]

-- Get the localization for a state ID, if available.
getStateLoc :: (HOI4Info g, Monad m) =>
    Int -> PPT g m Text
getStateLoc n = do
    let stateid_t = T.pack (show n)
    mstateloc <- getGameL10nIfPresent ("STATE_" <> stateid_t)
    case mstateloc of
        -- the wiki's state template renders as bold name + id in parentheses
        Just _ -> return $ Doc.doc2text (template "state" [stateid_t])
        -- Some scripts (e.g. prioritize lists) mix province ids in with state
        -- ids; try the victory point name for those
        _ -> getProvinceLoc n

-- | Get the display text for a province id: its victory point name where it
-- has one, always with the id itself so the reader can find it on the map.
getProvinceLoc :: (HOI4Info g, Monad m) =>
    Int -> PPT g m Text
getProvinceLoc n = do
    let provid_t = T.pack (show n)
    mvploc <- getGameL10nIfPresent ("VICTORY_POINTS_" <> provid_t)
    return $ case mvploc of
        Just vploc -> boldText vploc <> " (province " <> provid_t <> ")"
        _ -> "province (" <> provid_t <> ")"

eGetState :: (HOI4Info g, Monad m) =>
             Either Text (Text, Text) -> PPT g m (Maybe Text)
eGetState = \case
    Left name -> do
        pronouned <- pronoun (Just HOI4ScopeState) name
        let pronountext = Doc.doc2text pronouned
        return $ Just pronountext
    Right (vartag, var) -> tagged vartag var

-- | As 'eGetState', falling back on a marker for a human to fill in where
-- nothing can be worked out.
eGetStateText :: (HOI4Info g, Monad m) => Either Text (Text, Text) -> PPT g m Text
eGetStateText = fmap (fromMaybe "<!-- Check Script -->") . eGetState

-- convenience synonym
tryLoc :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
tryLoc = getGameL10nIfPresent

-- | Get icon and localization for the atom given. Return @mempty@ if there is
-- no icon, and wrapped in @<tt>@ tags if there is no localization.
tryLocAndIcon :: (HOI4Info g, Monad m) => Text -> PPT g m (Text,Text)
tryLocAndIcon atom = do
    loc <- tryLoc atom
    return (fromMaybe mempty (Just (iconText atom)),
            fromMaybe (typewriterText atom) loc)

-- | Get localization for the atom given. Return atom
-- if there is no localization.
tryLocMaybe :: (HOI4Info g, Monad m) => Text -> PPT g m (Text,Text)
tryLocMaybe atom = do
    loc <- tryLoc atom
    return ("", fromMaybe atom loc)

getRegionLoc :: (HOI4Info g, Monad m) =>
    Int -> PPT g m Text
getRegionLoc n = do
    let regionid_t = T.pack (show n)
    mregionloc <- getGameL10nIfPresent ("STRATEGICREGION_" <> regionid_t)
    return $ case mregionloc of
        Just loc -> boldText loc <> " (" <> regionid_t <> ")"
        _ -> "Region" <> regionid_t

-- | The flag or localized name behind a country atom, or 'Nothing' where it
-- cannot be worked out.
flagMaybeText :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
flagMaybeText txt = eflag (Just HOI4Country) (Left txt)

-- | As 'tryLocAndIcon', localizing the @_title@ key of the atom given.
tryLocAndIconTitle :: (HOI4Info g, Monad m) => Text -> PPT g m (Text, Text)
tryLocAndIconTitle t = tryLocAndIcon (t <> "_title")

-- | Fill in every script constant a piece of localization refers to in brackets.
-- The game reads those as it draws the text, and unlike the variables that share
-- the same bracket syntax, a constant is the same number whenever it is read, so
-- it can be filled in here as well. A reference to something that is not a
-- constant holding a number is left as it stands for a human to deal with.
fillConstants :: (HOI4Info g, Monad m) => Text -> PPT g m Text
fillConstants text = do
    constants <- getScriptConstants
    return (fill constants text)
    where
        marker = "[?constant:"
        fill constants t = case T.breakOn marker t of
            (_, rest) | T.null rest -> t
            (before, rest) ->
                let afterMarker = T.drop (T.length marker) rest
                    (path, closing) = T.breakOn "]" afterMarker
                in case (HM.lookup path constants, T.stripPrefix "]" closing) of
                    (Just val, Just after) ->
                        before <> Doc.doc2text (plainNum val) <> fill constants after
                    -- Whatever this refers to, the marker itself is done with, so
                    -- what follows is searched without it and the recursion ends.
                    _ -> before <> marker <> fill constants afterMarker

-- | Show every game variable a piece of localization still names in brackets by
-- its own name. What is left in brackets by the time this is reached is a value
-- the game works out as it draws the text -- a variable some effect sets while
-- the game is being played -- and nothing outside the game can say what it
-- holds, so the name is all there is to show. It is written the way every other
-- variable this program cannot resolve is, in a typewriter face.
--
-- The format a reference may carry after a @|@ says how the game writes the
-- number it finds, which says nothing about a name, so it is dropped.
showLocVariables :: Text -> Text
showLocVariables = go
    where
        marker = "[?"
        go t = case T.breakOn marker t of
            (_, rest) | T.null rest -> t
            (before, rest) ->
                let body = T.drop (T.length marker) rest
                    (inner, closing) = T.breakOn "]" body
                in case T.stripPrefix "]" closing of
                    -- Unterminated: the marker itself is done with, so what
                    -- follows is searched without it and the recursion ends.
                    Nothing -> before <> marker <> go body
                    Just after ->
                        before <> typewriterText (T.takeWhile (/= '|') inner) <> go after

getCharacterName :: (Monad m, HOI4Info g) =>
    Text -> PPT g m Text
getCharacterName idn = do
    characters <- getCharacters
    case HM.lookup idn characters of
        Just charid -> return $ cha_loc_name charid
        _ -> getGameL10n idn

-- | The post a character is commissioned into, worded as the game words it
-- where it gives a country that commander: @becomes a General@ and the like.
-- A character written for no military post has nothing to say here, and one
-- written for two is named for both, there being nothing in the script to say
-- which of them a tooltip has in mind.
getCharacterRole :: (Monad m, HOI4Info g) =>
    Text -> PPT g m Text
getCharacterRole idn = do
    characters <- getCharacters
    return $ case HM.lookup idn characters of
        Just charid -> T.intercalate " and " (mapMaybe roleName (cha_unit_roles charid))
        _ -> ""
    where
        roleName = \case
            "field_marshal" -> Just "a Field Marshal"
            "corps_commander" -> Just "a General"
            "navy_leader" -> Just "an Admiral"
            _ -> Nothing

-- | The name behind whatever an advisor statement is pointed at. Script names an
-- advisor either by the token their post is known by or by the character's own
-- id, and either way it is the person's name a reader wants.
advisorName :: (HOI4Info g, Monad m) => Text -> PPT g m Text
advisorName token = do
    charto <- getCharToken
    case HM.lookup token charto of
        Just adv -> getCharacterName (adv_cha_id adv)
        Nothing -> getCharacterName token

-- | The name of an organization. Most are localized under their own token; the
-- rest are named by the key their entry gives.
mioName :: (HOI4Info g, Monad m) => Text -> PPT g m Text
mioName token = do
    names <- getMioNames
    mloc <- getGameL10nIfPresent token
    case mloc of
        Just loc -> return (Doc.oneLine loc)
        Nothing -> case HM.lookup token names of
            Just key -> Doc.oneLine <$> getGameL10n key
            Nothing -> return (typewriterText token)

-- | What kind of manufacturer an organization is, which is said by the archetype
-- its entry is built out of rather than by anything of its own. An organization
-- written out in full has no archetype and so nothing to say here.
mioKind :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
mioKind token = do
    includes <- getMioIncludes
    case HM.lookup token includes of
        Nothing -> return Nothing
        Just archetype -> fmap Doc.oneLine <$> getGameL10nIfPresent archetype
