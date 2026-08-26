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
    ) where

import Data.Char (isAlpha, isDigit, toUpper)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Abstract (GenericScript)
import SettingsTypes (PPT, LocArg (..))
import qualified SettingsTypes as S
import qualified Doc
import MessageTools (template)
import HOI4.CountryNames (casualName)
import HOI4.Types -- everything

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
-- 'HOI4.SpecialHandlers.fillConstants' deals with instead. Text with nothing of
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
fillLocScopesFor :: (HOI4Info g, Monad m) => Maybe Text -> Text -> PPT g m Text
fillLocScopesFor mroot = fillLocScopesTo mroot 4

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
            -- Every other pronoun means whichever country the text happens to
            -- be drawn for, which nothing outside the game can say.
            Nothing | T.toLower target == "root" ->
                        maybe (return Nothing) (`countryLoc` cmd) mroot
                    | otherwise -> return Nothing
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
