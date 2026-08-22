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
    ,   fillLocScopes
    ,   getCoHi
    ) where

import Data.Char (isAlpha, isDigit, toUpper)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import SettingsTypes (PPT, LocArg (..))
import qualified SettingsTypes as S
import qualified Doc
import MessageTools (template)
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
fillLocScopes text = case T.breakOn "[" text of
    (_, rest) | T.null rest -> return text
    (before, rest) -> case T.stripPrefix "]" closing of
        -- Unterminated bracket: nothing sensible to do, leave the rest as it is.
        Nothing -> return text
        Just after -> do
            mname <- scopeName inner
            filled <- fillLocScopes after
            return $ before <> fromMaybe ("[" <> inner <> "]") mname <> filled
        where (inner, closing) = T.breakOn "]" (T.drop 1 rest)

-- | What one bracketed scope command comes to, or 'Nothing' for one we cannot
-- work out. See 'fillLocScopes'.
scopeName :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
scopeName inner = case T.stripPrefix "." cmdrest of
    Nothing -> return Nothing
    -- A state is named by the id it is scripted under, and has only the one
    -- name: no ruling party to vary it and no article to put in front of it.
    Just cmd | not (T.null target), T.all isDigit target ->
        if T.toLower cmd `elem` ["getname", "getnamecap", "getnamedef", "getnamedefcap"]
            then S.getGameL10nIfPresent ("STATE_" <> target)
            else return Nothing
    Just cmd -> do
        mtag <- countryTag target
        maybe (return Nothing) (`countryLoc` cmd) mtag
    where (target, cmdrest) = T.breakOn "." inner

-- | The country a bracketed scope names, if it names one. Script writes a tag in
-- whatever case the writer felt like and the game reads it either way, so the
-- case is not what tells us: a three-letter word the game knows a country by is.
-- Asking that much keeps an ordinary word that happens to be three letters long
-- from being read as a tag on the strength of a localization key alone.
countryTag :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe Text)
countryTag target
    | T.length target /= 3 || not (T.all isAlpha target) = return Nothing
    | otherwise = do
        inHistory <- HM.member tag <$> getCountryHistory
        named <- traverse S.getGameL10nIfPresent [tag <> "_DEF", tag <> "_ADJ"]
        return $ if inHistory || any (maybe False (not . T.null)) named
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
        mloc <- firstLoc keys
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

-- | The key a country's name is written under, which is the one the party ruling
-- it at the start of the game gives if there is such a key, and the tag itself
-- otherwise.
getCoHi :: (Monad m, HOI4Info g) =>
    Text -> PPT g m Text
getCoHi name = do
    chistories <- getCountryHistory
    let mchistories = HM.lookup name chistories
    case mchistories of
        Nothing -> return name
        Just chistory -> do
            rulLoc <- S.getGameL10nIfPresent (chRulingTag chistory)
            case rulLoc of
                Just rulingTag -> return $ chRulingTag chistory
                Nothing -> return name
