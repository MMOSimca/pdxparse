{-|
Module      : HOI4.Handlers.Flags
Description : Flags, dates and expansions

Handlers for the bookkeeping script keeps for itself: flags, dates and
which expansions are on.
-}
module HOI4.Handlers.Flags (
        hasDlc
    ,   setFlag
    ,   hasFlag
    ,   handleDate
    ) where

import Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Arrow (first)
import Control.Monad (foldM)

import Abstract -- everything
import MessageTools (formatDays)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT)

import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (msgToPP, preStatement)
import HOI4.Handlers.Generic (withMaybelocAtom2)

-- DLC

hasDlc :: (HOI4Info g, Monad m) => StatementHandler g m
hasDlc [pdx| %_ = ?dlc |]
    = msgToPP $ MsgHasDLC dlc_icon dlc
    where
        -- Script does not always spell an expansion's name the way its store
        -- page does ("Peace For Our Time"), and the difference is never more
        -- than which letters are capital, so the name is matched without
        -- regard to case.
        mdlc_key = HM.lookup (T.toLower dlc) . HM.fromList . map (first T.toLower) $
            [("Together for Victory", "tfv")
            ,("Death or Dishonor", "dod")
            ,("Waking the Tiger", "wtt")
            ,("Man the Guns", "mtg")
            ,("La Resistance", "lar")
            ,("Battle for the Bosporus", "bftb")
            ,("No Step Back", "nsb")
            ,("By Blood Alone", "bba")
            ,("Arms Against Tyranny", "aat")
            ,("Trial of Allegiance", "toa")
            ,("Gotterdammerung", "gtd")
            ,("Graveyard of Empires", "goe")
            ,("No Compromise, No Surrender", "ncns")
            ,("Peace for Our Time", "pfot")
            ,("Thunder at Our Gates", "taog")
            ]
        dlc_icon = maybe "" iconText mdlc_key
hasDlc stmt = preStatement stmt

data SetFlag = SetFlag
        {   sf_flag :: Text
        ,   sf_value :: Maybe Double
        ,   sf_days :: Maybe Double
        ,   sf_dayst :: Maybe Text
        }

newSF :: SetFlag
newSF = SetFlag undefined Nothing Nothing Nothing

setFlag :: forall g m. (HOI4Info g, Monad m) => ScriptMessage -> StatementHandler g m
setFlag msgft stmt@[pdx| %_ = $flag |] = withMaybelocAtom2 msgft MsgSetFlag stmt
setFlag msgft stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_sf =<< foldM addLine newSF scr
    where
        addLine :: SetFlag -> GenericStatement -> PPT g m SetFlag
        addLine sf [pdx| flag = $flag |] =
            return sf { sf_flag = flag }
        addLine sf [pdx| value = !amt |] =
            return sf { sf_value = Just amt }
        addLine sf [pdx| days = !amt |] =
            return sf { sf_days = Just amt }
        addLine sf [pdx| days = $amt |] =
            return sf { sf_dayst = Just amt }
        addLine sf stmt
            = warn (UnknownSection "set_country_flag" stmt) $ return sf
        pp_sf sf = do
            let value = case sf_value sf of
                    Just num -> T.pack $ " to " ++ show (round num)
                    _ -> ""
                days = case (sf_days sf, sf_dayst sf) of
                    (Just day, _) -> " for " <> formatDays day
                    (_, Just day) -> " for " <> day <> " days"
                    _ -> ""
            mloc <- getGameL10nIfPresent (sf_flag sf)
            let loc = fromMaybe "" mloc
            msgfts <- messageText msgft
            return $ MsgSetFlagFor msgfts (sf_flag sf) value days loc
setFlag _ stmt = preStatement stmt

data HasFlag = HasFlag
        {   hf_flag :: Text
        ,   hf_value :: Maybe Text
        ,   hf_days :: Maybe Text
        ,   hf_date :: Maybe Text
        }

newHF :: HasFlag
newHF = HasFlag undefined Nothing Nothing Nothing

hasFlag :: forall g m. (HOI4Info g, Monad m) => ScriptMessage -> StatementHandler g m
hasFlag msgft stmt@[pdx| %_ = $flag |] = withMaybelocAtom2 msgft MsgHasFlag stmt
hasFlag msgft stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_hf =<< foldM addLine newHF scr
    where
        addLine :: HasFlag -> GenericStatement -> PPT g m HasFlag
        addLine hf [pdx| flag = $flag |] =
            return hf { hf_flag = flag }
        addLine hf [pdx| value = !amt |] =
            let amtd = " Is set equal to or more than " <> show (amt :: Int) <> "." in
            return hf { hf_value = Just $ T.pack amtd }
        addLine hf [pdx| value < !amt |] =
            let amtd = " Is set to less than " <> show (amt :: Int) <> "." in
            return hf { hf_value = Just $ T.pack amtd }
        addLine hf [pdx| value > !amt |] =
            let amtd = " Is set to more than " <> show (amt :: Int) <> "." in
            return hf { hf_value = Just $ T.pack amtd }
        addLine hf [pdx| days < !amt |] =
            let amtd = " Has been set for less than " <> show (amt :: Int) <> " days." in
            return hf { hf_days = Just $ T.pack amtd }
        addLine hf [pdx| days > !amt |] =
            let amtd = " Has been set for more than " <> show (amt :: Int) <> " days." in
            return hf { hf_days = Just $ T.pack amtd }
        addLine hf [pdx| date > %amt |] =
            let amtd = " Has been set later than " <> show amt <> "." in
            return hf { hf_date = Just $ T.pack amtd }
        addLine hf [pdx| date < %amt |] =
            let amtd = " Has been set earlier than " <> show amt <> "." in
            return hf { hf_date = Just $ T.pack amtd }
        addLine hf stmt
            = warn (UnknownSection "has_country_flag" stmt) $ return hf
        pp_hf hf =
            case (hf_value hf, hf_days hf, hf_date hf) of
                (Nothing, Nothing, Nothing) -> do
                    mloc <- getGameL10nIfPresent (hf_flag hf)
                    let loc = fromMaybe "" mloc
                    msgfts <- messageText msgft
                    return $ MsgHasFlag msgfts (hf_flag hf) loc
                _ -> do
                    mloc <- getGameL10nIfPresent (hf_flag hf)
                    let loc = fromMaybe "" mloc
                    msgfts <- messageText msgft
                    return $ MsgHasFlagFor msgfts (hf_flag hf) (fromMaybe "" (hf_value hf)) (fromMaybe "" (hf_days hf)) (fromMaybe "" (hf_date hf)) loc
hasFlag _ stmt = preStatement stmt

----------
-- date --
----------

handleDate :: (Monad m, HOI4Info g) =>
    Text -> Text -> StatementHandler g m
handleDate after before  stmt@[pdx| %_ = %date |] = case dateParts date of
    Just (year, month, day) -> do
        monthloc <- isMonth month
        msgToPP $ MsgDate after monthloc (fromIntegral day) (fromIntegral year)
    _ -> preStatement stmt
handleDate after before stmt@[pdx| %_ > %date |] = case dateParts date of
    Just (year, month, day) ->  do
        monthloc <- isMonth month
        msgToPP $ MsgDate after monthloc (fromIntegral day) (fromIntegral year)
    _ -> preStatement stmt
handleDate after before stmt@[pdx| %_ < %date |] = case dateParts date of
    Just (year, month, day) ->  do
        monthloc <- isMonth month
        msgToPP $ MsgDate before monthloc (fromIntegral day) (fromIntegral year)
    _ -> preStatement stmt
handleDate _ _ stmt = preStatement stmt

-- | The year, month and day a right-hand side holds. Script writes a date
-- either bare, where the parser reads it as a date of its own, or in quotes,
-- where it comes through as the text of one.
dateParts :: GenericRhs -> Maybe (Int, Int, Int)
dateParts (DateRhs Date {year = year, month = month, day = day}) = Just (year, month, day)
dateParts rhs = do
    text <- textRhs rhs
    case map (T.unpack . T.strip) (T.splitOn "." text) of
        [y, m, d] | all (all isDigit) [y, m, d], not (any null [y, m, d]) ->
            Just (read y, read m, read d)
        _ -> Nothing

isMonth :: (HOI4Info g, Monad m) =>
    Int -> PPT g m Text
isMonth month
    = getGameL10n $ case month of
            1 -> "January"
            2 -> "February"
            3 -> "March"
            4 -> "April"
            5 -> "May"
            6 -> "June"
            7 -> "July"
            8 -> "August"
            9 -> "September"
            10 -> "October"
            11 -> "November"
            12 -> "December"
            0 -> "" -- no programmer counting, is used when only year is used to check
            14 -> "14th month for some reason" -- for some reason there is a month 14, but not idea why and what for.
            _ -> error ("impossible: tried to localize bad month number" ++ show month)
