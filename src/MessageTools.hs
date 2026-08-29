{-# OPTIONS_GHC -fno-warn-orphans #-}
module MessageTools (
    -- * Numbers
    -- ** Plain formatting
        plainNum, plainNumMin, roundNum, plainNumSign
    ,   roundNumNoSpace
    ,   fixedNumText
    ,   plainPc, plainPcMin
    -- ** Coloured formatting
    -- | These functions take an additional 'Bool' argument that specifies
    -- whether a positive quantity is good (@True@) or bad (@False@). It
    -- formats the number using a @{{red}}@ or @{{green}}@ template
    -- accordingly. Don't use these for numbers that may be either good or bad
    -- depending on context, e.g. karma.
    ,   colourNum, colourPc, colourNumMin, colourPcMin
    ,   colourNumSign, colourPcSign
    -- ** Fixed decimal places
    -- | Each of these writes the number to a set number of decimal places
    -- instead of to as many as it happens to have. See 'fixedNum'.
    ,   colourNumSignPrec, colourPcSignPrec
    ,   plainNumSignPrec, plainNumMinPrec
    ,   plainPcPrec, plainPcMinPrec, plainPcSignPrec
    -- ** Reduced numbers
    -- | Several quantities range from 0 to 100 in game, but are expressed in
    -- script as a number between 0 and 1. To present
    -- these, pass your chosen presentation function to 'reducedNum'.
    ,   reducedNum
    -- * Plural
    ,   plural
    -- * Gain/lose
    -- | These functions hardcode their message fragments. They will have to
    -- be duplicated for languages other than English.
    ,   gainOrLose
    ,   increasedOrDecreased, increaseOrDecrease
    ,   addOrRemove, addedOrRemoved
    -- * Time formatting
    , formatHours
    , formatDays
    , formatMonths
    -- * Wiki markup
    ,   template, templateDoc
    -- * If-then-else
    ,   ifThenElse, ifThenElseT
    -- * General text formatting
    ,   iquotes, quotes, bold, boldText, italic, italicText, typewriterText
    -- * The 'ppNumSep' number formatting method
    ,   PPSep (..)
    ,   module Text.Shakespeare.I18N
    ,   module Text.PrettyPrint.Leijen.Text
    ) where

import Data.List (intersperse, find)
import Data.Maybe (fromMaybe)

import Numeric (floatToDigits)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.Shakespeare.I18N (ToMessage (..))

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified Doc

instance ToMessage Doc where
    toMessage = Doc.doc2text

----------------------
-- Printing numbers --
----------------------

-- | Pretty-print a number, writing out as many decimal places as it happens to
-- have.
class Num a => PPSep a where
    ppNumSep :: a -> Doc

instance PPSep Integer where
    ppNumSep n = Doc.strictText . T.pack $
            (if n < 0 then "−" else "") <> ppNumSep' True (show (abs n))

-- | Write out a run of digits, whole or fractional. The game breaks a long
-- number up with a separator and the wiki does not, so the digits go down as
-- they come.
ppNumSep' :: Bool -> String -> String
ppNumSep' _ = id

instance PPSep Int where
    ppNumSep = ppNumSep . toInteger

instance PPSep Double where
    ppNumSep n
        = let absn = roundEightDecimals (abs n)
              (digits, expn) = floatToDigits 10 absn
              (_, fracDigits') = splitAt expn digits
              -- fracDigits' is [] if exp is a nonzero whole number
              fracDigits = if fracDigits' == [0] then [] else fracDigits'
          in (if n < 0 then "−" else "")
                <> PP.text (TL.pack . ppNumSep' True $ show (truncate absn :: Int))
                <> (if null fracDigits
                    then ""
                    else "."
                         <> PP.text (TL.pack . ppNumSep' False $
                             replicate (negate expn) '0' -- zeroes after decimal
                             ++ concatMap show fracDigits))

-- | Format a number to a given number of decimal places, keeping any trailing
-- zeroes. Unlike 'ppNumSep', which writes out
-- only the places a number happens to have, this writes the places it is asked
-- for: the localization says how precisely a value is meant to be read, which is
-- not always how precisely it was written in script.
--
-- The count is a floor rather than an exact width. A value written to fewer
-- places than it needs would read as a rounder number than it is, and one
-- written to none at all would read as no change whatsoever.
fixedNumText :: Int -> Double -> Text
fixedNumText places n = T.pack $
    (if rounded < 0 then "−" else "")
        <> ppNumSep' True (show whole)
        <> (if places' > 0 then "." <> ppNumSep' False (pad (show frac)) else "")
    where
        places' = max (max 0 places) (neededPlaces n)
        scale = 10 ^ places' :: Integer
        rounded = round (n * fromIntegral scale) :: Integer
        (whole, frac) = abs rounded `divMod` scale
        pad s = replicate (places' - length s) '0' <> s

-- | How many decimal places it takes to write a number without rounding it. The
-- game itself keeps at most three, so a value needing more than that is floating
-- point noise and is left to be rounded off.
neededPlaces :: Double -> Int
neededPlaces n = fromMaybe maxPlaces (find isExact [0 .. maxPlaces])
    where
        maxPlaces = 3
        isExact places = abs (n - rounded places) < 1e-9
        rounded places = let scale = 10 ^ places :: Integer
                         in fromIntegral (round (n * fromIntegral scale) :: Integer)
                                / fromIntegral scale

-- | Format a number as is.
plainNum :: Double -> Doc
plainNum = ppNum False False False False False

-- | Format a number as is, keeping the minus sign.
plainNumMin :: Double -> Doc
plainNumMin = ppNum False False False False True

-- | Format a number as is, with a sign.
plainNumSign :: Double -> Doc
plainNumSign = ppNum False False False True False

-- | Format a number as a percentage.
plainPc :: Double -> Doc
plainPc = ppNum False True False False False

-- | Format a number as a percentage, keeping the minus sign.
plainPcMin :: Double -> Doc
plainPcMin = ppNum False True False False True

-- | Front end to 'ppNum' for uncoloured numbers.
roundNum' :: Bool -- ^ Whether to treat this number as a percentage
          -> Bool -- ^ Whether to add a + if this number is positive
          -> Double -> Doc
roundNum' is_pc pos_plus n =
    let rounded :: Int
        rounded = round n
    in ppNum False is_pc True pos_plus False rounded

-- | Format a number, but make sure it's an integer by rounding it off.
roundNum :: Double -> Doc
roundNum = roundNum' False False

-- | Format a number, but make sure it's an integer by rounding it off.
roundNumNoSpace :: (RealFrac n, PPSep n) => n -> Text
roundNumNoSpace n = Doc.doc2text $ PP.integer (round n :: Integer)

-- | Format a number in an appropriate colour.
colourNum :: Bool -> Double -> Doc
colourNum good = ppNum True False good False False

-- | Format a number as a percentage, in an appropriate colour.
colourPc :: Bool -> Double -> Doc
colourPc good = ppNum True True good False False

-- | Format a number in an appropriate colour, keeping
-- the @-@ in front of a negative one. See 'colourPcMin'.
colourNumMin :: Bool -> Double -> Doc
colourNumMin good = ppNum True False good False True

-- | Format a number as a percentage, in an appropriate colour, keeping the @-@
-- in front of a negative one. Unlike 'colourPc',
-- which drops the sign on the assumption that the surrounding sentence says
-- which way the value goes, this is for a figure that stands on its own and so
-- has to say so itself.
colourPcMin :: Bool -> Double -> Doc
colourPcMin good = ppNum True True good False True

-- | Format a number in an appropriate colour, adding
-- a @+@ at the start if positive.
colourNumSign :: Bool -> Double -> Doc
colourNumSign good = ppNum True False good True False

-- | Format a number as a percentage in an appropriate colour, adding a @+@ at
-- the start if positive.
colourPcSign :: Bool -> Double -> Doc
colourPcSign good = ppNum True True good True False

-- | Format a number using the given function, but multiply it by 100 first.
reducedNum :: PPSep n => (n -> Doc) -> n -> Doc
reducedNum p n = p (n * 100)

-- | Write a number to a set number of decimal places, or, given 'Nothing', to as
-- many as it happens to have.
fixedNum :: Maybe Int -> Double -> Doc
fixedNum Nothing = ppNumSep
fixedNum (Just places) = Doc.strictText . fixedNumText places

-- | The formatting functions above, each writing the number to a set number of
-- decimal places. The game's localization says how precisely each of its
-- modifiers is meant to be read, and a value written to fewer places than that
-- reads as rounder than it is. 'Nothing' keeps as many places as the number has,
-- which is what the function each is named after does.
colourNumSignPrec, colourPcSignPrec :: Maybe Int -> Bool -> Double -> Doc
colourNumSignPrec places good = ppNumWith (fixedNum places) True False good True False
colourPcSignPrec places good = ppNumWith (fixedNum places) True True good True False

plainNumSignPrec, plainNumMinPrec, plainPcPrec, plainPcMinPrec, plainPcSignPrec
    :: Maybe Int -> Double -> Doc
plainNumSignPrec places = ppNumWith (fixedNum places) False False False True False
plainNumMinPrec places = ppNumWith (fixedNum places) False False False False True
plainPcPrec places = ppNumWith (fixedNum places) False True False False False
plainPcMinPrec places = ppNumWith (fixedNum places) False True False False True
plainPcSignPrec places = ppNumWith (fixedNum places) False True False True False

-- | Round number to eight decimal places to avoid floating point inaccuracies
-- e.g. 0.55 would be rendered as 0.5500000000000001
roundEightDecimals :: Double -> Double
roundEightDecimals num = fromIntegral (round (num * 10000)) / 10000

-- | Pretty-print a number.
ppNum :: (Ord n, PPSep n) => Bool -- ^ Whether to apply a colour template (red
                                  --   for bad, green for good).
                          -> Bool -- ^ Whether to treat this number as a
                                  --   percentage, i.e. add a percentage sign.
                          -> Bool -- ^ Whether positive is good and negative is
                                  --   bad, or vice versa. Ignored if the first
                                  --   argument is False.
                          -> Bool -- ^ Whether to add a + to positive numbers,
                                  --   or strip the - from negative ones.
                          -> Bool -- ^ Whether to keep the - from negative numbers.
                          -> n -> Doc
ppNum = ppNumWith ppNumSep

-- | As 'ppNum', but with the writing of the digits themselves left open, so that
-- a number can be written to a set number of decimal places rather than to as
-- many as it happens to have.
ppNumWith :: (Ord n, Num n) => (n -> Doc) -- ^ How to write the number itself.
                          -> Bool -> Bool -> Bool -> Bool -> Bool
                          -> n -> Doc
ppNumWith write colour is_pc pos pos_plus kp_min n =
    let n_pp'd = (if kp_min
                then id
                else (if pos_plus then Doc.ppSigned else Doc.ppNosigned))
                 write n <> if is_pc then "%" else ""
    in (if not colour then id else
        case (if pos then n else negate n) `compare` 0 of
            LT -> template "red" . (:[]) . Doc.doc2text
            EQ -> bold
            GT -> template "green" . (:[]) . Doc.doc2text)
        n_pp'd

-- | If the numeric argument is singular, return the second argument; otherwise
-- return the third argument.
--
-- This is for cases where the form of a word depends on whether the number is
-- 1 or something else. For example, instead of @Has at least #{n} province(s)@:
--
-- * Has at least 1 province(s)
-- * Has at least 2 province(s)
--
-- we can say @Has at least #{n} #{plural n "province" "provinces"}@, which gives
-- the following, prettier output:
--
-- * Has at least 1 province
-- * Has at least 2 provinces
plural :: (Eq n, Num n) => n -> Text -> Text -> Text
plural n sing plur | n == 1    = sing
                   | otherwise = plur

-- | Say "Gain" or "Lose" (with that capitalisation) depending on whether the
-- numeric argument is positive or negative (respectively).
gainOrLose :: (Ord n, Num n) => n -> Text
gainOrLose n | n < 0     = "Lose"
             | otherwise = "Gain"

-- | Say "increased" or "decreased" (with that capitalisation) depending on whether the
-- numeric argument is positive or negative (respectively).
increasedOrDecreased :: (Ord n, Num n) => n -> Text
increasedOrDecreased n | n < 0     = "decreased"
                       | otherwise = "increased"

-- | Say "Increased" or "Decreased" (with that capitalisation) depending on whether the
-- numeric argument is positive or negative (respectively).
increaseOrDecrease :: (Ord n, Num n) => n -> Text
increaseOrDecrease n | n < 0     = "Decrease"
                     | otherwise = "Increase"

-- | Say "Add" or "Remove" (with that capitalisation) depending on whether the
-- numeric argument is positive or negative (respectively).
addOrRemove :: (Ord n, Num n) => n -> Text
addOrRemove n | n < 0     = "Remove"
              | otherwise = "Add"


-- | Say "added" or "removed" (with that capitalisation) depending on whether the
-- numeric argument is positive or negative (respectively).
addedOrRemoved :: (Ord n, Num n) => n -> Text
addedOrRemoved n | n < 0     = "removed"
                 | otherwise = "added"

-- | Format years
formatYears :: Int -> Text
formatYears 1 = "1 year"
formatYears ys = T.pack $ show ys <> " years"

-- | Format days
formatDays :: Double -> Text
formatDays days = formatDays' (round days :: Int)
    where
    formatDays' :: Int -> Text
    formatDays' days | days < 0   = "the rest of the game"
    formatDays' 1                 = "1 day"
    formatDays' days | days < 365 = T.pack $ show days <> " days"
    formatDays' days              = formatYears (days `div` 365) <>
                                        (if days `mod` 365 == 0 then
                                            ""
                                         else
                                            " and " <> formatDays' (days `mod` 365))
-- | Format months
formatMonths :: Double -> Text
formatMonths months = formatMonths' (round months :: Int)
    where
    formatMonths' :: Int -> Text
    formatMonths' 1 = "1 month"
    formatMonths' months | months < 12 = T.pack $ show months <> " months"
    formatMonths' months = formatYears (months `div` 12) <>
                            (if months `mod` 12 == 0 then
                                ""
                            else
                                " and " <> formatMonths' (months `mod` 12))

-- | Format hours
formatHours :: Double -> Text
formatHours hours = formatHours' (round hours :: Int)
    where
    formatHours' :: Int -> Text
    formatHours' hours | hours < 0   = "the rest of the game"
    formatHours' 1                 = "1 hour"
    formatHours' hours | hours < 24 = T.pack $ show hours <> " hours"
    formatHours' hours              = formatDays (fromIntegral (hours `div` 24)) <>
                                        (if hours `mod` 24 == 0 then
                                            ""
                                         else
                                            " and " <> formatHours' (hours `mod` 24))

-----------------
---- Wiki text --
-----------------

-- | Template with arguments. @template "foo" ["bar","baz"]@ produces
-- @{{foo|bar|baz}}@.
template :: Text -> [Text] -> Doc
template name args = templateDoc (Doc.strictText name) (map Doc.strictText args)

-- | Doc version of 'template'.
templateDoc :: Doc -> [Doc] -> Doc
templateDoc name args = PP.hcat $
    "{{"
    : (intersperse "|" (name:args)
      ++ ["}}"])

-- | Set text in italics, and wrap in quotation marks. Use this for short
-- localized strings such as modifier and event names.
iquotes :: Text -> Doc
iquotes = PP.enclose "''“" "”''" . Doc.strictText

quotes :: Text -> Doc
quotes = PP.enclose "“" "”" . Doc.strictText

---- Set doc in italics.
italic :: Doc -> Doc
italic = PP.enclose "''" "''"

italicText :: Text -> Text
italicText = Doc.doc2text . italic . Doc.strictText

-- | Set doc in boldface. Take care: if the text passed to this begins or ends
-- with an apostrophe, you may get incorrect results. Mixing with italics,
-- however, does work.
bold :: Doc -> Doc
bold = PP.enclose "'''" "'''"

-- | Set text in boldface. Take care: if the text passed to this begins or ends
-- with an apostrophe, you may get incorrect results. Mixing with italics,
-- however, does work.
boldText :: Text -> Text
boldText = Doc.doc2text . bold . Doc.strictText

typewriter :: Doc -> Doc
typewriter = PP.enclose "<tt>" "</tt>"

typewriterText :: Text -> Text
typewriterText = Doc.doc2text . typewriter . Doc.strictText

-- | Produce output based on a boolean (i.e. if-then-else). Needed because the
-- i18n templates don't understand this syntax, but instead interpret these
-- three keywords as identifiers.
--
-- You don't need to use this in the non-TH version of "Messages". Just use
-- plain old if-then-else.
ifThenElse :: Bool -> a -> a -> a
ifThenElse yn yes no = if yn then yes else no

-- | As 'ifThenElse', but specialized to 'Text'. This is needed because, in the
-- presence of the OverloadedStrings extension, type inference doesn't know
-- what specific string type you mean when you use a string literal.
ifThenElseT :: Bool -> Text -> Text -> Text
ifThenElseT = ifThenElse
