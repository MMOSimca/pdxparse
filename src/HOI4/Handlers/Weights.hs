{-|
Module      : HOI4.Handlers.Weights
Description : Mean time to happen, AI weighting and AI strategies

The weights script hangs on events, decisions and AI choices. The
@mean_time_to_happen@ and @ai_will_do@ formatters are called by the feature
modules directly rather than through the handler table.
-}
module HOI4.Handlers.Weights (
        ppMtth
    ,   addAiStrategy
    ,   ppAiWillDo
    ,   ppAiMod
    ) where

import Data.Char (toUpper)
import Data.Foldable (fold)
import Data.List (foldl', intersperse)
import Data.Maybe
import qualified Data.Text as T

import Control.Arrow (first)

import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import Doc (Doc)
import qualified Doc -- everything
import MessageTools (plural, boldText)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, withCurrentIndentZero)

import {-# SOURCE #-} HOI4.Common (ppScript, ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (msgToPP, plainMsg, preStatement)

-----------------------------------------------------------------
-- Script handlers that should be used directly, not via ppOne --
-----------------------------------------------------------------

-- | Data for @mean_time_to_happen@ clauses
data MTTH = MTTH
        {   mtth_years :: Maybe Int
        ,   mtth_months :: Maybe Int
        ,   mtth_days :: Maybe Int
        ,   mtth_modifiers :: [MTTHModifier]
        } deriving Show

-- | Data for @modifier@ clauses within @mean_time_to_happen@ clauses
data MTTHModifier = MTTHModifier
        {   mtthmod_factor :: Maybe Double
        ,   mtthmod_conditions :: GenericScript
        } deriving Show

-- | Empty MTTH
newMTTH :: MTTH
newMTTH = MTTH Nothing Nothing Nothing []

-- | Empty MTTH modifier
newMTTHMod :: MTTHModifier
newMTTHMod = MTTHModifier Nothing []

-- | Format a @mean_time_to_happen@ clause as wiki text.
ppMtth :: (HOI4Info g, Monad m) => Bool -> GenericScript -> PPT g m Doc
ppMtth isTriggeredOnly = ppMtth' . foldl' addField newMTTH
    where
        addField mtth [pdx| years    = !n   |] = mtth { mtth_years = Just n }
        addField mtth [pdx| months   = !n   |] = mtth { mtth_months = Just n }
        addField mtth [pdx| days     = !n   |] = mtth { mtth_days = Just n }
        addField mtth [pdx| modifier = @rhs |] = addMTTHMod mtth rhs
        addField mtth stmt = warn (UnknownSection "mean_time_to_happen" stmt) mtth
        addMTTHMod mtth scr = mtth {
                mtth_modifiers = mtth_modifiers mtth
                                 ++ [foldl' addMTTHModField newMTTHMod scr] } where
            addMTTHModField mtthmod [pdx| factor = !n |]
                = mtthmod { mtthmod_factor = Just n }
            addMTTHModField mtthmod stmt -- anything else is a condition
                = mtthmod { mtthmod_conditions = mtthmod_conditions mtthmod ++ [stmt] }
        ppMtth' (MTTH myears mmonths mdays modifiers) = do
            modifiers_pp'd <- intersperse PP.line <$> mapM pp_mtthmod modifiers
            let hasYears = isJust myears
                hasMonths = isJust mmonths
                hasDays = isJust mdays
                hasModifiers = not (null modifiers)
            return . mconcat $ (if isTriggeredOnly then [] else
                maybe []
                    (\years ->
                        [PP.int years, PP.space, Doc.strictText $ plural years "year" "years"]
                        ++
                        if hasMonths && hasDays then [",", PP.space]
                        else if hasMonths || hasDays then ["and", PP.space]
                        else [])
                    myears
                ++
                maybe []
                    (\months -> [PP.int months, PP.space, Doc.strictText $ plural months "month" "months"])
                    mmonths
                ++
                maybe []
                    (\days ->
                        (if hasYears && hasMonths then ["and", PP.space]
                         else []) -- if years but no months, already added "and"
                        ++
                        [PP.int days, PP.space, Doc.strictText $ plural days "day" "days"])
                    mdays
                ) ++
                (if hasModifiers then
                    (if isTriggeredOnly then
                        [PP.line, "'''Weight modifiers'''", PP.line]
                    else
                        [PP.line, "<br/>'''Modifiers'''", PP.line])
                    ++ modifiers_pp'd
                 else [])
        pp_mtthmod (MTTHModifier (Just factor) conditions) =
            case conditions of
                [_] -> do
                    conditions_pp'd <- ppScript conditions
                    return . mconcat $
                        [conditions_pp'd
                        ,PP.enclose ": '''×" "'''" (Doc.ppFloat factor)
                        ]
                _ -> do
                    conditions_pp'd <- indentUp (ppScript conditions)
                    return . mconcat $
                        ["*"
                        ,PP.enclose "'''×" "''':" (Doc.ppFloat factor)
                        ,PP.line
                        ,conditions_pp'd
                        ]
        pp_mtthmod (MTTHModifier Nothing _)
            = return "(invalid modifier! Bug in extractor?)"

-- | Handler for @add_ai_strategy@, which leans the AI towards something or away
-- from it. Nothing of the world changes by it and the game draws none of it, so
-- there is no wording of the game's own to keep to: what script writes is the
-- kind of leaning, what it is about, and how strongly, and all three are said.
--
-- The thing it is about is a country for all but a couple of kinds, which name a
-- building or a kind of equipment instead. A kind that names nothing at all --
-- how heavily to garrison, what to hold factories back for -- is about the
-- country's own doings, and is said on its own.
addAiStrategy :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addAiStrategy stmt@[pdx| %_ = @scr |] = case aiKind of
    Just kind -> do
        about <- aiAbout kind aiAbout'
        msgToPP $ case aiValue of
            Just value -> MsgAddAiStrategy (named kind <> targeted) about value
            Nothing -> MsgAddAiStrategyUnweighted (named kind <> targeted) about
    _ -> preStatement stmt
    where
        (aiKind, aiTarget, aiId, aiValue) =
            foldl' addLine (Nothing, Nothing, Nothing, Nothing) scr
        addLine (k, t, i, v) [pdx| type   = $kind |] = (Just kind, t, i, v)
        addLine (k, t, i, v) [pdx| target = $tgt  |] = (k, Just tgt, i, v)
        addLine (k, t, i, v) [pdx| id     = $vartag:$var |] = (k, t, Just (Right (vartag, var)), v)
        addLine (k, t, i, v) [pdx| id     = $who  |] = (k, t, Just (Left who), v)
        addLine (k, t, i, v) [pdx| id     = ?who  |] = (k, t, Just (Left who), v)
        addLine (k, t, i, v) [pdx| value  = !n    |] = (k, t, i, Just n)
        addLine acc stmt = warn (UnknownSection "ai_strategy" stmt) acc
        -- Script names the country under either key: where there is no id, the
        -- target is the country the leaning is about. Where there is one, the
        -- target instead says which dealing is meant, the kind on its own saying
        -- only that it is one.
        aiAbout' = case aiId of
            Just ewho -> Just ewho
            Nothing -> Left <$> aiTarget
        targeted = case (aiId, aiTarget) of
            (Just _, Just tgt) -> " (" <> named tgt <> ")"
            _ -> ""
        -- These two name a thing the country builds or keeps; every other kind
        -- names a country.
        aboutAThing = ["building_target", "save_equipment"]
        aiAbout _ Nothing = return ""
        aiAbout kind (Just ewho)
            | kind `elem` aboutAThing, Left who <- ewho = do
                loc <- getGameL10n who
                return (" for " <> boldText loc)
            | otherwise = do
                mflag <- eflag (Just HOI4Country) ewho
                return (maybe "" (" towards " <>) mflag)
        -- Script writes these as ids; a name is those words with the underscores
        -- taken back out.
        named theid = case T.uncons (T.replace "_" " " theid) of
            Just (c, rest) -> T.cons (toUpper c) rest
            Nothing -> theid
addAiStrategy stmt = preStatement stmt

-- AI decision factors

-- | Extract the appropriate message(s) from an @ai_will_do@ clause.
ppAiWillDo :: (HOI4Info g, Monad m) => AIWillDo -> PPT g m IndentedMessages
ppAiWillDo (AIWillDo mbase mods) = do
    mods_pp'd <- fold <$> traverse ppAiMod mods
    let baseWtMsg = maybe MsgNoBaseWeight MsgAIBaseWeight mbase
    iBaseWtMsg <- msgToPP baseWtMsg
    return $ iBaseWtMsg ++ mods_pp'd

-- | Extract the appropriate message(s) from a @modifier@ section within an
-- @ai_will_do@ clause.
ppAiMod :: (HOI4Info g, Monad m) => AIModifier -> PPT g m IndentedMessages
ppAiMod (AIModifier (Just multiplier) Nothing triggers) = do
    triggers_pp'd <- ppMany triggers
    case triggers_pp'd of
        [(i, triggerMsg)] -> do
            triggerText <- messageText triggerMsg
            return [(i, MsgAIFactorOneline triggerText multiplier)]
        _ -> withCurrentIndentZero $ \i -> return $
            (i, MsgAIFactorHeader multiplier)
            : map (first succ) triggers_pp'd -- indent up
ppAiMod (AIModifier Nothing (Just addition) triggers) = do
    triggers_pp'd <- ppMany triggers
    case triggers_pp'd of
        [(i, triggerMsg)] -> do
            triggerText <- messageText triggerMsg
            return [(i, MsgAIAddOneline triggerText addition)]
        _ -> withCurrentIndentZero $ \i -> return $
            (i, MsgAIAddHeader addition)
            : map (first succ) triggers_pp'd -- indent up
ppAiMod AIModifier {} =
    plainMsg "(missing multiplier/add for this factor)"
