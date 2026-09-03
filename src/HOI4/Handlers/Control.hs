{-|
Module      : HOI4.Handlers.Control
Description : Where and how often a block runs

Handlers for the statements that say where a block runs and how often,
rather than what it does: @THIS@, the scopes over listed targets, counted
triggers, loops over a counter, and random chance.
-}
module HOI4.Handlers.Control (
        random
    ,   randomList
    ,   thisScope
    ,   listedScope
    ,   listedState
    ,   countTriggers
    ,   forLoopEffect
    ,   anyStateIn
    ) where

import Data.Char (isDigit)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative (liftA2)
import Control.Monad (foldM)

import Debug.Trace

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (plainNumMin)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, indentDown, withCurrentIndent, withCurrentFile)
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (compoundMessage, headed, msgToPP, preStatement)

-- Random

random :: (HOI4Info g, Monad m) => StatementHandler g m
random stmt@[pdx| %_ = @scr |]
    | (front, back) <- break
                        (\case
                            [pdx| chance = %_ |] -> True
                            _ -> False)
                        scr
      , not (null back)
      , [pdx| %_ = %rhs |] <- head back
      , Just chance <- floatRhs rhs
      = compoundMessage
          (MsgRandomChance chance)
          [pdx| %undefined = @(front ++ tail back) |]
    | (front, back) <- break
                        (\case
                            [pdx| chance = %_ |] -> True
                            _ -> False)
                        scr
      , not (null back)
      , [pdx| %_ = %rhs |] <- head back
      , Just chancevar <- varName rhs
      = compoundMessage
          (MsgRandomChanceVar chancevar)
          [pdx| %undefined = @(front ++ tail back) |]
    | otherwise = compoundMessage MsgRandom stmt
random stmt = preStatement stmt

-- | The name of the variable a right-hand side holds, where it holds one
-- rather than a number.
varName :: GenericRhs -> Maybe Text
varName (GenericRhs vartag [var]) = Just (vartag <> ":" <> var)
varName (GenericRhs var []) = Just var
varName _ = Nothing

toPct :: Double -> Double
toPct num = fromIntegral (round (num * 1000)) / 10 -- round to one digit after the point

data RandomMod = RandomMod{
     rm_mod  :: GenericScript
    ,rm_rest :: GenericScript
}

newRM :: RandomMod
newRM = RandomMod [] []

randomList :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
randomList stmt@[pdx| %_ = @scr |] =
    let (_,scre) = extractStmt (matchLhsText "seed") scr in -- ugly solution to dealing with seed type in random_list
    let (_,scree) = extractStmt (matchLhsText "log") scre in -- ugly solution to dealing with log type in random_list
    if all chk scree then -- Ugly solution for vars in random list
        fmtRandomList $ map entry scree
    else
        fmtRandomVarList $ map entryv scree
    where
        chk [pdx| !weight = @scr |] = True
        chk [pdx| %var = @scr |] = False
        chk _ = trace ("DEBUG: random_list " ++ show scr) (error "Bad clause in random_list check")
        entry [pdx| !weight = @scr |] = (fromIntegral weight, scr)
        entry _ = trace ("DEBUG: random_list " ++ show scr) (error "Bad clause in random_list, possibly vars?")
        entryv [pdx| $var = @scr |] = (var, scr)
        entryv [pdx| $_:$var = @scr |] = (var, scr)
        entryv [pdx| !weight = @scr |] = (T.pack (show weight), scr)
        entryv _ = trace ("DEBUG: random_list " ++ show scr) (error "Bad clause in random_list, possibly ints?")
        fmtRandomList entries = withCurrentIndent $ \i ->
            let total = sum (map fst entries)
            in (:) (i, MsgRandom) <$> (concat <$> indentUp (mapM (fmtRandomList' total) entries))
        fmtRandomList' total (wt, what) = do
            -- TODO: Could probably be simplified.
            let (mtrigger, rest) = extractStmt (matchLhsText "trigger") what
            rm <- foldM foldModifiers newRM rest
            trig <- (case mtrigger of
                Just s -> indentUp (compoundMessage MsgRandomListTrigger s)
                _ -> return [])
            mod <- ppMods rm
            body <- ppMany (rm_rest rm) -- has integral indentUp
            liftA2 (++)
                (msgToPP $ MsgRandomChanceHOI4 (toPct (wt / total)) wt)
                (pure (trig ++ mod ++ body))
        -- Ugly solution for vars in random list
        fmtRandomVarList entries = withCurrentIndent $ \i ->
            (:) (i, MsgRandom) <$> (concat <$> indentUp (mapM fmtRandomVarList' entries))
        fmtRandomVarList' (wt, what) = do
            -- TODO: Could probably be simplified.
            let (mtrigger, rest) = extractStmt (matchLhsText "trigger") what
            rm <- foldM foldModifiers newRM rest
            trig <- (case mtrigger of
                Just s -> indentUp (compoundMessage MsgRandomListTrigger s)
                _ -> return [])
            mod <- ppMods rm
            body <- ppMany (rm_rest rm) -- has integral indentUp
            liftA2 (++)
                (msgToPP $ MsgRandomVarChance wt)
                (pure (trig ++ mod ++ body))

        ppMods rm = concat <$> indentUp (mapM (\case
                s@[pdx| %_ = @scr |] ->
                    let
                        (mfactor, s') = extractStmt (matchLhsText "factor") scr
                        (madd, sa') = extractStmt (matchLhsText "add") scr
                    in
                        case mfactor of
                            Just [pdx| %_ = !factor |] -> do
                                cond <- ppMany s'
                                liftA2 (++) (msgToPP $ MsgRandomListModifier factor) (pure cond)
                            _ -> case madd of
                                    Just [pdx| %_ = !add |] -> do
                                        cond <- ppMany sa'
                                        liftA2 (++) (msgToPP $ MsgRandomListAddModifier add) (pure cond)
                                    Just [pdx| %_ = $vartag:$var |] -> do
                                        cond <- ppMany sa'
                                        liftA2 (++) (msgToPP $ MsgRandomListAddModifierVar (vartag <> ":" <> var)) (pure cond)
                                    Just [pdx| %_ = $var |] -> do
                                        cond <- ppMany sa'
                                        liftA2 (++) (msgToPP $ MsgRandomListAddModifierVar var) (pure cond)
                                    _ -> preStatement s
                s -> preStatement s) (rm_mod rm))

        foldModifiers :: RandomMod -> GenericStatement -> PPT g m RandomMod
        foldModifiers rm stmt@[pdx| modifier = %s |] = return rm {rm_mod = rm_mod rm ++ [stmt]}
        foldModifiers rm stmt = return rm {rm_rest = rm_rest rm ++ [stmt]}
randomList _ = withCurrentFile $ \file ->
    error ("randomList sent strange statement in " ++ file)

-------------------------
-- handler for THIS    --
-------------------------

-- | @THIS@ names the scope the block is already written in, so it says nothing
-- the reader is not already under: what is inside it stands where it stood,
-- without a heading and without the step in that a heading would bring.
thisScope :: (HOI4Info g, Monad m) => StatementHandler g m
thisScope [pdx| %_ = @scr |] = indentDown (ppMany scr)
thisScope stmt = preStatement stmt

------------------------------------------------------
-- handler for the scopes over a list of targets     --
------------------------------------------------------

-- | A trigger asked of states or countries listed out by name, rather than of
-- whatever a scope of its own catches: @any_state_of@, @all_country_of@ and
-- their like. The list may also be a script constant, which is written out as
-- the constant's name since what it holds is not in the script at hand.
listedScope :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -- ^ Message to use as the block header
        -> (Text -> PPT g m Text) -- ^ How to name one of the things listed
        -> StatementHandler g m
listedScope header nameOf [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let (mtarget, rest) = extractStmt (matchLhsText "target") scr
    names <- case mtarget of
        Just [pdx| %_ = @tgts |] -> traverse nameOf (mapMaybe bareTarget tgts)
        Just [pdx| %_ = ?one |] -> (:[]) <$> nameOf one
        _ -> return []
    let listed = if null names then "the ones named" else T.intercalate ", " names
    script_pp'd <- ppMany rest
    return ((i, header listed) : script_pp'd)
    where
        bareTarget (StatementBare (IntLhs n)) = Just (T.pack (show n))
        bareTarget (StatementBare (GenericLhs t [])) = Just t
        bareTarget stmt = warn (UnknownSection "target array" stmt) Nothing
listedScope _ _ stmt = preStatement stmt

-- | The name of a state written into such a list, whether by its id or through
-- a variable holding it.
listedState :: (HOI4Info g, Monad m) => Text -> PPT g m Text
listedState t
    | not (T.null t), T.all isDigit t = getStateLoc (read (T.unpack t))
    | otherwise = eGetStateText (Left t)

-----------------------------------
-- handler for count_triggers    --
-----------------------------------

-- | How many of the conditions under it must hold. Script writes the number
-- with any of @=@, @>@ and @<@, and the three do not say the same thing, so
-- the comparison is carried into the message rather than assumed.
countTriggers :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
countTriggers stmt@[pdx| %_ = @scr |] =
    withCurrentIndent $ \i -> do
        let (mamount, rest) = extractStmt (matchLhsText "amount") scr
            withAmount num comp = do
                script_pp'd <- ppMany rest
                return (headed i (MsgCountTriggers num comp) script_pp'd)
        case mamount of
            Just [pdx| %_ = !num |] -> withAmount num "At least"
            Just [pdx| %_ > !num |] -> withAmount num "More than"
            Just [pdx| %_ < !num |] -> withAmount num "Fewer than"
            _ -> preStatement stmt
countTriggers stmt = preStatement stmt

-----------------------------------
-- handler for for_loop_effect   --
-----------------------------------

-- | Runs what is under it once for each value of a counter. The bounds and the
-- counter are fields of the block rather than effects of their own, so they are
-- read off into the heading and only the body is listed.
forLoopEffect :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
forLoopEffect [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let (mstart, s1) = extractStmt (matchLhsText "start") scr
        (mend,   s2) = extractStmt (matchLhsText "end") s1
        (madd,   s3) = extractStmt (matchLhsText "add") s2
        (mvalue, s4) = extractStmt (matchLhsText "value") s3
        -- The name of the variable that breaks the loop says nothing about what
        -- the loop does, and neither does how its bounds are compared.
        (_,      s5) = extractStmt (matchLhsText "break") s4
        (_,     rest) = extractStmt (matchLhsText "compare") s5
        -- Defaults are the game's: from 0, up by 1, counting in 'v'.
        bound def mstmt = case mstmt of
            Just [pdx| %_ = !num |] -> Doc.doc2text (plainNumMin (num :: Double))
            Just [pdx| %_ = $vartag:$var |] -> vartag <> ":" <> var
            Just [pdx| %_ = $var |] -> var
            _ -> def
        loopvar = case mvalue of
            Just [pdx| %_ = $var |] -> var
            _ -> "v"
    script_pp'd <- ppMany rest
    return (headed i (MsgForLoop loopvar (bound "0" mstart) (bound "0" mend) (bound "1" madd))
            script_pp'd)
forLoopEffect stmt = preStatement stmt

-------------------------------
-- handler for any_state_in  --
-------------------------------

-- | The states of a strategic region, a continent, or an array, with a
-- condition asked of each.
anyStateIn :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
anyStateIn [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let isWhere st = any (`matchLhsText` st)
            ["strategic_region", "array", "continent", "ai_area"]
        (mwhere, rest) = extractStmt isWhere scr
    header <- case mwhere of
        Just [pdx| strategic_region = !num |] -> MsgAnyStateIn <$> getRegionLoc num
        Just [pdx| continent = $con |] -> MsgAnyStateIn <$> getGameL10n con
        Just [pdx| %_ = $vartag:$var |] -> return $ MsgAnyStateInArray (vartag <> "." <> var)
        Just [pdx| %_ = $arr |] -> return $ MsgAnyStateInArray arr
        _ -> return $ MsgAnyStateInArray "<!-- Check Script -->"
    script_pp'd <- ppMany rest
    return ((i, header) : script_pp'd)
anyStateIn stmt = preStatement stmt
