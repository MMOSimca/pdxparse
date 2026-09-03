{-|
Module      : HOI4.Handlers.Chunks
Description : Runs of statements that only make sense together

A dynamic modifier's variables written one after another, a series of
idea slots, or a run of blocks on the same state: 'chunkScript' draws each of
these together so that "HOI4.Common" can write it out as one thing.
-}
module HOI4.Handlers.Chunks (
        ScriptChunk (..)
    ,   chunkScript
    ,   ppIdeaSlotChunk
    ,   ppDynModChunk
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', groupBy)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import qualified Doc -- everything
import QQ -- everything
import SettingsTypes (PPT, scope, concatMapM)

import {-# SOURCE #-} HOI4.Common (ppMany, ppOne)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (msgToPP, plainMsg)
import HOI4.Handlers.Ideas (showIdea, showIdeaUnderHeading)
import HOI4.Handlers.Modifiers (ppDynModBox)

-------------------------------------------------------
-- Runs of statements that only make sense together  --
-------------------------------------------------------

-- | How a run of variable writes changes the dynamic modifier behind them.
data DynModOp = DynModSet | DynModAdd | DynModSub deriving (Eq)

-- | An effect script, with the runs of statements that only say something
-- together pulled out of it.
data ScriptChunk
    = PlainStmt GenericStatement
    | DynModChunk HOI4DynamicModifier Bool [(Text, Double)]
      -- ^ The modifier, whether its values are set rather than added to, and
      --   the modifier keys affected with their new values.
    | IdeaSlotChunk GenericStatement [GenericStatement]
      -- ^ The tooltip announcing that a slot's ideas change, and the
      --   @show_ideas_tooltip@ statements naming the ideas it announces.
    | StateChunk Text IndentedMessages
      -- ^ The states a run of scopes names, written out as one, and what the
      --   block every one of them holds comes to.

-- | Split a script into the chunks that are shown as a whole.
chunkScript :: (HOI4Info g, Monad m) => GenericScript -> PPT g m [ScriptChunk]
chunkScript scr = chunkStates =<< chunkIdeaSlots <$> chunkDynModVars scr

-- | Group each run of consecutive state scopes that come to the same thing, so
-- that what befalls all of them is said once with the states named together,
-- the way the game says it.
--
-- What is compared is what the blocks come to, not how script writes them:
-- script often says a thing of several states in wording that differs in some
-- part the wiki does not show, and two states the wiki says the very same thing
-- of are worth naming together however differently they were written. Each
-- block is written out the once, here, and the run keeps what it came to.
--
-- A state scope standing on its own is left as the plain statement it was.
chunkStates :: (HOI4Info g, Monad m) => [ScriptChunk] -> PPT g m [ScriptChunk]
chunkStates chunks = do
    quiet <- traverse saysNothing chunks
    concat <$> traverse chunkRun (groupBy alongside (zip chunks quiet))
    where
        -- What says nothing stands between two states without parting them:
        -- script spaces its blocks out with tooltips whose text is empty, and
        -- a run the game shows as one should not be broken up by a blank line
        -- that the wiki does not draw in the first place.
        alongside (one, _) (two, quiettwo) =
            isJust (stateScope one) && (quiettwo || isJust (stateScope two))
        chunkRun run = case mapMaybe (stateScope . fst) run of
            states@(_:_:_) -> traverse together . groupBy sameSaid =<< traverse said states
            _ -> return (map fst run)
        said (n, block) = do
            block_pp <- scope HOI4ScopeState (ppMany block)
            saidas <- imsg2doc block_pp
            return (n, Doc.doc2text saidas, block_pp)
        sameSaid (_, one, _) (_, two, _) = one == two
        together shared = do
            heading <- case [n | (n, _, _) <- shared] of
                [n] -> getStateLoc n
                ns -> return (Doc.doc2text (template "states" (map (T.pack . show) ns)))
            return (StateChunk heading (said_pp (head shared)))
        said_pp (_, _, block_pp) = block_pp
        stateScope (PlainStmt (Statement (IntLhs n) OpEq (CompoundRhs block))) = Just (n, block)
        stateScope _ = Nothing

-- | Whether a chunk comes to nothing a reader sees. Only the tooltips are asked,
-- since a tooltip with no text is how script spaces its blocks out and is the
-- one thing written to be seen and yet show nothing.
saysNothing :: (HOI4Info g, Monad m) => ScriptChunk -> PPT g m Bool
saysNothing (PlainStmt stmt@[pdx| custom_effect_tooltip = %_ |]) = null <$> ppOne stmt
saysNothing (PlainStmt stmt@[pdx| tooltip = %_ |]) = null <$> ppOne stmt
saysNothing _ = return False

-- | Group each tooltip saying that ideas become available in (or leave) an
-- advisor or company slot with the @show_ideas_tooltip@ statements it heads, so
-- that the ideas can be listed under it. A tooltip that heads nothing is left
-- where it was, to be shown as the ordinary tooltip it is.
chunkIdeaSlots :: [ScriptChunk] -> [ScriptChunk]
chunkIdeaSlots = reverse . foldl' addChunk []
    where
        addChunk chunks chunk = case chunk of
            PlainStmt stmt
                | isSlotTooltip stmt -> IdeaSlotChunk stmt [] : chunks
                | isShownIdea stmt
                , IdeaSlotChunk tt ideas : rest <- chunks ->
                    IdeaSlotChunk tt (ideas ++ [stmt]) : rest
            _ -> chunk : chunks
        -- The slot a run of ideas belongs to is named by the tooltip's key, and
        -- the game keeps to @available_@ and @remove_@ for those.
        isSlotTooltip [pdx| custom_effect_tooltip = $key |] =
            any (`T.isPrefixOf` key) ["available_", "remove_"]
        isSlotTooltip _ = False
        isShownIdea [pdx| show_ideas_tooltip = $_ |] = True
        isShownIdea _ = False

-- | Present the tooltip announcing a change to an advisor or company slot as a
-- heading for the ideas it names, and those ideas under it. The trailing
-- newline that spaces the tooltip out in game is noise on the wiki, and so is
-- the @Custom effect tooltip:@ that an ordinary tooltip is labelled with: what
-- follows the heading says plainly enough that it is one.
ppIdeaSlotChunk :: forall g m. (HOI4Info g, Monad m) =>
    GenericStatement -> [GenericStatement] -> PPT g m IndentedMessages
ppIdeaSlotChunk [pdx| %_ = $key |] ideas@(_:_) = do
    loc <- T.strip <$> getGameL10n key
    -- Without a heading there is nothing for the ideas to be listed under.
    if T.null loc then concatMapM showIdea ideas else do
        headmsg <- plainMsg loc
        ideamsg <- concatMapM showIdeaUnderHeading ideas
        return $ headmsg ++ ideamsg
ppIdeaSlotChunk tt ideas = concatMapM ppOne (tt : ideas)

-- | Split a script into chunks, grouping each run of consecutive statements
-- that write to the variables of one and the same dynamic modifier.
chunkDynModVars :: (HOI4Info g, Monad m) => GenericScript -> PPT g m [ScriptChunk]
chunkDynModVars scr
    | not (any (isJust . dynModVarOp) scr) = return (map PlainStmt scr)
    | otherwise = do
        varTable <- dynModVarTable <$> getDynamicModifiers
        return $ reverse $ foldl' (addChunk varTable) [] scr
    where
        addChunk table chunks stmt = case resolve table =<< dynModVarOp stmt of
            Nothing -> PlainStmt stmt : chunks
            Just (dmod, key, isSet, val) -> case chunks of
                DynModChunk dmod' isSet' mods : rest
                    | dmodName dmod' == dmodName dmod && isSet' == isSet ->
                        DynModChunk dmod' isSet' (mods ++ [(key, val)]) : rest
                -- The tooltip right before such a run is the game's way of
                -- announcing which modifier is about to change; the effect box
                -- says as much, so drop it rather than say it twice.
                PlainStmt prev : rest | isCustomTooltip prev ->
                    DynModChunk dmod isSet [(key, val)] : rest
                _ -> DynModChunk dmod isSet [(key, val)] : chunks
        resolve table (op, var, val) = do
            (dmod, key) <- HM.lookup var table
            return (dmod, key, op == DynModSet, if op == DynModSub then negate val else val)
        isCustomTooltip [pdx| custom_effect_tooltip = %_ |] = True
        isCustomTooltip _ = False

-- | Map each variable that a dynamic modifier reads a value from to that
-- modifier and the modifier key it supplies.
dynModVarTable :: HashMap Text HOI4DynamicModifier -> HashMap Text (HOI4DynamicModifier, Text)
dynModVarTable = HM.fromList . concatMap entries . HM.elems
    where
        entries dmod = mapMaybe (entry dmod) (dmodEffects dmod)
        entry dmod [pdx| $key = $var |] = Just (var, (dmod, key))
        entry _ _ = Nothing

-- | Recognise an effect that writes a plain number to a single variable, i.e.
-- @add_to_variable = { some_var = 0.025 }@ or the @var@/@value@ spelling of it.
-- Anything more involved is left to the ordinary variable handler.
dynModVarOp :: GenericStatement -> Maybe (DynModOp, Text, Double)
dynModVarOp [pdx| $lhs = @scr |] = case theop of
        Nothing -> Nothing
        Just op -> case foldl' addLine (Nothing, Nothing, False) scr of
            (Just var, Just val, False) -> Just (op, var, val)
            _ -> Nothing
    where
        theop = case lhs of
            "set_variable" -> Just DynModSet
            "add_to_variable" -> Just DynModAdd
            "subtract_from_variable" -> Just DynModSub
            _ -> Nothing
        -- The third component flags anything unexpected in the statement.
        addLine acc@(mvar, mval, bad) stmt = case stmt of
            [pdx| var = ?v |]
                | isNothing mvar -> (Just v, mval, bad)
            [pdx| value = !n |]
                | isNothing mval -> (mvar, Just n, bad)
            [pdx| tooltip = %_ |] -> acc
            [pdx| $v = !n |]
                | isNothing mvar && isNothing mval -> (Just v, Just n, bad)
            _ -> (mvar, mval, True)
dynModVarOp _ = Nothing

-- | Present a run of writes to a dynamic modifier's variables as an effect box
-- listing what the modifier grants after the change.
ppDynModChunk :: forall g m. (HOI4Info g, Monad m) =>
    HOI4DynamicModifier -> Bool -> [(Text, Double)] -> PPT g m IndentedMessages
ppDynModChunk dmod isSet mods = do
    headmsg <- msgToPP $ if isSet then MsgSetDynamicModifier else MsgModifyDynamicModifier
    box <- ppDynModBox dmod (map modStmt mods)
    return (headmsg ++ box)
    where
        modStmt (key, val) = Statement (GenericLhs key []) OpEq (FloatRhs val)
