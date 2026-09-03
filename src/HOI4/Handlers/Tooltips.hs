{-|
Module      : HOI4.Handlers.Tooltips
Description : Tooltips

Handlers for the tooltips script writes to say in a sentence what its
machinery comes to, and the readers of the localization keys and formatters
those tooltips are written with.
-}
module HOI4.Handlers.Tooltips (
        effectTooltip
    ,   eventOptionTooltip
    ,   tooltipWith
    ,   customTriggerTooltip
    ,   customOverrideTooltip
    ,   locKeyText
    ) where

import Data.Char (isDigit)
import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (typewriterText)
import QQ -- everything
import SettingsTypes (PPT, indentUp, indentDown, concatMapM, LocArg (..), setIsInEffect)
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (compoundMessage, msgToPP, plainMsg', preStatement, tooltipText)
import HOI4.Handlers.Modifiers (handleModifier, modifierMSG)

-- | Handler for @effect_tooltip@, which shows the effects inside it without
-- executing them.
effectTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
effectTooltip [pdx| %_ = @scr |]
    -- The tooltip says what the effects inside it would do without doing them,
    -- so what it holds is written where the tooltip itself stood. A line of its
    -- own would say nothing, and the indent that comes with one would put its
    -- contents a step further in than the effects standing beside it.
    = indentDown (ppMany scr)
effectTooltip stmt = preStatement stmt

--------------
-- tooltips --
--------------

-- | Handler for @custom_effect_tooltip@. A tooltip is usually a localization key
-- on its own, but it can also be a block naming the key together with values for
-- the arguments its text is written with.
--
-- Tooltips are worth the trouble of reading: script hides the machinery of an
-- effect in a @hidden_effect@ and puts a tooltip next to it saying in one
-- sentence what the machinery comes to, so the text a tooltip names is the
-- game's own summary of an effect we would otherwise have to spell out.
-- | Handler for @event_option_tooltip@, which stands next to the effect that
-- will offer an event and says what picking one of its options comes to. What
-- the game draws is that option's effects, not the words written on its button,
-- so the effects are what we write out.
--
-- The option is named by its localization key, which by convention is the id of
-- the event it belongs to with the option's letter on the end, so the event to
-- look in is everything up to the last dot.
eventOptionTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
eventOptionTooltip stmt@[pdx| %_ = ?key |] = do
    events <- getEvents
    let eid = T.dropEnd 1 (T.dropWhileEnd (/= '.') key)
    case HM.lookup eid events of
        Just evt | Just opt <- find isNamedKey (fromMaybe [] (hoi4evt_options evt)) -> do
            -- The effects belong to that event, so the pronouns mean what
            -- they do inside it. Its FROM is whoever fires it: script writes
            -- this tooltip beside its own firing of the event, so the ROOT
            -- of the script in hand is who that is, and failing that, an
            -- event everything fires from one country is that country's.
            -- Its ROOT -- the recipient -- is not known here.
            mfirer <- getRootIdent >>= \case
                Just val -> return (Just val)
                Nothing -> fmap ScopeValTag <$> eventFirerTag eid
            withRootIdent Nothing $ withFromIdent mfirer $ withThisIdent Nothing $
                setIsInEffect True (ppMany (fromMaybe [] (hoi4opt_effects opt)))
        _ -> preStatement stmt
    where isNamedKey opt = hoi4opt_name opt == Just key
eventOptionTooltip stmt = preStatement stmt

tooltipWith :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
tooltipWith msg stmt@[pdx| %_ = @scr |] =
    case extractStmt (matchLhsText "localization_key") scr of
        (Just [pdx| %_ = ?key |], rest) -> do
            args <- HM.fromList . catMaybes <$> traverse locArg rest
            tooltipText msg =<< locKeyText args key
        _ -> preStatement stmt
tooltipWith msg [pdx| %_ = ?key |] = tooltipText msg =<< locKeyText HM.empty key
tooltipWith _ stmt = preStatement stmt

-- | Handler for @custom_trigger_tooltip@, which shows its own sentence in
-- place of the triggers inside it. That sentence is all the game ever shows
-- of the block, so it stands alone where the block stood, with the hidden
-- triggers dropped: a lone line can read on from whatever the block was
-- written under, where a heading and a list cannot. A block with no sentence
-- to show falls back to listing its triggers under a heading, which is more
-- than the game says, but the wiki has nothing else to say there.
customTriggerTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
customTriggerTooltip stmt@[pdx| %_ = @scr |] = do
    shown <- concatMapM (tooltipWith MsgUnprocessed) [tt | tt@[pdx| tooltip = %_ |] <- scr]
    if null shown
        then compoundMessage MsgCustomTriggerTooltip stmt
        else return shown
customTriggerTooltip stmt = preStatement stmt

-- | Handler for @custom_override_tooltip@, which runs the effects inside it but
-- shows only its own sentence in place of theirs. What the game hides here is
-- exactly what the wiki is for, so the sentence heads the block and the effects
-- are written out under it.
customOverrideTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
customOverrideTooltip stmt@[pdx| %_ = @scr |] = do
    let (mtooltip, rest) = extractStmt (matchLhsText "tooltip") scr
    ttmsg <- maybe (return []) (tooltipWith MsgTooltip) mtooltip
    script_pp'd <- ppMany rest
    return $ ttmsg ++ script_pp'd
customOverrideTooltip stmt = preStatement stmt

-- | The text a tooltip's localization key comes to. A key is usually just a key,
-- but it can instead ask one of the game's formatters for something the game
-- works out as it draws the tooltip, written as the formatter's name, a @|@, and
-- the token to apply it to.
locKeyText :: (HOI4Info g, Monad m) => HashMap Text LocArg -> Text -> PPT g m Text
locKeyText args key = case T.stripPrefix "|" rest of
    Just token -> fromMaybe (typewriterText key) <$> locFormatter fmt token
    Nothing -> fillConstants =<< getGameL10nArgs args key
    where (fmt, rest) = T.breakOn "|" key

-- | What one of the game's localization formatters comes to when applied to a
-- token, or 'Nothing' for one we cannot work out, which is left as written for a
-- human to fill in. The eight the game documents are of two kinds: those that
-- name or describe one thing, which come to a phrase, and those that say what
-- something grants, which come to a line apiece.
--
-- Several are documented as needing the country the tooltip is drawn for, but
-- only @country_culture@ reads it for anything: to prefer a country's own wording
-- over the generic one. A wiki page has no country, so the generic wording is
-- what we use, which is the wording the game itself uses for every country that
-- has nothing of its own to say.
locFormatter :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m (Maybe Text)
locFormatter fmt token = case fmt of
    "character_name" -> Just <$> getCharacterName token
    "idea_name" -> do
        ides <- getIdeas
        traverse (getGameL10n . id_name) (HM.lookup token ides)
    "idea_desc" -> do
        ides <- getIdeas
        return (id_desc_loc =<< HM.lookup token ides)
    -- An advisor is an idea, and the post's own entry names the character rather
    -- than describing them, so the description wanted is the idea's.
    "advisor_desc" -> do
        charto <- getCharToken
        descOf $ case HM.lookup token charto of
            Just adv -> adv_idea_token adv
            Nothing -> token
    "country_leader_desc" -> descOf token
    "country_culture" -> getGameL10nIfPresent token
    "building_state_modifier" -> do
        blds <- getBuildings
        case bld_state_modifiers =<< HM.lookup token blds of
            Just mods -> Just <$> (messageLines =<< handleModifier mods)
            Nothing -> return Nothing
    "tech_effect" -> do
        techs <- concat . HM.elems <$> getTechnologies
        case find ((token ==) . tech_id) techs of
            Just tech -> Just <$> (messageLines =<< techEffect tech)
            Nothing -> return Nothing
    _ -> return Nothing
    where descOf key = getGameL10nIfPresent (key <> "_desc")

-- | Write out messages as the lines of a tooltip's text. A tooltip goes into a
-- single list item, so what would otherwise have been the list's own nesting has
-- to be spelled out. The game asks for a particular indent with an @INDENT@
-- argument, which wiki markup has no use for.
messageLines :: (HOI4Info g, Monad m) => IndentedMessages -> PPT g m Text
messageLines msgs = T.intercalate "\n" <$> traverse line msgs
    where
        base = if null msgs then 0 else minimum (map fst msgs)
        line (i, msg) = (T.replicate (2 * max 0 (i - base)) "&nbsp;" <>) . T.strip
                            <$> messageText msg

-- | What finishing a technology comes to: whatever it makes available, the
-- modifiers it gives the country, and the stats it improves for the units it
-- applies to.
techEffect :: (HOI4Info g, Monad m) => HOI4Technology -> PPT g m IndentedMessages
techEffect tech = do
    units <- getUnit
    unittags <- getUnitTag
    let isStatBlock stmt = case stmt of
            [pdx| $what = @_ |] -> what `elem` units || what `elem` unittags
            _ -> False
        available = concatMap (fromMaybe [])
            [tech_equipment tech, tech_modules tech, tech_units tech]
    availmsg <- concatMapM (\what -> msgToPP . MsgTechEnables =<< getGameL10n what) available
    buildmsg <- concatMapM building (fromMaybe [] (tech_buildings tech))
    globalmsg <- fold <$> traverse (modifierMSG False "") (fromMaybe [] (tech_globalmod tech))
    statmsg <- concatMapM statBlock (filter isStatBlock (fromMaybe [] (tech_sortrest tech)))
    return $ availmsg ++ buildmsg ++ globalmsg ++ statmsg
    where
        -- A technology raises the level a building may be built to rather than
        -- simply making it available, so the level is worth saying.
        building scr = do
            let (mbuilding, rest) = extractStmt (matchLhsText "building") scr
                (mlevel, _) = extractStmt (matchLhsText "level") rest
            case mbuilding of
                Just [pdx| %_ = $what |] -> do
                    whatloc <- getGameL10n what
                    case mlevel of
                        Just [pdx| %_ = !level |] -> msgToPP (MsgTechEnablesBuilding whatloc level)
                        _ -> msgToPP (MsgTechEnables whatloc)
                _ -> return []
        -- The stats a technology improves are written under the unit or the class
        -- of units they apply to, which the game picks out in yellow.
        statBlock [pdx| $what = @scr |] = do
            whatloc <- Doc.oneLine <$> getGameL10n what
            headmsg <- plainMsg' ("'''" <> whatloc <> "'''")
            modmsg <- fold <$> indentUp (traverse (modifierMSG False "") scr)
            return (headmsg : modmsg)
        statBlock stmt = preStatement stmt

-- | The value a tooltip gives for one of the arguments its text is written with,
-- ready to be written into that text.
locArg :: (HOI4Info g, Monad m) => GenericStatement -> PPT g m (Maybe (Text, LocArg))
-- An argument can be a tooltip in its own right, written the same way.
locArg [pdx| $name = @scr |] = case extractStmt (matchLhsText "localization_key") scr of
    (Just [pdx| %_ = ?key |], rest) -> do
        inner <- HM.fromList . catMaybes <$> traverse locArg rest
        Just . (,) name . LocText <$> locKeyText inner key
    _ -> return Nothing
locArg [pdx| $name = !num |] = return (Just (name, LocNum num))
locArg [pdx| $name = ?val |] = Just . (,) name . LocText <$> locArgValue val
locArg _ = return Nothing

-- | Read one tooltip argument's value. Most of them name localization of their
-- own, some ask a formatter for text the game works out for itself, and the rest
-- are the game reading something out of its own state, where the script says how
-- to find it but not what it will say. Those last are left as written for a human
-- to fill in, except for a state's name, which we can look up.
locArgValue :: (HOI4Info g, Monad m) => Text -> PPT g m Text
locArgValue val
    -- A promoted scope, e.g. "[70.GetName]".
    | Just inner <- T.stripPrefix "[" =<< T.stripSuffix "]" unquoted
        = case T.breakOn "." inner of
            (sid, _) | not (T.null sid), T.all isDigit sid
                -> getStateLoc (read (T.unpack sid))
            _ -> return (typewriterText unquoted)
    | otherwise = locKeyText HM.empty unquoted
    where unquoted = T.dropAround (== '"') val
