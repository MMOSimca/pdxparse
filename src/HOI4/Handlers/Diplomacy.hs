{-|
Module      : HOI4.Handlers.Diplomacy
Description : Relations between countries

Handlers for opinions, war goals and declarations of war, annexation,
peace and truces, factions, relation rules, and border wars.
-}
module HOI4.Handlers.Diplomacy (
        opinion
    ,   hasOpinion
    ,   addNamedThreat
    ,   createWargoal
    ,   removeWargoal
    ,   declareWarOn
    ,   annexCountry
    ,   addToWar
    ,   hasWarGoalAgainst
    ,   diplomaticRelation
    ,   startBorderWar
    ,   cancelBorderWar
    ,   finalizeBorderWar
    ,   setBorderWarData
    ,   createFactionFromTemplate
    ,   setTruce
    ,   whitePeace
    ,   puppetCountry
    ,   relationModifier
    ,   addRelationRuleOverride
    ) where

import Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', intercalate)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (foldM)

import Debug.Trace

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (plural, iquotes, typewriterText, colourNumSign)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, withCurrentIndent)
import StatementUtils -- everything

import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Templates
import HOI4.Types -- everything

import HOI4.Handlers.Core (msgToPP, preMessage, preStatement, statedFrom, tooltipText)
import HOI4.Handlers.Generic (textAtom, withFlag)
import HOI4.Handlers.Modifiers (modifierMSG)
import HOI4.Handlers.Tooltips (locKeyText)

-- Opinions

-- Add an opinion modifier towards someone (for a number of years).
data AddOpinion = AddOpinion {
        op_who :: Maybe (Either Text (Text, Text))
    ,   op_modifier :: Maybe Text
    ,   op_years :: Maybe Double
    } deriving Show

newAddOpinion :: AddOpinion
newAddOpinion = AddOpinion Nothing Nothing Nothing

-- | Handler for the effects that hand a country an opinion modifier towards
-- another, or take one away again.
--
-- An opinion modifier marked @trade@ moves how the two trade with one another
-- rather than what they think of one another, and the game says so in words of
-- its own. Which of the two an effect comes to is settled by the modifier it
-- names, not by the effect, so the modifier is looked up to tell them apart.
-- The trade wording says nothing of how long it lasts, so it is used however
-- long script asks for.
opinion :: (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> ScriptMessage)
        -> (Text -> Text -> Text -> Double -> ScriptMessage)
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
opinion msgIndef msgDur msgTrade stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_add_opinion (foldl' addLine newAddOpinion scr)
    where
        addLine :: AddOpinion -> GenericStatement -> AddOpinion
        addLine op [pdx| target        = $tag         |] = op { op_who = Just (Left tag) }
        addLine op [pdx| target        = $vartag:$var |] = op { op_who = Just (Right (vartag, var)) }
        addLine op [pdx| modifier      = ?label       |] = op { op_modifier = Just label }
        addLine op [pdx| years         = !n           |] = op { op_years = Just n }
        -- following two for add_mutual_opinion_modifier_effect
        addLine op [pdx| scope_country = $tag         |] = op { op_who = Just (Left tag) }
        addLine op [pdx| scope_country = $vartag:$var |] = op { op_who = Just (Right (vartag, var)) }
        addLine op [pdx| opinion_modifier = ?label    |] = op { op_modifier = Just label }
        addLine op stmt = warn (UnknownSection "opinion" stmt) op
        pp_add_opinion op = case (op_who op, op_modifier op) of
            (Just ewhom, Just modifier) -> do
                mwhomflag <- eflag (Just HOI4Country) ewhom
                mod_loc <- getGameL10n modifier
                omods <- getOpinionModifiers
                let isTrade = maybe False (fromMaybe False . omodTrade)
                                    (HM.lookup modifier omods)
                case (mwhomflag, op_years op) of
                    (Just whomflag, _) | isTrade -> return $ msgTrade modifier mod_loc whomflag
                    (Just whomflag, Nothing) -> return $ msgIndef modifier mod_loc whomflag
                    (Just whomflag, Just years) -> return $ msgDur modifier mod_loc whomflag years
                    _ -> return (preMessage stmt)
            _ -> warn (BadValue "opinion" stmt) $ return (preMessage stmt)
opinion _ _ _ stmt = preStatement stmt

data HasOpinion = HasOpinion
        {   hop_target :: Maybe Text
        ,   hop_value :: Maybe Double
        ,   hop_valuevar :: Maybe Text
        ,   hop_ltgt :: Text
        }

newHasOpinion :: HasOpinion
newHasOpinion = HasOpinion Nothing Nothing Nothing undefined

hasOpinion :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> ScriptMessage) ->
    StatementHandler g m
hasOpinion msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_hasOpinion (foldl' addLine newHasOpinion scr)
    where
        addLine :: HasOpinion -> GenericStatement -> HasOpinion
        addLine hop [pdx| target = ?target |] = hop { hop_target = Just target }
        addLine hop [pdx| value = !val |] = hop { hop_value = Just val, hop_ltgt = "equal to or more than" } -- at least
        addLine hop [pdx| value > !val |] = hop { hop_value = Just val, hop_ltgt = "more than" } -- at least
        addLine hop [pdx| value < !val |] = hop { hop_value = Just val, hop_ltgt = "less than" } -- less than
        addLine hop [pdx| value = $val |] = hop { hop_valuevar = Just val, hop_ltgt = "equal to or more than" } -- at least
        addLine hop [pdx| value > $val |] = hop { hop_valuevar = Just val, hop_ltgt = "more than" } -- at least
        addLine hop [pdx| value < $val |] = hop { hop_valuevar = Just val, hop_ltgt = "less than" } -- less than
        addLine hop _ = warn (UnknownSection "has_opinion" stmt) hop
        pp_hasOpinion :: HasOpinion -> PPT g m ScriptMessage
        pp_hasOpinion hop = case (hop_target hop, hop_value hop, hop_valuevar hop, hop_ltgt hop) of
            (Just target, Just value, _, ltgt) -> do
                target_flag <- flagText (Just HOI4Country) target
                let valuet = templateColor' (colourNumSign True value)
                return (msg valuet target_flag ltgt)
            (Just target, _, Just valuet, ltgt) -> do
                target_flag <- flagText (Just HOI4Country) target
                return (msg valuet target_flag ltgt)
            _ -> return (preMessage stmt)
hasOpinion _ stmt = preStatement stmt

----------------------------------
-- Handler for add_named_threat --
----------------------------------
foldCompound "addNamedThreat" "NamedThreat" "nt"
    []
    [CompField "threat" [t|Double|] Nothing True
    ,CompField "name" [t|Text|] Nothing True
    ]
    [|  do
        threatLoc <- getGameL10n _name
        tensionLoc <- getGameL10n "WORLD_TENSION_NAME"
        return $ MsgAddNamedThreat tensionLoc _threat threatLoc
    |]

----------------------------------
-- Handler for create_wargoal --
----------------------------------

----------------------------------
-- Handler for declare_war_on --
----------------------------------

-- | What @create_wargoal@ and @declare_war_on@ both write: the wargoal type,
-- the country it is against, and the states it is over.
data WarGoal = WarGoal
    {   war_type :: Maybe Text
    ,   war_type_loc :: Maybe Text
    ,   war_target_flag :: Maybe Text
    ,   war_expire :: Maybe Double
    ,   war_generator :: Maybe (Either Text [Int]) -- ^ a variable, or the states themselves
    ,   war_states :: [Text]
    } deriving Show

parseWarGoal :: forall g m. (HOI4Info g, Monad m) => String -> GenericScript -> PPT g m WarGoal
parseWarGoal what = foldM addLine (WarGoal Nothing Nothing Nothing Nothing Nothing [])
    where
        addLine :: WarGoal -> GenericStatement -> PPT g m WarGoal
        addLine wg [pdx| type = $wargoal |]
            = (\wgtype_loc -> wg
                   { war_type = Just wargoal
                   , war_type_loc = Just wgtype_loc })
              <$> getGameL10n wargoal
        addLine wg [pdx| target = $vartag:$var |]
            = (\target_loc -> wg { war_target_flag = target_loc })
              <$> eflag (Just HOI4Country) (Right (vartag, var))
        addLine wg [pdx| target = ?target |]
            = (\target_loc -> wg { war_target_flag = target_loc })
              <$> eflag (Just HOI4Country) (Left target)
        addLine wg [pdx| expire = %rhs |]
            = return wg { war_expire = floatRhs rhs }
        addLine wg stmts@[pdx| generator = %state |] = case state of
            CompoundRhs array -> do
                let states = mapMaybe stateFromArray array
                statesloc <- traverse getStateLoc states
                return wg { war_generator = Just (Right states)
                          , war_states = statesloc }
            IntRhs intstate -> do
                statesloc <- getStateLoc intstate
                return wg { war_generator = Just (Right [intstate])
                          , war_states = [statesloc] }
            GenericRhs _vartag [vstate] ->
                return wg { war_generator = Just (Left vstate) } --Need to deal with existing variables here
            GenericRhs vstate _ ->
                return wg { war_generator = Just (Left vstate) } --Need to deal with existing variables here
            _ -> warn (BadValue (T.pack (what ++ " generator")) stmts) $ return wg
        addLine wg stmt
            = warn (UnknownSection (T.pack what) stmt) $ return wg

        stateFromArray :: GenericStatement -> Maybe Int
        stateFromArray (StatementBare (IntLhs e)) = Just e
        stateFromArray stmt = warn (UnknownSection "generator array" stmt) Nothing

createWargoal :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createWargoal stmt@[pdx| %_ = @scr |] =
    msgToPP . pp_create_wg =<< parseWarGoal "create_wargoal" scr
    where
        pp_create_wg :: WarGoal -> ScriptMessage
        pp_create_wg wg =
            let states = case war_generator wg of
                    Just (Right arr) -> T.pack $ concat [" for the ", T.unpack $ plural (length arr) "state " "states " , intercalate ", " $ map T.unpack (war_states wg)]
                    Just (Left var) -> " for the states in " <> typewriterText var
                    _ -> ""
            in case (war_type wg, war_type_loc wg,
                     war_target_flag wg,
                     war_expire wg) of
                (Nothing, _, _, _) -> preMessage stmt -- need WG type
                (_, _, Nothing, _) -> preMessage stmt -- need target
                (_, Just wgtype_loc, Just target_flag, Just days) -> MsgCreateWGDuration wgtype_loc target_flag days states
                (Just wgtype, Nothing, Just target_flag, Just days) -> MsgCreateWGDuration wgtype target_flag days states
                (_, Just wgtype_loc, Just target_flag, Nothing) -> MsgCreateWG wgtype_loc target_flag states
                (Just wgtype, Nothing, Just target_flag, Nothing) -> MsgCreateWG wgtype target_flag states
createWargoal stmt = preStatement stmt

-- | Takes away a war goal the country holds. It names the goal and whoever it
-- is held against exactly as @create_wargoal@ does.
removeWargoal :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
removeWargoal stmt@[pdx| %_ = @scr |] =
    msgToPP . pp_remove_wg =<< parseWarGoal "remove_wargoal" scr
    where
        pp_remove_wg :: WarGoal -> ScriptMessage
        pp_remove_wg wg = case (war_type wg, war_type_loc wg, war_target_flag wg) of
            (Nothing, _, _) -> preMessage stmt -- need WG type
            (_, _, Nothing) -> preMessage stmt -- need target
            (_, Just wgtype_loc, Just target_flag) -> MsgRemoveWargoal wgtype_loc target_flag
            (Just wgtype, Nothing, Just target_flag) -> MsgRemoveWargoal wgtype target_flag
removeWargoal stmt = preStatement stmt

declareWarOn :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
declareWarOn stmt@[pdx| %_ = @scr |] =
    msgToPP . pp_declare_war =<< parseWarGoal "declare_war_on" scr
    where
        pp_declare_war :: WarGoal -> ScriptMessage
        pp_declare_war wg =
            let states = case war_generator wg of
                    Just (Right arr) -> T.pack $ concat ["for the ", T.unpack $ plural (length arr) "state " "states " , intercalate ", " $ map T.unpack (war_states wg)]
                    Just (Left var) -> T.pack ("for " ++ T.unpack var)
                    _ -> ""
            in case (war_type wg, war_type_loc wg,
                     war_target_flag wg) of
                (Nothing, _, _) -> preMessage stmt -- need DW type
                (_, _, Nothing) -> preMessage stmt -- need target
                (_, Just dwtype_loc, Just target_flag) -> MsgDeclareWarOn target_flag dwtype_loc states
                (Just dwtype, Nothing, Just target_flag) -> MsgDeclareWarOn target_flag dwtype states
declareWarOn stmt = preStatement stmt

----------------------------------
-- Handler for annex_country --
----------------------------------
foldCompound "annexCountry" "AnnexCountry" "an"
    []
    [CompField "target" [t|Text|] Nothing True
    ,CompField "transfer_troops" [t|Text|] Nothing False
    ]
    [|  do
        let transferTroops = case _transfer_troops of
                Just "yes" -> " (troops transferred)"
                Just "no" -> " (troops not transferred)"
                _ -> ""
        targetTag <- flagText (Just HOI4Country) _target
        return $ MsgAnnexCountry targetTag transferTroops
    |]

----------------------------------
-- Handler for add_to_war --
----------------------------------
foldCompound "addToWar" "AddToWar" "atw"
    []
    [CompField "targeted_alliance" [t|Text|] Nothing True
    ,CompField "enemy" [t|Text|] Nothing True
    ,CompField "hostility_reason" [t|Text|] Nothing False -- guarantee, asked_to_join, war, ally
    -- Joins only the war against the named enemy instead of every war the
    -- ally is in; nothing worth writing out beyond the war already named.
    ,CompField "single_target_only" [t|Text|] Nothing False
    ]
    [|  do
        let reason = case _hostility_reason of
                Just "guarantee" -> ""
                Just "asked_to_join" -> ""
                Just "war" -> ""
                Just "ally" -> ""
                _ -> ""
        ally <- flagText (Just HOI4Country) _targeted_alliance
        enemy <- flagText (Just HOI4Country) _enemy
        return $ MsgAddToWar ally enemy reason
    |]

hasWarGoalAgainst :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
hasWarGoalAgainst stmt@[pdx| %_ = $txt |] = withFlag MsgHasWargoalAgainst stmt
-- A war goal of no named type is only the country it is held against, which is
-- what the trigger's plain form says of it.
hasWarGoalAgainst stmt@[pdx| %_ = @scr |] = case extractStmt (matchLhsText "type") scr of
    (Just _, _) -> textAtom "target" "type" MsgHasWargoalAgainstType (fmap Just . flagText (Just HOI4Country)) stmt
    _ -> case extractStmt (matchLhsText "target") scr of
        (Just target, _) -> withFlag MsgHasWargoalAgainst target
        _ -> preStatement stmt
hasWarGoalAgainst stmt = preStatement stmt

-------------------------------------
-- Handler for diplomatic_relation --
-------------------------------------
foldCompound "diplomaticRelation" "DiplomaticRelation" "dr"
    []
    [CompField "country" [t|Text|] Nothing True
    ,CompField "relation" [t|Text|] Nothing True
    ,CompField "active" [t|Text|] Nothing False
    ]
    [|  do
        let active = case _active of
                Just "no" -> False
                Just "yes" -> True
                _ -> True
            relation = case _relation of
                "non_aggression_pact" -> if active then "Enters a {{icon|nap|1}} with " else "Disbands the {{icon|nap|1}} with "
                "guarantee" -> if active then "Guarantee " else "Revokes guarantee for "
                "puppet" -> if active then "Becomes a subject of " else "Is no longer a subject of "
                "military_access" -> if active then "Grants military access for " else "Revokes military access for "
                "docking_rights" -> if active then "Grants docking rights for " else "Revokes docking rights for "
                _ -> "<!-- Check Script -->"
        flag <- flagText (Just HOI4Country) _country
        return $ MsgDiplomaticRelation relation flag
    |]

----------------------------------
-- handler for start_border_war --
----------------------------------

data StartBorderWar = StartBorderWar
        {   sbw_change_state_after_war :: Bool
        ,   sbw_state_attacker :: Text
        ,   sbw_state_defender :: Text
        ,   sbw_on_win_attacker :: Text
        ,   sbw_on_win_defender :: Text
        ,   sbw_on_loss_attacker :: Text
        ,   sbw_on_loss_defender :: Text
        ,   sbw_on_cancel_attacker :: Text
        ,   sbw_on_cancel_defender :: Text
        }

newSBW :: StartBorderWar
newSBW = StartBorderWar False "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->" "<!--CHECK SCRIPT-->"

startBorderWar :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
startBorderWar stmt@[pdx| %_ = @scr |]
    = ppSBW =<< foldM addLine newSBW scr
    where
        addLine :: StartBorderWar -> GenericStatement -> PPT g m StartBorderWar
        addLine sbw [pdx| change_state_after_war = %rhs |]
            | GenericRhs "yes" [] <- rhs = return sbw { sbw_change_state_after_war = True }
            | otherwise = return sbw
        addLine sbw [pdx| attacker = @scr |] = foldM (addLine' True) sbw scr
        addLine sbw [pdx| defender = @scr |] = foldM (addLine' False) sbw scr
        addLine sbw [pdx| combat_width = %_ |] = return sbw
        addLine sbw [pdx| minimum_duration_in_days = %_ |] = return sbw
        addLine sbw [pdx| dig_in_factor = %_ |] = return sbw
        addLine sbw [pdx| terrain_factor = %_ |] = return sbw
        addLine sbw stmt = warn (UnknownSection "startBorderWar" stmt) $ return sbw

        addLine' :: Bool -> StartBorderWar -> GenericStatement -> PPT g m StartBorderWar
        addLine' atde sbw [pdx| state = !num |] = do
            stateloc <- getStateLoc num
            if atde
            then return  sbw { sbw_state_attacker = stateloc }
            else return sbw { sbw_state_defender = stateloc }
        addLine' atde sbw [pdx| state = $txt |] = do
            if atde
            then return  sbw { sbw_state_attacker = txt }
            else return sbw { sbw_state_defender = txt }
        addLine' atde sbw [pdx| state = $vartag:$var |] = do
            if atde
            then return  sbw { sbw_state_attacker = var }
            else return sbw { sbw_state_defender = var }
        addLine' atde sbw [pdx| on_win = $eid |] = do
            if atde
            then return  sbw { sbw_on_win_attacker = eid }
            else return sbw { sbw_on_win_defender = eid }
        addLine' atde sbw [pdx| on_lose = $eid |] = do
            if atde
            then return  sbw { sbw_on_loss_attacker = eid }
            else return sbw { sbw_on_loss_defender = eid }
        addLine' atde sbw [pdx| on_cancel = $eid |] = do
            if atde
            then return  sbw { sbw_on_cancel_attacker = eid }
            else return sbw { sbw_on_cancel_defender = eid }
        addLine' atde sbw [pdx| num_provinces = %_ |] = return sbw
        addLine' _ sbw [pdx| leader_score = %_ |] = return sbw
        addLine' _ sbw [pdx| dig_in_factor = %_ |] = return sbw
        addLine' _ sbw [pdx| terrain_factor = %_ |] = return sbw
        addLine' _ sbw [pdx| modifier = %_ |] = return sbw
        addLine' _ sbw stmt = warn (UnknownSection "startBorderWar@attdef" stmt) $ return sbw
        ppSBW :: StartBorderWar -> PPT g m IndentedMessages
        ppSBW sbw = do
            headMsg <- msgToPP $ MsgStartBorderWar (sbw_state_attacker sbw) (sbw_state_defender sbw) (sbw_change_state_after_war sbw)
            bordEventMsgs <- do
                onwinmsg <- messageText MsgBorderWin
                onlossmsg <- messageText MsgBorderLoss
                oncancelmsg <- messageText MsgBorderCancel
                defmsg <- messageText MsgBorderDefender
                attmsg <- messageText MsgBorderAttacker
                let getevntloc eid = do
                        mloc <- getEventTitle eid
                        return $ fromMaybe eid mloc
                winattevt <- getevntloc (sbw_on_win_attacker sbw)
                windefevt <- getevntloc (sbw_on_win_defender sbw)
                lossattevt <- getevntloc (sbw_on_loss_attacker sbw)
                lossdefevt <- getevntloc (sbw_on_loss_defender sbw)
                cancattevt <- getevntloc (sbw_on_cancel_attacker sbw)
                cancdefevt <- getevntloc (sbw_on_cancel_defender sbw)
                msgwinatt <- indentUp $ msgToPP $ MsgTriggerBorderEvent onwinmsg (sbw_on_win_attacker sbw) winattevt attmsg
                msgwindeff <- indentUp $ msgToPP $ MsgTriggerBorderEvent onwinmsg (sbw_on_win_defender sbw) windefevt defmsg
                msglossatt <- indentUp $ msgToPP $ MsgTriggerBorderEvent onlossmsg (sbw_on_loss_attacker sbw) lossattevt attmsg
                msglossdef <- indentUp $ msgToPP $ MsgTriggerBorderEvent onlossmsg (sbw_on_loss_defender sbw) lossdefevt defmsg
                msgcancatt <- indentUp $ msgToPP $ MsgTriggerBorderEvent oncancelmsg (sbw_on_cancel_attacker sbw) cancattevt attmsg
                msgcancdef <- indentUp $ msgToPP $ MsgTriggerBorderEvent oncancelmsg (sbw_on_cancel_defender sbw) cancdefevt defmsg
                return $ msgwinatt ++ msgwindeff ++ msglossatt ++ msglossdef ++ msgcancatt ++ msgcancdef
            return $ headMsg ++ bordEventMsgs
startBorderWar stmt = preStatement stmt

-- | The two states a border war is fought between, as every effect that works
-- on an ongoing one names them, along with whatever else the block held.
borderWarSides :: (HOI4Info g, Monad m) =>
    GenericScript -> PPT g m (Text, Text, GenericScript)
borderWarSides scr = do
    let (matt, rest) = extractStmt (matchLhsText "attacker") scr
        (mdef, rest') = extractStmt (matchLhsText "defender") rest
    att <- statedFrom matt
    def <- statedFrom mdef
    return (att, def, rest')

cancelBorderWar :: (HOI4Info g, Monad m) => StatementHandler g m
cancelBorderWar [pdx| %_ = @scr |] = do
    (att, def, _) <- borderWarSides scr
    msgToPP $ MsgCancelBorderWar att def
cancelBorderWar stmt = preStatement stmt

-- | Ends a border war with one side declared the winner. Where neither side is
-- named the winner the war is called off instead, which is what cancelling it
-- comes to.
finalizeBorderWar :: (HOI4Info g, Monad m) => StatementHandler g m
finalizeBorderWar [pdx| %_ = @scr |] = do
    (att, def, rest) <- borderWarSides scr
    let attwin = not (null [() | [pdx| attacker_win = yes |] <- rest])
        defwin = not (null [() | [pdx| defender_win = yes |] <- rest])
    msgToPP $ if attwin then MsgFinalizeBorderWar att def
              else if defwin then MsgFinalizeBorderWar def att
              else MsgCancelBorderWar att def
finalizeBorderWar stmt = preStatement stmt

-- | Changes the terms an ongoing border war is fought on. What it changes is
-- listed under the two states, the way the game's own tooltip lists it.
setBorderWarData :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setBorderWarData [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    (att, def, rest) <- borderWarSides scr
    changes <- indentUp (concat <$> traverse change rest)
    return ((i, MsgSetBorderWarData att def) : changes)
    where
        change :: GenericStatement -> PPT g m IndentedMessages
        change [pdx| combat_width = !num |] = msgToPP $ MsgBorderWarCombatWidth num
        change [pdx| attacker_modifier = !num |] = msgToPP $ MsgBorderWarSideModifier "Attacker" num
        change [pdx| defender_modifier = !num |] = msgToPP $ MsgBorderWarSideModifier "Defender" num
        change [pdx| change_state_after_war = %rhs |]
            | GenericRhs "yes" [] <- rhs = msgToPP $ MsgBorderWarChangeState True
            | otherwise = msgToPP $ MsgBorderWarChangeState False
        -- Whether the events the war was started with fire is nothing the
        -- reader can see either way.
        change [pdx| dont_fire_events = %_ |] = return []
        change stmt = preStatement stmt
setBorderWarData stmt = preStatement stmt

--------------------------------------------
-- Handler for create_faction_from_template --
--------------------------------------------
-- | Handler for @create_faction_from_template@, which starts a faction already
-- set up rather than an empty one. The template is named bare, or given inside a
-- block along with the name the faction is to go by.
createFactionFromTemplate :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createFactionFromTemplate stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (Just tmpl, mname) -> do
            nameloc <- traverse getGameL10n mname
            msgToPP (MsgCreateFactionFromTemplate (fromMaybe "" nameloc) tmpl)
        _ -> preStatement stmt
    where
        addLine (t, n) [pdx| template = $tm |] = (Just tm, n)
        addLine (t, n) [pdx| name = $nm |] = (t, Just nm)
        addLine (t, n) [pdx| name = ?nm |] = (t, Just nm)
        addLine acc [pdx| icon = %_ |] = acc
        addLine acc [pdx| color = %_ |] = acc
        addLine acc stmt = warn (UnknownSection "create_faction_from_template" stmt) acc
createFactionFromTemplate [pdx| %_ = $tmpl |] = msgToPP (MsgCreateFactionFromTemplate "" tmpl)
createFactionFromTemplate stmt = preStatement stmt

-------------------------
-- Handler for set_truce --
-------------------------
-- | Handler for @set_truce@, which bars the two countries from going to war with
-- one another for a time.
setTruce :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setTruce stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (Just target, mdays) -> do
            targetloc <- fromMaybe "" <$> eflag (Just HOI4Country) target
            msgToPP (MsgSetTruce targetloc (fromMaybe 0 mdays))
        _ -> preStatement stmt
    where
        addLine (t, d) [pdx| target = $vartag:$var |] = (Just (Right (vartag, var)), d)
        addLine (t, d) [pdx| target = ?tgt |] = (Just (Left tgt), d)
        addLine (t, d) [pdx| days = !n |] = (t, Just n)
        addLine acc stmt = warn (UnknownSection "set_truce" stmt) acc
setTruce stmt = preStatement stmt

----------------------------
-- Handler for white_peace --
----------------------------
-- | Handler for @white_peace@, which ends the war between the two countries with
-- neither taking anything. The other side is named bare, or given as @tag@
-- inside a block where a @message@ may name the event that tells it so.
whitePeace :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
whitePeace stmt@[pdx| %_ = @scr |] =
    case [inner | inner@[pdx| tag = %_ |] <- scr] of
        (tagstmt : _) -> withFlag MsgMakeWhitePeace tagstmt
        [] -> preStatement stmt
whitePeace stmt = withFlag MsgMakeWhitePeace stmt

-----------------------
-- Handler for puppet --
-----------------------
-- | Handler for @puppet@, which makes the target a subject. Written as a block
-- it also says whether the subject's wars are ended along with it, which they
-- are unless the script says otherwise.
puppetCountry :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
puppetCountry stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, True, True) scr of
        (Just target, endwars, endcivil) -> do
            targetloc <- fromMaybe "" <$> eflag (Just HOI4Country) target
            msgToPP (MsgPuppetCountry targetloc (endWarText endwars endcivil))
        _ -> preStatement stmt
    where
        addLine (t, w, c) [pdx| target = $vartag:$var |] = (Just (Right (vartag, var)), w, c)
        addLine (t, w, c) [pdx| target = ?tgt |] = (Just (Left tgt), w, c)
        addLine (t, w, c) [pdx| end_wars = no |] = (t, False, c)
        addLine (t, w, c) [pdx| end_wars = yes |] = (t, True, c)
        addLine (t, w, c) [pdx| end_civil_wars = no |] = (t, w, False)
        addLine (t, w, c) [pdx| end_civil_wars = yes |] = (t, w, True)
        addLine acc [pdx| always = %_ |] = acc
        addLine acc stmt = warn (UnknownSection "puppet" stmt) acc
puppetCountry stmt = withFlag MsgPuppet stmt

-- | How a subject's own wars are dealt with as it is made one.
endWarText :: Bool -> Bool -> Text
endWarText endwars endcivil = case (endwars, endcivil) of
    (True, True) -> ", ending its wars and civil wars"
    (True, False) -> ", ending its wars"
    (False, True) -> ", ending its civil wars"
    (False, False) -> ""

-----------------------------------------
-- Handler for the relation modifiers  --
-----------------------------------------

-- | Handler for @add_relation_modifier@ and the two statements that go with it.
-- An opinion modifier is only a number moving two countries towards or away from
-- each other; a relation modifier is one of the static modifiers, and what it
-- grants holds for as long as the relation between the two does, so that is
-- written out under it.
relationModifier :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> Bool -> StatementHandler g m
relationModifier msg witheffects stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (Just ewhom, Just modid) -> do
            mwhomflag <- eflag (Just HOI4Country) ewhom
            let whomflag = fromMaybe "<!-- check script -->" mwhomflag
            mmod <- HM.lookup modid <$> getModifiers
            case mmod of
                Just mod -> withCurrentIndent $ \i -> do
                    -- Taking the modifier away, or asking whether it is there,
                    -- says nothing new about what it grants.
                    effect <- if witheffects
                        then fold <$> indentUp (traverse (modifierMSG False "") (modEffects mod))
                        else return []
                    let locName = maybe (typewriterText modid) (Doc.doc2text . iquotes) (modLocName mod)
                    return ((i, msg locName whomflag) : effect)
                Nothing -> trace ("relation modifier not found: " ++ T.unpack modid) $ preStatement stmt
        _ -> warn (BadValue "relation modifier" stmt) $ preStatement stmt
    where
        addLine (whom, modid) [pdx| target = $tag |] = (Just (Left tag), modid)
        addLine (whom, modid) [pdx| target = $vartag:$var |] = (Just (Right (vartag, var)), modid)
        addLine (whom, modid) [pdx| modifier = ?label |] = (whom, Just label)
        addLine acc stmt = warn (UnknownSection "relation modifier" stmt) acc
relationModifier _ _ stmt = preStatement stmt

----------------------------------------
-- Handler for add_relation_rule_override --
----------------------------------------

-- | Handler for @add_relation_rule_override@, which lifts or imposes one of the
-- rules that would otherwise settle what two countries may do with each other.
--
-- Script may hang the override on a trigger rather than on one named country, and
-- where it does it writes out a line of its own saying when the rule applies;
-- that says it better than anything assembled from the rule names would.
addRelationRuleOverride :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addRelationRuleOverride stmt@[pdx| %_ = @scr |] =
    case (mdesc, rules) of
        (Just desc, _) -> tooltipText MsgRelationRuleOverrideDesc =<< locKeyText HM.empty desc
        (_, rules@(_:_)) -> do
            mwhomflag <- maybe (return Nothing) (eflag (Just HOI4Country)) mtarget
            let whomflag = fromMaybe "<!-- check script -->" mwhomflag
            fold <$> traverse (msgToPP . ($ whomflag) . uncurry rule) rules
        _ -> preStatement stmt
    where
        (mtarget, mdesc, rules) = foldl' addLine (Nothing, Nothing, []) scr
        addLine (target, desc, rs) [pdx| target = $tag |] = (Just (Left tag), desc, rs)
        addLine (target, desc, rs) [pdx| target = $vartag:$var |] = (Just (Right (vartag, var)), desc, rs)
        addLine (target, desc, rs) [pdx| usage_desc = ?key |] = (target, Just key, rs)
        -- The trigger deciding when the override holds is a named block of
        -- script, and the line under @usage_desc@ is the game's own reading of it.
        addLine acc [pdx| trigger = %_ |] = acc
        addLine (target, desc, rs) [pdx| $what = %rhs |]
            | GenericRhs "yes" [] <- rhs = (target, desc, rs ++ [(what, True)])
            | GenericRhs "no" [] <- rhs = (target, desc, rs ++ [(what, False)])
        addLine acc stmt = warn (UnknownSection "add_relation_rule_override" stmt) acc

        rule "can_send_volunteers" yn = MsgRelationRuleVolunteers yn
        rule "can_access_market" yn = MsgRelationRuleMarket yn
        rule what yn = MsgRelationRuleOther what yn
addRelationRuleOverride stmt = preStatement stmt
