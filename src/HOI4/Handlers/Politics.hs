{-|
Module      : HOI4.Handlers.Politics
Description : A country's politics

Handlers for ideologies and parties, autonomy, the ruling party and its
leader, game rules, popularities, resistance and the balance of power.
-}
module HOI4.Handlers.Politics (
        beliefIcon
    ,   partyIconOf
    ,   ideologyIconLoc
    ,   partyIconLoc
    ,   withPartyIcon
    ,   setAutonomy
    ,   setPolitics
    ,   hasCountryLeader
    ,   setPartyName
    ,   startCivilWar
    ,   setRule
    ,   addAutonomyRatio
    ,   setCapital
    ,   setPopularities
    ,   forceEnableResistance
    ,   powerBalanceRange
    ,   compareAutonomyState
    ,   setPowerBalance
    ,   getHighestScoredCountry
    ,   addContestedOwner
    ,   addResistanceTarget
    ,   addPowerBalanceModifier
    ,   addPowerBalanceValue
    ) where

import Data.Char (toUpper)
import Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (foldM)

import Debug.Trace

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (iquotes, typewriterText, colourPc, reducedNum)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, withCurrentIndent)
import StatementUtils -- everything

import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Templates
import HOI4.Types -- everything
import HOI4.WikiTables (iconTerm)

import HOI4.Handlers.Core (msgToPP, preMessage, preStatement)
import HOI4.Handlers.Generic (parseTA, parseTV, TextAtom (..), textValue, TextValue (..), withFlag, withState)
import HOI4.Handlers.Modifiers (modifierMSG)

-- | The game keeps two words for every ideology: the one its party and the
-- people in it go by -- Fascist, Communist -- and the one the belief itself
-- goes by -- Fascism, Communism. Script writes neither, only the word it files
-- the ideology under, so which of the two a line wants has to be said here.
--
-- This is the word for the belief, which is the wiki's own key for the icon
-- rather than anything the game says: a capital on what script wrote, except
-- where the wiki knows the ideology by another name altogether.
beliefTerm :: Text -> Text
beliefTerm atom
    | termed /= atom = termed
    | otherwise = case T.uncons atom of
        Just (c, rest) -> T.cons (toUpper c) rest
        Nothing -> atom
    where termed = iconTerm atom

-- | The wiki's icon for an ideology, named as the belief: what a change in
-- popularity is about is the ideology, not the people who hold it.
beliefIcon :: Text -> Text
beliefIcon = iconText . beliefTerm

-- | The wiki's icon for an ideology, named as the party: the word that belongs
-- in "the ... party", which is the one the game itself localizes the ideology
-- to. An atom that names no ideology at all can stand where one does -- a
-- pronoun for whichever party rules, a country whose government is being
-- matched -- and is left to the icon template as it is written.
partyIconOf :: (HOI4Info g, Monad m) => Text -> PPT g m Text
partyIconOf atom = do
    subideos <- getIdeology
    if atom `elem` HM.elems subideos
        then iconText <$> getGameL10n atom
        else return (iconText atom)

-- | Icon and localization for an ideology whose popularity is changing. The
-- wiki writes the ideology as its icon with the name after it. A scope pronoun
-- in the ideology's place means whichever party rules the country in question,
-- which has no one icon to show, so none is given and the message says so in
-- words.
ideologyIconLoc :: (HOI4Info g, Monad m) => Text -> PPT g m (Text,Text)
ideologyIconLoc atom = do
    (_, what) <- tryLocAndIcon atom
    return (if isPronoun atom then mempty else beliefIcon atom, what)

-- | As 'ideologyIconLoc', for a line about the party rather than the belief.
partyIconLoc :: (HOI4Info g, Monad m) => Text -> PPT g m (Text,Text)
partyIconLoc atom = do
    (_, what) <- tryLocAndIcon atom
    ico <- partyIconOf atom
    return (if isPronoun atom then mempty else ico, what)

-- | Handler for a statement whose RHS names the ideology of a party.
withPartyIcon :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
-- A variable holding the ideology names no one party, so there is no icon to
-- show for it and the message says so in words.
withPartyIcon msg stmt@[pdx| %_ = $vartag:$var |] = do
    mtagloc <- tagged vartag var
    maybe (preStatement stmt) (msgToPP . msg "") mtagloc
withPartyIcon msg [pdx| %_ = ?key |] = do
    what <- Doc.doc2text <$> allowPronoun Nothing (fmap Doc.strictText . getGameL10n) key
    ico <- partyIconOf key
    msgToPP $ msg ico what
withPartyIcon _ stmt = preStatement stmt

------------------------------
-- Handler for set_autonomy --
------------------------------
data SetAutonomy = SetAutonomy
        {   sa_target :: Maybe Text
        ,   sa_autonomy_state :: Maybe Text
        ,   sa_freedom_level :: Maybe Double
        ,   sa_end_wars :: Bool
        ,   sa_end_civil_wars :: Bool
        }

newSA :: SetAutonomy
newSA = SetAutonomy Nothing Nothing Nothing True True

setAutonomy :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> Double -> Text -> ScriptMessage) -> StatementHandler g m
setAutonomy msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_sa =<< foldM addLine newSA scr
    where
        addLine :: SetAutonomy -> GenericStatement -> PPT g m SetAutonomy
        addLine sa [pdx| target = $vartag:$var |] = do
            flagd <- eflag (Just HOI4Country) (Right (vartag,var))
            return sa { sa_target = flagd }
        addLine sa [pdx| target = ?txt |] = do
            flagd <- eflag (Just HOI4Country) (Left txt)
            return sa { sa_target = flagd }
        addLine sa [pdx| autonomy_state = $txt |] =
            return sa { sa_autonomy_state = Just txt }
        addLine sa [pdx| autonomous_state = $txt |] =
            return sa { sa_autonomy_state = Just txt }
        addLine sa [pdx| freedom_level = !amt |] =
            return sa { sa_freedom_level = Just (amt :: Double) }
        addLine sa [pdx| end_wars = $yn |] =
            return sa { sa_end_wars = False }
        addLine sa [pdx| end_civil_wars = $yn |] =
            return sa { sa_end_civil_wars = False }
        addLine sa [pdx| release_non_owned_controlled = %_|] =
            return sa
        -- Book-keeping for which states change controller along with the
        -- autonomy change; nothing the reader would see.
        addLine sa [pdx| force_change_controller_for_non_ally_controlled = %_|] =
            return sa
        addLine sa stmt
            = warn (UnknownSection "set_autonomy" stmt) $ return sa
        pp_sa sa = do
            let endwar = case (sa_end_wars sa, sa_end_civil_wars sa) of
                    (True, True) -> T.pack " and end wars and civil wars for subject"
                    (True, False) -> T.pack " and end wars for subject"
                    (False, True) -> T.pack " and end civil wars for subject"
                    _ -> ""
                freedom = fromMaybe 0 (sa_freedom_level sa)
                autonomy_state = fromMaybe "<!-- Check Script -->" (sa_autonomy_state sa)
                target = fromMaybe "<!-- Check Script -->" (sa_target sa)
            autonomy <- getGameL10n autonomy_state
            return $ msg target (iconText autonomy) autonomy freedom endwar
setAutonomy _ stmt = preStatement stmt

------------------------------
-- Handler for set_politics --
------------------------------
data SetPolitics = SetPolitics
        {   sp_ruling_party :: Text
        ,   sp_ruling_partyvar :: Maybe Text
        ,   sp_elections_allowed :: Maybe Text
        ,   sp_last_election :: Maybe Text
        ,   sp_election_frequency :: Maybe Double
        ,   sp_election_frequencyvar :: Maybe Text
        ,   sp_long_name :: Maybe Text
        ,   sp_name :: Maybe Text
        }

newSP :: SetPolitics
newSP = SetPolitics "<!-- Check Script -->" Nothing Nothing Nothing Nothing Nothing Nothing Nothing

setPolitics :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setPolitics stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_sp =<< foldM addLine newSP scr
    where
        addLine :: SetPolitics -> GenericStatement -> PPT g m SetPolitics
        addLine sp [pdx| ruling_party = $txt |] = return sp { sp_ruling_party = txt }
        -- The party is picked at run time from a variable holding an ideology
        -- group; there is no name to localize, only the variable to show.
        addLine sp [pdx| ruling_party = $vartag:$var |] =
            return sp { sp_ruling_partyvar = Just (vartag <> ":" <> var) }
        addLine sp [pdx| elections_allowed = %_ |] = return sp
        addLine sp [pdx| last_election = %_ |] = return sp
        addLine sp [pdx| election_frequency = $txt |] =
            return sp { sp_election_frequencyvar = Just txt }
        addLine sp [pdx| election_frequency = !amt |] =
            return sp { sp_election_frequency = Just (amt :: Double) }
        addLine sp [pdx| long_name = $yn |] = return sp
        addLine sp [pdx| name = $yn |] = return sp
        addLine sp stmt
            = warn (UnknownSection "set_politics" stmt) $ return sp
        pp_sp sp = do
            let freq = fromMaybe 0 (sp_election_frequency sp)
            (partyicon, party) <- case sp_ruling_partyvar sp of
                Just pv -> return ("", typewriterText pv)
                Nothing -> do
                    party <- getGameL10n (sp_ruling_party sp)
                    return (iconText party, party)
            case sp_election_frequencyvar sp of
                Just freqvar -> return $ MsgSetPoliticsVar partyicon party freqvar
                _ -> return $ MsgSetPolitics partyicon party freq
setPolitics stmt = preStatement stmt

------------------------------------
-- Handler for has_country_leader --
------------------------------------
foldCompound "hasCountryLeader" "HasLeader" "hcl"
    []
    [CompField "character" [t|Text|] Nothing False
    ,CompField "ruling_only" [t|Text|] Nothing False
    ,CompField "name" [t|Text|] Nothing False
    ,CompField "id" [t|Double|] Nothing False
    ]
    [|  do
        let charjust = case (_character, _name, _id) of
                (Just character, _, _) -> character
                (_, Just name, _) -> name
                (_, _, Just id)-> T.pack $ show $ floor id
                _ -> "<!-- Check Script -->"
        charloc <- getGameL10n charjust
        return $ MsgHasCountryLeader charloc
    |]

------------------------------------
-- Handler for has_country_leader --
------------------------------------
foldCompound "setPartyName" "SetPartyName" "spn"
    []
    [CompField "ideology" [t|Text|] Nothing True
    ,CompField "long_name" [t|Text|] Nothing False
    ,CompField "name" [t|Text|] Nothing True
    ]
    [|  do
        let long_name = fromMaybe "" _long_name
        long_loc <- getGameL10n long_name
        short_loc <- getGameL10n _name
        ideoicon <- partyIconOf _ideology
        return $ MsgSetPartyName ideoicon short_loc long_loc
    |]

startCivilWar :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
startCivilWar stmt@[pdx| %_ = @scr |] = do
    let (_ideology, _) = extractStmt (matchLhsText "ideology") scr
        (_size, _) = extractStmt (matchLhsText "size") scr
    size <- case _size of
        Just [pdx| %_ = !num |] -> return $ templateColor'(reducedNum (colourPc False) num)
        Just [pdx| %_ = ?var |] -> return var
        _ -> return "<!-- Check Script -->"
    ideology <- case _ideology of
        Just [pdx| %_ = $txt |] -> getGameL10n txt
        _ -> return "<!-- Check Script -->"
    msgToPP $ MsgStartCivilWar ideology size
startCivilWar stmt = preStatement stmt

-- | Handler for set_rule
setRule :: forall g m. (HOI4Info g, Monad m) =>
    (Double -> ScriptMessage) -- ^ Message to use as the block header
    -> StatementHandler g m
setRule header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        -- @desc@ names a piece of text the game shows in place of the wording a
        -- rule would otherwise be given. It is not a rule and is not one of the
        -- ones counted.
        let (_, rules) = extractStmt (matchLhsText "desc") scr
        rules_pp'd <- ppRules rules
        let numrules = fromIntegral $ length rules
        return ((i, header numrules) : rules_pp'd)
    where
        ppRules :: GenericScript -> PPT g m IndentedMessages
        ppRules scr = indentUp (concat <$> mapM ppRule scr)
        ppRule :: StatementHandler g m
        ppRule stmt@[pdx| $lhs = ?yn |] =
            case yn of
                "yes" -> do
                    let lhst = T.toUpper lhs
                    loc <- getGameL10n lhst
                    msgToPP $ MsgSetRuleYesNo "{{icon|yes}}" loc
                "no" -> do
                    let lhst = T.toUpper lhs
                    loc <- getGameL10n lhst
                    msgToPP $ MsgSetRuleYesNo "{{icon|no}}" loc
                _ -> preStatement stmt
        ppRule stmt = warn (UnknownSection "set_rule" stmt) (preStatement stmt)
setRule _ stmt = preStatement stmt

addAutonomyRatio :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Double -> ScriptMessage) -> (Text -> Text -> Text -> ScriptMessage) -> StatementHandler g m
addAutonomyRatio valmsg varmsg stmt@[pdx| %_ = @scr |] = case length scr of
    2 -> textValue "localization" "value" valmsg varmsg tryLocMaybe stmt
    1 -> do
        let (_value, _) = extractStmt (matchLhsText "value") scr
        case _value of
            Just [pdx| %_ = !num |] -> msgToPP $ valmsg "" "" num
            Just [pdx| %_ = $var |] -> msgToPP $ varmsg "" "" var
            _ -> preStatement stmt
    _ -> preStatement stmt
addAutonomyRatio _ _ stmt = preStatement stmt

setCapital :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
setCapital msg stmt@[pdx| %_ = @scr |] =
        let (_, rest) = extractStmt (matchLhsText "remember_old_capital") scr in
        case rest of
            [[pdx| state = !state |]] -> do
                stateloc <- getStateLoc state
                msgToPP $ msg stateloc ""
            [[pdx| state = $state |]] -> do
                stateloc <- eGetStateText (Left state)
                msgToPP $ msg stateloc ""
            [[pdx| state = $vartag:$var |]] -> do
                stateloc <- eGetStateText (Right (vartag, var))
                msgToPP $ msg stateloc ""
            _ -> preStatement stmt
setCapital msg stmt = withFlag msg stmt

setPopularities :: (HOI4Info g, Monad m) => StatementHandler g m
setPopularities [pdx| %_ = @scr |] = do
    basemsg <- msgToPP MsgSetPopularities
    popmsg <- fold <$> indentUp (traverse getpops scr)
    return $ basemsg ++ popmsg
    where
        getpops stmt@[pdx| $ideo = !num |] = do
            ideoloc <- getGameL10n ideo
            msgToPP $ MsgSetPopularity (beliefIcon ideo) ideoloc num
        getpops stmt@[pdx| $ideo = ?txt |] = do
            ideoloc <- getGameL10n ideo
            msgToPP $ MsgSetPopularityVar (beliefIcon ideo) ideoloc txt
        getpops stmt = preStatement stmt
setPopularities stmt = preStatement stmt

--------------------------------------------
-- handler for force_enable_resistance    --
--------------------------------------------

-- | Turns resistance on in a state whatever the game would otherwise make of
-- it. Whoever the occupier has to be for that is the only part of it a reader
-- needs; @clear@ only says whether what was set before is thrown away first.
forceEnableResistance :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
forceEnableResistance msg [pdx| %_ = @scr |] = do
    let (moccupier, _) = extractStmt (matchLhsText "occupier") scr
    whoflag <- case moccupier of
        Just [pdx| %_ = ?who |] -> flagText (Just HOI4Country) who
        _ -> return ""
    msgToPP $ msg whoflag ""
forceEnableResistance msg [pdx| %_ = ?who |]
    | T.toLower who `notElem` ["yes", "no"] = do
        whoflag <- flagText (Just HOI4Country) who
        msgToPP $ msg whoflag who
forceEnableResistance _ stmt = preStatement stmt

-------------------------------------------
-- handler for is_power_balance_in_range --
-------------------------------------------

data PowerBalanceRange = PowerBalanceRange
        {   pbr_id :: Text
        ,   pbr_range :: Text
        ,   pbr_comp :: Double
        }

newPBR :: PowerBalanceRange
newPBR = PowerBalanceRange "<!--Check Script-->" "<!--Check Script-->" 0

powerBalanceRange :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
powerBalanceRange stmt@[pdx| %_ = @scr |]
    = msgToPP =<< ppPBR =<< foldM addLine newPBR scr
    where
        addLine :: PowerBalanceRange -> GenericStatement -> PPT g m PowerBalanceRange
        addLine pbr [pdx| id = ?txt |] = return pbr { pbr_id = txt }
        addLine pbr [pdx| range = ?txt |] = return pbr { pbr_range = txt }
        addLine pbr [pdx| range > ?txt |] = return pbr { pbr_range = txt, pbr_comp = 1 }--right
        addLine pbr [pdx| range < ?txt |] = return pbr { pbr_range = txt, pbr_comp = -1 }--left
        addLine pbr stmt = warn (UnknownSection "powerBalanceRange" stmt) $ return pbr
        ppPBR :: PowerBalanceRange -> PPT g m ScriptMessage
        ppPBR pbr = do
            idloc <- getGameL10n (pbr_id pbr)
            rangeloc <- getGameL10n (pbr_range pbr)
            return $ MsgIsPowerBalanceInRange idloc rangeloc (pbr_id pbr) (pbr_range pbr) (pbr_comp pbr)
powerBalanceRange stmt = preStatement stmt

---------------------------------------
-- Handler for compare_autonomy_state --
---------------------------------------
-- | Handler for @compare_autonomy_state@, which ranks the subject's autonomy
-- level against a named one. It is written with a @>@ or a @<@ rather than an
-- @=@, so it reads as a comparison and not as a check for that level itself.
compareAutonomyState :: (HOI4Info g, Monad m) => StatementHandler g m
compareAutonomyState stmt = case stmt of
    [pdx| %_ > $lvl |] -> withLevel "more autonomous than" lvl
    [pdx| %_ < $lvl |] -> withLevel "less autonomous than" lvl
    [pdx| %_ = $lvl |] -> withLevel "at least as autonomous as" lvl
    _ -> preStatement stmt
    where
        withLevel comp lvl = do
            loc <- getGameL10n lvl
            msgToPP (MsgCompareAutonomyState comp loc)

------------------------------------
-- Handler for set_power_balance --
------------------------------------
-- | Handler for @set_power_balance@, which starts a balance of power or moves
-- one already running. Only the fields it is given take effect; whatever it
-- leaves out is left as it stands.
setPowerBalance :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setPowerBalance stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing, Nothing, False) scr of
        (Just bopid, mvalue, mleft, mright, isdefault) -> do
            boploc <- getGameL10n bopid
            leftloc <- traverse getGameL10n mleft
            rightloc <- traverse getGameL10n mright
            basemsg <- msgToPP (MsgSetPowerBalance boploc bopid)
            valmsg <- case mvalue of
                Just val -> indentUp (msgToPP (MsgSetPowerBalanceValue val))
                Nothing -> return []
            sidemsg <- case (leftloc, rightloc) of
                (Nothing, Nothing) -> return []
                _ -> indentUp (msgToPP (MsgSetPowerBalanceSides
                        (fromMaybe "" leftloc) (fromMaybe "" rightloc)))
            defmsg <- if isdefault
                then indentUp (msgToPP MsgSetPowerBalanceDefault)
                else return []
            return (basemsg ++ valmsg ++ sidemsg ++ defmsg)
        _ -> preStatement stmt
    where
        addLine (i, v, l, r, d) [pdx| id = $bopid |] = (Just bopid, v, l, r, d)
        addLine (i, v, l, r, d) [pdx| set_value = !n |] = (i, Just n, l, r, d)
        addLine (i, v, l, r, d) [pdx| left_side = $s |] = (i, v, Just s, r, d)
        addLine (i, v, l, r, d) [pdx| right_side = $s |] = (i, v, l, Just s, d)
        addLine (i, v, l, r, d) [pdx| set_default = yes |] = (i, v, l, r, True)
        addLine acc [pdx| set_default = no |] = acc
        addLine acc stmt = warn (UnknownSection "set_power_balance" stmt) acc
setPowerBalance stmt = preStatement stmt

-------------------------------------------
-- Handler for get_highest_scored_country --
-------------------------------------------
-- | Handler for @get_highest_scored_country@, which runs a scorer over every
-- country and saves the winner in a variable for the script to read back.
getHighestScoredCountry :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
getHighestScoredCountry stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (Just scorer, mvar) ->
            msgToPP (MsgGetHighestScoredCountry scorer
                        (fromMaybe "highest_scored_country" mvar))
        _ -> preStatement stmt
    where
        addLine (s, v) [pdx| scorer = $sc |] = (Just sc, v)
        addLine (s, v) [pdx| var = $vr |] = (s, Just vr)
        addLine (s, v) [pdx| var = ?vr |] = (s, Just vr)
        addLine acc stmt = warn (UnknownSection "get_highest_scored_country" stmt) acc
getHighestScoredCountry stmt = preStatement stmt

------------------------------------
-- Handler for add_contested_owner --
------------------------------------
-- | Handler for @add_contested_owner@, which puts a second claimant on a state.
-- It is written from either side, so the one name it takes is a country in a
-- state scope and a state in a country scope.
addContestedOwner :: (HOI4Info g, Monad m) => StatementHandler g m
addContestedOwner stmt@[pdx| %_ = !(_ :: Double) |] = withState MsgAddContestedOwnerState stmt
addContestedOwner stmt = withFlag MsgAddContestedOwner stmt

-------------------------------------
-- Handler for add_resistance_target --
-------------------------------------
-- | Handler for @add_resistance_target@, which moves the resistance a state
-- settles at. The tooltip names the reason the game gives the player for it.
addResistanceTarget :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addResistanceTarget stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing) scr of
        (Just amount, mtip, mdays) -> do
            tiploc <- traverse getGameL10n mtip
            msgToPP (MsgAddResistanceTarget amount (fromMaybe 0 mdays) (fromMaybe "" tiploc))
        _ -> preStatement stmt
    where
        addLine (a, t, d) [pdx| amount = !n |] = (Just n, t, d)
        addLine (a, t, d) [pdx| tooltip = $tip |] = (a, Just tip, d)
        addLine (a, t, d) [pdx| days = !n |] = (a, t, Just n)
        -- Whose occupation the target applies to. The state is in scope either
        -- way, so this narrows which occupier rather than saying anything new.
        addLine acc [pdx| occupier = %_ |] = acc
        addLine acc [pdx| occupied = %_ |] = acc
        addLine acc stmt = warn (UnknownSection "add_resistance_target" stmt) acc
addResistanceTarget stmt@[pdx| %_ = !amount |] = msgToPP (MsgAddResistanceTarget amount 0 "")
addResistanceTarget stmt = preStatement stmt

--------------------------------------------
-- Handler for add_power_balance_modifier --
--------------------------------------------

addPowerBalanceModifier :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addPowerBalanceModifier stmt@[pdx| %_ = @scr |] =
    pp_ta (parseTA "id" "modifier" scr)
    where
        pp_ta :: TextAtom -> PPT g m IndentedMessages
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just idpob, Just modi) -> do
                mmod <- HM.lookup modi <$> getModifiers
                midpob_loc <- getGameL10nIfPresent idpob
                let idpob_loc = fromMaybe (typewriterText idpob) midpob_loc
                case mmod of
                    Just mod -> withCurrentIndent $ \i -> do
                        effect <- fold <$> indentUp (traverse (modifierMSG False "") (modEffects mod))
                        let name = modLocName mod
                            locName = maybe (typewriterText modi) (Doc.doc2text . iquotes) name
                        return ((i, MsgAddPowerBalanceModifier idpob_loc idpob locName modi) : effect)
                    _ -> trace ("add_power_balance_modifier: Modifier " ++ T.unpack modi ++ " not found") $ preStatement stmt
            _-> preStatement stmt
addPowerBalanceModifier stmt = warn (UnknownSection "add_power_balance_modifier" stmt) $ preStatement stmt

-----------------------------------------
-- Handler for add_power_balance_value --
-----------------------------------------

-- | Handler for @add_power_balance_value@, which moves a balance of power. The
-- sign of the value says which way it moves; @tooltip_side@ names the side the
-- game credits the move to in its tooltip, so it is named in the message too
-- where script gives it.
addPowerBalanceValue :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addPowerBalanceValue stmt@[pdx| %_ = @scr |] = do
    let (mside, rest) = extractStmt (matchLhsText "tooltip_side") scr
        tv = parseTV "id" "value" rest
    msideloc <- case mside of
        Just [pdx| %_ = $side |] -> Just <$> getGameL10n side
        _ -> return Nothing
    case (tv_what tv, tv_value tv, tv_var tv) of
        (Just what, Just value, _) -> do
            wloc <- getGameL10n what
            msgToPP $ case msideloc of
                Just side -> MsgAddPowerBalanceValueSide wloc what side value
                Nothing -> MsgAddPowerBalanceValue wloc what value
        (Just what, _, Just var) -> do
            wloc <- getGameL10n what
            msgToPP $ MsgAddPowerBalanceValueVar wloc what var
        _ -> preStatement stmt
addPowerBalanceValue stmt = preStatement stmt
