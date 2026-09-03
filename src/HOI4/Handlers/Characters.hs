{-|
Module      : HOI4.Handlers.Characters
Description : Characters and their traits

Handlers for characters' roles as leaders, advisors, generals and
admirals, their names, and the traits they carry.
-}
module HOI4.Handlers.Characters (
        setNationality
    ,   addRandomTrait
    ,   setCountryLeaderName
    ,   generateScientistCharacter
    ,   addFieldMarshalRole
    ,   setCharacterName
    ,   removeAdvisorRole
    ,   withCharacter
    ,   addAdvisorRole
    ,   addLeaderRole
    ,   createLeader
    ,   promoteCharacter
    ,   setCanBeFiredInAdvisorRole
    ,   handleTrait
    ,   addRemoveLeaderTrait
    ,   addRemoveUnitTrait
    ,   addTimedTrait
    ,   swapLeaderTrait
    ,   getLeaderTraits
    ,   getUnitTraits
    ,   showUnitLeader
    ,   characterListTooltip
    ,   removeCountryLeaderRole
    ,   canBeCountryLeader
    ) where

import Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (italicText, typewriterText)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, concatMapM, getCurrentCharacter)
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (bareAtom, getbaretraits, limitClause, msgToPP, plainMsg', preStatement)
import HOI4.Handlers.Generic (parseTA, taTypeFlag, textAtom, TextAtom (..), withBool, withFlag, withLocAtom, withLookupAtom)
import HOI4.Handlers.Politics (partyIconOf)
import HOI4.Handlers.Modifiers (handleEquipmentBonus, handleModifier, handleResearchBonus, handleTargetedModifier, modifierMSG, sortmods)

---------------------------------
-- Handler for set_nationality --
---------------------------------

-- | Handler for @set_nationality@, which moves a character to another country.
-- Script writes the country on its own where the character is the scope the
-- effect stands in, and names both where it is not -- and it writes the block
-- form with only the country in it as well, which says the same as the bare
-- one.
setNationality :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setNationality stmt@[pdx| %_ = @scr |] =
    case extractStmt (matchLhsText "target_country") scr of
        (Just target, rest) | not (any (matchLhsText "character") rest) ->
            withFlag MsgSetNationality target
        _ -> taTypeFlag "character" "target_country" MsgSetNationalityChar stmt
-- Whatever else names the country -- a tag, a pronoun, a variable holding one.
setNationality stmt = withFlag MsgSetNationality stmt

-----------------------------------
-- handler for add_random_trait  --
-----------------------------------

addRandomTrait :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addRandomTrait stmt@[pdx| %_ = @scr |] = do
    locs <- traverse getGameL10n (mapMaybe bareAtom scr)
    if null locs
        then preStatement stmt
        else msgToPP $ MsgAddRandomTrait (T.intercalate ", " (map italicText locs))
addRandomTrait stmt = preStatement stmt

----------------------------------------
-- handler for set_country_leader_name --
----------------------------------------

-- | Renames a country's leader. With no party named it is whoever leads the
-- country now.
setCountryLeaderName :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setCountryLeaderName stmt@[pdx| %_ = @scr |] = do
    let (mname, rest) = extractStmt (matchLhsText "name") scr
        (mideo, _) = extractStmt (matchLhsText "ideology") rest
    ideoloc <- case mideo of
        Just [pdx| %_ = ?ideo |] -> getGameL10n ideo
        _ -> return ""
    case mname of
        Just [pdx| %_ = ?name |] -> do
            nameloc <- getGameL10n name
            msgToPP $ MsgSetCountryLeaderName nameloc ideoloc
        _ -> preStatement stmt
setCountryLeaderName stmt = preStatement stmt

------------------------------------------------
-- handler for generate_scientist_character   --
------------------------------------------------

-- | Hires a scientist the game makes up on the spot. What they are good at is
-- the whole of what the country gains; the portrait and the gender they are
-- drawn with are not.
generateScientistCharacter :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
generateScientistCharacter [pdx| %_ = @scr |] = do
    let (mtraits, rest) = extractStmt (matchLhsText "traits") scr
        (mskills, _) = extractStmt (matchLhsText "skills") rest
    traitlocs <- case mtraits of
        Just [pdx| %_ = @trts |] -> traverse getGameL10n (mapMaybe bareAtom trts)
        _ -> return []
    header <- msgToPP $ MsgGenerateScientistCharacter
        (T.intercalate ", " (map italicText traitlocs))
    skillmsgs <- case mskills of
        Just [pdx| %_ = @skls |] -> indentUp (concat <$> traverse skillMsg skls)
        _ -> return []
    return $ header ++ skillmsgs
    where
        skillMsg :: GenericStatement -> PPT g m IndentedMessages
        skillMsg [pdx| $spec = !lvl |] = do
            specloc <- getGameL10n spec
            msgToPP $ MsgScientistSkill specloc lvl
        skillMsg stmt = preStatement stmt
generateScientistCharacter stmt = preStatement stmt

----------------
-- characters --
----------------

addFieldMarshalRole :: (Monad m, HOI4Info g) => (Text -> ScriptMessage) -> StatementHandler g m
addFieldMarshalRole msg stmt@[pdx| %_ = @scr |] = do
        let (name, _) = extractStmt (matchLhsText "character") scr
        nameloc <- case name of
            Just [pdx| character = ?id |] -> getCharacterName id
            _ -> case extractStmt (matchLhsText "name") scr of
                (Just [pdx| name = ?id |],_) -> getCharacterName id
                _-> return ""
        msgToPP $ msg nameloc
addFieldMarshalRole _ stmt = preStatement stmt

setCharacterName :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setCharacterName stmt@[pdx| %_ = ?txt |] = withLocAtom MsgSetCharacterName stmt
setCharacterName stmt@[pdx| %_ = @scr |] = case scr of
    [[pdx| $who = $name |]] -> do
        whochar <- getCharacterName who
        nameloc <- getGameL10n name
        msgToPP $ MsgSetCharacterNameType whochar nameloc
    _ -> preStatement stmt
setCharacterName stmt = preStatement stmt

removeAdvisorRole :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
removeAdvisorRole stmt@[pdx| %_ = @scr |] =
    if length scr == 2
    then textAtom "character" "slot" MsgRemoveAdvisorRole getGameL10nIfPresent stmt
    else do
        let (mslot,_) = extractStmt (matchLhsText "slot") scr
        slot <- case mslot of
            Just [pdx| %_ = $slottype |] -> getGameL10n slottype
            _-> return "<!-- Check Script -->"
        msgToPP $ MsgRemoveAdvisorRole "" "" slot
removeAdvisorRole stmt = preStatement stmt

withCharacter :: (HOI4Info g, Monad m) => (Text -> ScriptMessage) -> StatementHandler g m
withCharacter = withLookupAtom getCharacterName

addAdvisorRole :: (Monad m, HOI4Info g) => StatementHandler g m
addAdvisorRole stmt@[pdx| %_ = @scr |] = do
        let (name, rest) = extractStmt (matchLhsText "character") scr
            (advisor, rest') = extractStmt (matchLhsText "advisor") rest
            (activate, _) = extractStmt (matchLhsText "activate") rest'
        activate <- maybe (return False) (\case
            [pdx| %_ = yes |] -> return True
            _-> return False) activate
        nameloc <- case name of
            Just [pdx| character = $id |] -> getCharacterName id
            _ -> return ""
        case advisor of
            Just advisorj -> do
                (slotloc, traitmsg) <- parseAdvisor advisorj
                basemsg <- msgToPP $ MsgAddAdvisorRole nameloc slotloc
                (if activate
                then do
                    hiremsg <- msgToPP MsgAndIsHired
                    return $ basemsg ++ traitmsg ++ hiremsg
                else return $ basemsg ++ traitmsg)
            _-> preStatement stmt
addAdvisorRole stmt = preStatement stmt

parseAdvisor :: (Monad m, HOI4Info g) =>
    GenericStatement -> PPT g m (Text, [IndentedMessage])
parseAdvisor stmt@[pdx| %_ = @scr |] = do
    let (slot, rest) = extractStmt (matchLhsText "slot") scr
        (traits, modrest) = extractStmt (matchLhsText "traits") rest
        (modifier, bonusrest) = extractStmt (matchLhsText "modifier") modrest
        (resbonus, _) = extractStmt (matchLhsText "research_bonus") bonusrest
    modmsg <- maybe (return []) (indentUp . handleModifier) modifier
    resmsg <- maybe (return []) (indentUp . handleResearchBonus) resbonus
    traitmsg <- case traits of
        Just [pdx| %_ = @arr |] -> do
            let traitbare = mapMaybe getbaretraits arr
            concatMapM (indentUp . getLeaderTraits) traitbare
        _-> return []
    slotloc <- maybe (return "") (\case
        [pdx| %_ = $slottype|] -> getGameL10n slottype
        _->return "<!-- Check Script -->") slot

    return (slotloc, traitmsg ++ modmsg ++ resmsg)
parseAdvisor stmt = return ("<!-- Check Script -->", [])

-- | The party a character is written to lead or belong to, as the wiki's icon
-- for it. Script writes the sub-ideology a leader is filed under -- despotism,
-- leninism -- and the party is named for the ideology that belongs to.
partyIcon :: (Monad m, HOI4Info g) => Text -> PPT g m Text
partyIcon subideo = do
    subideos <- getIdeology
    maybe (return "<!-- Check Script -->") partyIconOf (HM.lookup subideo subideos)

-- | The party the character the script has scoped to is written to lead, or
-- nothing where the script is not about a character or their entry names no
-- party.
scopeParty :: (Monad m, HOI4Info g) => PPT g m (Maybe Text)
scopeParty = do
    chas <- getCharacters
    inscope <- getCurrentCharacter
    let msub = cha_leader_ideology =<< (flip HM.lookup chas =<< inscope)
    traverse partyIcon msub

addLeaderRole :: (Monad m, HOI4Info g) => StatementHandler g m
addLeaderRole stmt@[pdx| %_ = @scr |] = do
        let (name, rest) = extractStmt (matchLhsText "character") scr
            (leader, rest') = extractStmt (matchLhsText "country_leader") rest
            (promote, _) = extractStmt (matchLhsText "promote_leader") rest'
        promoted <- maybe (return False) (\case
            [pdx| %_ = yes |] -> return True
            _-> return False) promote
        nameloc <- case name of
            Just [pdx| character = $id |] -> getCharacterName id
            _ -> return ""
        case leader of
            Just leaderj -> do
                (ideoloc, traitmsg) <- parseLeader leaderj
                basemsg <- if promoted
                    then msgToPP $ MsgAddCountryLeaderRolePromoted nameloc ideoloc
                    else msgToPP $ MsgAddCountryLeaderRole nameloc ideoloc
                return $ basemsg ++ traitmsg
            _-> preStatement stmt
addLeaderRole stmt = preStatement stmt

parseLeader :: (Monad m, HOI4Info g) =>
    GenericStatement -> PPT g m (Text, [IndentedMessage])
parseLeader stmt@[pdx| %_ = @scr |] = do
    let (ideo, rest) = extractStmt (matchLhsText "ideology") scr
        (traits, _) = extractStmt (matchLhsText "traits") rest
    traitmsg <- case traits of
        Just [pdx| %_ = @arr |] -> do
            let traitbare = mapMaybe getbaretraits arr
            concatMapM ppHt traitbare
        _-> return []
    ideoloc <- maybe (return "") (\case
        [pdx| %_ = $ideotype|] -> partyIcon ideotype
        _->return "<!-- Check Script -->") ideo
    return (ideoloc, traitmsg)
parseLeader stmt = return ("<!-- Check Script -->", [])

createLeader :: (Monad m, HOI4Info g) => StatementHandler g m
createLeader stmt@[pdx| %_ = @scr |] = do
        let (name, _) = extractStmt (matchLhsText "name") scr
        nameloc <- case name of
            Just [pdx| %_ = ?id |] -> getCharacterName id
            _ -> return ""
        (ideoloc, traitmsg) <- parseLeader stmt
        basemsg <- msgToPP $ MsgAddCountryLeaderRole nameloc ideoloc
        return $ basemsg ++ traitmsg
createLeader stmt = preStatement stmt

promoteCharacter :: (Monad m, HOI4Info g) => StatementHandler g m
promoteCharacter stmt@[pdx| %_ = @scr |] =
    ppPC (parseTA "character" "ideology" scr)
    where
        ppPC ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> promomessage what atom stmt
            (_, Just atom) -> promomessage "" atom stmt
            _ -> preStatement stmt
promoteCharacter stmt@[pdx| %_ = $txt |]
    -- The character is whoever the script has scoped to, who the scope names
    -- already, so the line says only what becomes of them.
    | txt == "yes" = do
        mparty <- scopeParty
        msgToPP $ maybe (MsgPromoteCharacter "") (MsgAddCountryLeaderRolePromoted "") mparty
    | otherwise = do
        chas <- getCharacters
        subideos <- getIdeology
        case HM.lookup txt subideos of
            Just ideo -> promomessage "" txt stmt
            _-> case HM.lookup txt chas of
                Just ccha -> promomessage txt "" stmt
                _-> preStatement stmt
promoteCharacter stmt = preStatement stmt

promomessage :: (Monad m, HOI4Info g) => Text
    -> Text-> StatementHandler g m
promomessage what atom stmt = do
    chas <- getCharacters
    let mcha = HM.lookup what chas
    -- The party the character comes to lead: the one the statement names, or
    -- failing that the one their own entry writes them for.
    party <- case (atom, cha_leader_ideology =<< mcha) of
        (a, _) | not (T.null a) -> partyIcon a
        (_, Just own) -> partyIcon own
        _ -> return ""
    case mcha of
        Just ccha -> do
            let nameloc = cha_loc_name ccha
            traitmsg <- case cha_leader_traits ccha of
                Just trts -> do
                    concatMapM ppHt trts
                _-> return []
            basemsg <- if T.null party
                then msgToPP $ MsgPromoteCharacter nameloc
                else msgToPP $ MsgAddCountryLeaderRolePromoted nameloc party
            return $ basemsg ++ traitmsg
        -- The character may be named through an event target or a variable,
        -- where there is no entry to look their name up in; the name the script
        -- itself uses is then all there is to call them by.
        _-> let nameloc = if T.null what then "" else typewriterText what
            in msgToPP $ if T.null party
                then MsgPromoteCharacter nameloc
                else MsgAddCountryLeaderRolePromoted nameloc party

ppHt :: (Monad m, HOI4Info g) => Text -> PPT g m IndentedMessages
ppHt trait = do
    traitloc <- Doc.oneLine <$> getGameL10n trait
    namemsg <- indentUp $ plainMsg' ("'''" <> traitloc <> "'''")
    traitmsg' <- indentUp $ indentUp $ getLeaderTraits trait
    return $ namemsg : traitmsg'

-------------------------------
-- Handlers for advisor posts --
-------------------------------

-- | Handler for @activate_advisor@ and @deactivate_advisor@, which put someone
-- into one of the country's advisor posts and take them out of it again.
advisorPost :: (HOI4Info g, Monad m) => (Text -> ScriptMessage) -> StatementHandler g m
advisorPost msg [pdx| %_ = $token |] = msgToPP . msg =<< advisorName token
advisorPost _ stmt = preStatement stmt

-- | Handler for @set_can_be_fired_in_advisor_role@, which decides whether the
-- player may dismiss someone from a post they hold. Script may leave the
-- character out, meaning whoever the surrounding scope is about, and may leave
-- the slot out, meaning every post they hold.
setCanBeFiredInAdvisorRole :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setCanBeFiredInAdvisorRole stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing, Nothing) scr of
        (mchar, mslot, Just value) -> do
            who <- maybe (return "") advisorName mchar
            slot <- maybe (return "") getGameL10n mslot
            msgToPP $ MsgSetCanBeFiredInAdvisorRole who slot value
        _ -> preStatement stmt
    where
        addLine (mchar, mslot, value) [pdx| character = $char |] = (Just char, mslot, value)
        addLine (mchar, mslot, value) [pdx| slot = $slot |] = (mchar, Just slot, value)
        addLine (mchar, mslot, value) [pdx| value = yes |] = (mchar, mslot, Just True)
        addLine (mchar, mslot, value) [pdx| value = no |] = (mchar, mslot, Just False)
        addLine acc stmt = warn (UnknownSection "set_can_be_fired_in_advisor_role" stmt) acc
setCanBeFiredInAdvisorRole stmt = preStatement stmt

------------
-- traits --
------------
data HandleTrait = HandleTrait
    { ht_trait :: Text
    , ht_character :: Maybe Text
    , ht_ideology :: Maybe Text
    }

newHT :: HandleTrait
newHT = HandleTrait undefined Nothing Nothing

handleTrait :: forall g m. (HOI4Info g, Monad m) => Bool -> StatementHandler g m
handleTrait addremove stmt@[pdx| %_ = @scr |] =
    pp_ht addremove (foldl' addLine newHT scr)
    where
        addLine ht [pdx| trait = $txt |] = ht { ht_trait = txt }
        addLine ht [pdx| character = $txt |] = ht { ht_character = Just txt }
        addLine ht [pdx| ideology = $txt |] = ht { ht_ideology = Just txt }
        addLine ht [pdx| slot = %_ |] = ht
        addLine ht stmt = warn (UnknownSection "handleTrait" stmt) ht
        pp_ht addremove ht = do
            traitloc <- Doc.oneLine <$> getGameL10n (ht_trait ht)
            traitmsg <- indentUp $ getLeaderTraits (ht_trait ht)
            baseMsg <- case (ht_character ht, ht_ideology ht) of
                (Just char, Just ideo) -> do
                    charloc <- getCharacterName char
                    ideoloc <- getGameL10n ideo
                    msgToPP $ MsgTraitCharIdeo charloc addremove ideoloc traitloc
                (Just char, _) -> do
                    charloc <- getCharacterName char
                    msgToPP $ MsgTraitChar charloc addremove traitloc
                (_, Just ideo) -> do
                    ideoloc <- getGameL10n ideo
                    msgToPP $ MsgTraitIdeo addremove ideoloc traitloc
                _ -> msgToPP $ MsgTrait addremove traitloc
            return $ baseMsg ++ traitmsg
handleTrait _ stmt = preStatement stmt

-- | Handler for adding or removing a trait: the trait's name, with what it
-- grants written out under it by the given lookup.
addRemoveTrait :: (Monad m, HOI4Info g) =>
    (Text -> PPT g m IndentedMessages) -> (Text -> ScriptMessage) -> StatementHandler g m
addRemoveTrait getTraits msg stmt@[pdx| %_ = $trait |] = do
    traitloc <- Doc.oneLine <$> getGameL10n trait
    traitmsg <- indentUp $ getTraits trait
    baseMsg <- msgToPP (msg traitloc)
    return $ baseMsg ++ traitmsg
-- The block form names the trait under @trait@, and may say which of the
-- country's leaders it belongs to under @ideology@. The trait is the part that
-- carries anything to read.
addRemoveTrait getTraits msg stmt@[pdx| %_ = @scr |] =
    case [traitstmt | traitstmt@[pdx| trait = %_ |] <- scr] of
        (traitstmt : _) -> addRemoveTrait getTraits msg traitstmt
        [] -> preStatement stmt
addRemoveTrait _ _ stmt = preStatement stmt

addRemoveLeaderTrait :: (Monad m, HOI4Info g) => (Text -> ScriptMessage) -> StatementHandler g m
addRemoveLeaderTrait = addRemoveTrait getLeaderTraits

addRemoveUnitTrait :: (Monad m, HOI4Info g) => (Text -> ScriptMessage) -> StatementHandler g m
addRemoveUnitTrait = addRemoveTrait getUnitTraits

data AddTimedTrait = AddTimedTrait
    { adt_trait :: Text
    , adt_days :: Maybe Double
    , adt_daysvar :: Maybe Text
    }

newADT :: AddTimedTrait
newADT = AddTimedTrait undefined Nothing Nothing

addTimedTrait ::  (Monad m, HOI4Info g) => GenericStatement -> PPT g m IndentedMessages
addTimedTrait stmt@[pdx| %_ = @scr |] =
    ppADT (foldl' addLine newADT scr)

    where
        addLine adt [pdx| trait = $txt |] = adt { adt_trait = txt }
        addLine adt [pdx| days = !num |] = adt { adt_days = Just num }
        addLine adt [pdx| days = $txt |] = adt { adt_daysvar = Just txt }
        addLine adt stmt = warn (UnknownSection "addTimedTrait" stmt) adt
        ppADT adt = do
            traitloc <- getGameL10n (adt_trait adt)
            traitmsg <- indentUp $ getUnitTraits (adt_trait adt)
            baseMsg <- case (adt_days adt, adt_daysvar adt) of
                (Just days,_)-> msgToPP $ MsgAddTimedUnitLeaderTrait traitloc days
                (_, Just daysvar)->msgToPP $ MsgAddTimedUnitLeaderTraitVar traitloc daysvar
                _-> msgToPP $ MsgAddTimedUnitLeaderTraitVar traitloc "<!-- Check Script -->"
            return $ baseMsg ++ traitmsg
addTimedTrait stmt = preStatement stmt

data SwapTrait = SwapTrait
    { st_add :: Text
    , st_remove :: Text
    }

newST :: SwapTrait
newST = SwapTrait undefined undefined

swapLeaderTrait ::  (Monad m, HOI4Info g) => GenericStatement -> PPT g m IndentedMessages
swapLeaderTrait stmt@[pdx| %_ = @scr |] =
    ppST (foldl' addLine newST scr)

    where
        addLine st [pdx| add = $txt |] = st { st_add = txt }
        addLine st [pdx| remove = $txt |] = st { st_remove = txt }
        addLine st [pdx| ideology = %_ |] = st -- restricts the swap to a leader of this ideology
        addLine st stmt = warn (UnknownSection "swapTrait" stmt) st
        ppST st = do
            traitaddloc <- getGameL10n (st_add st)
            traitremoveloc <- getGameL10n (st_remove st)
            let same = traitaddloc == traitremoveloc
            namemsg <- indentUp $ plainMsg' ("'''" <> traitaddloc <> "'''")
            traitmsg' <- indentUp $ indentUp $ getLeaderTraits (st_add st)
            let traitmsg = namemsg : traitmsg'
            baseMsg <- if same
                then msgToPP MsgModifyCountryLeaderTrait
                else msgToPP $ MsgReplaceCountryLeaderTrait traitremoveloc
            return $ baseMsg ++ traitmsg
swapLeaderTrait stmt = preStatement stmt

getLeaderTraits :: (Monad m, HOI4Info g) => Text -> PPT g m IndentedMessages
getLeaderTraits trait = do
    traits <- getCountryLeaderTraits
    case HM.lookup trait traits of
        Just clt-> do
            mod <- maybe (return []) (\ml -> fmap fold $ traverse (modifierMSG False "") =<< sortmod ml) (clt_modifier clt)
            equipmod <- maybe (return []) handleEquipmentBonus (clt_equipment_bonus clt)
            tarmod <- maybe (return []) (concatMapM handleTargetedModifier) (clt_targeted_modifier clt)
            hidmod <- maybe (return []) handleModifier (clt_hidden_modifier clt)
            return ( mod ++ hidmod ++ tarmod ++ equipmod )
        Nothing -> getUnitTraits trait
    where
        sortmod scr = sortmods scr =<< getModKeys

-- | How much faster the character earns experience towards other traits while
-- they hold this one. What stands on the left of each line is a trait of the
-- game's own rather than a modifier.
traitXpFactor :: forall g m. (Monad m, HOI4Info g) => StatementHandler g m
traitXpFactor [pdx| %_ = @scr |] = concat <$> traverse ppXp scr
    where
        ppXp :: GenericStatement -> PPT g m IndentedMessages
        ppXp [pdx| $trait = !factor |] = do
            traitloc <- getGameL10n trait
            msgToPP $ MsgTraitXpFactor traitloc factor
        ppXp stmt = preStatement stmt
traitXpFactor stmt = preStatement stmt

getUnitTraits :: (Monad m, HOI4Info g) => Text-> PPT g m IndentedMessages
getUnitTraits trait = do
    traits <- getUnitLeaderTraits
    case HM.lookup trait traits of
        Just ult-> do
            attack <- maybe (return []) (msgToPP . MsgAddSkill "Attack") (ult_attack_skill ult)
            defense <- maybe (return []) (msgToPP . MsgAddSkill "Defense") (ult_defense_skill ult)
            planning <- maybe (return []) (msgToPP . MsgAddSkill "Planning") (ult_planning_skill ult)
            logistics <- maybe (return []) (msgToPP . MsgAddSkill "Logistics") (ult_logistics_skill ult)
            maneuvering <- maybe (return []) (msgToPP . MsgAddSkill "Maneuvering") (ult_maneuvering_skill ult)
            coordination <- maybe (return []) (msgToPP . MsgAddSkill "Coordination") (ult_coordination_skill ult)
            let skillmsg = attack ++ defense ++ planning ++ logistics ++ maneuvering ++ coordination
                mod = getscript (ult_modifier ult)
                nsmod = getscript (ult_non_shared_modifier ult)
                ccmod = getscript (ult_corps_commander_modifier ult)
                fmmod = getscript (ult_field_marshal_modifier ult)
            trtxp <- maybe (return []) traitXpFactor (ult_trait_xp_factor ult)
            mods <- do
                let mods' = mod ++ nsmod ++ ccmod ++ fmmod
                keys <- getModKeys
                sm <- sortmods mods' keys
                fold <$> traverse (modifierMSG False "") sm
            sumod <- maybe (return []) handleEquipmentBonus (ult_sub_unit_modifiers ult)

            return (trtxp ++ mods ++ sumod ++ skillmsg)
        Nothing -> return []
    where
        getscript stmt = case stmt of
            Just [pdx| %_ = @scr|] -> scr
            _ -> []

-- | Handler for @show_unit_leaders_tooltip@, which names a commander in a
-- tooltip. Script uses it to show what a @hidden_effect@ next to it just did, so
-- the commander's name is all there is to say.
showUnitLeader :: (HOI4Info g, Monad m) => StatementHandler g m
showUnitLeader [pdx| %_ = ?token |] = do
    nameloc <- getCharacterName token
    role <- getCharacterRole token
    msgToPP (MsgShowUnitLeader (Doc.oneLine nameloc) token role)
showUnitLeader stmt = preStatement stmt

-- | Handler for @character_list_tooltip@, which names in a tooltip every
-- character the conditions inside it pick out. Nothing but those conditions is
-- ever written in the block, so they go on the line rather than under it.
characterListTooltip :: (HOI4Info g, Monad m) => StatementHandler g m
characterListTooltip stmt@[pdx| %_ = @scr |] = do
    let (mlimit, rest) = extractStmt (matchLhsText "limit") scr
        (mamount, _) = extractStmt (matchLhsText "random_select_amount") rest
        amount = case mamount of
            Just [pdx| %_ = !num |] -> T.pack (show (round (num :: Double) :: Int))
            _ -> ""
    mclause <- maybe (return Nothing) limitClause mlimit
    case mclause of
        Just clause -> msgToPP (MsgCharacterListTooltip amount clause)
        Nothing -> do
            basemsg <- msgToPP (MsgCharacterListTooltip amount "")
            script_pp'd <- indentUp (ppMany scr)
            return $ basemsg ++ script_pp'd
characterListTooltip stmt = preStatement stmt

--------------------------------------------
-- Handler for remove_country_leader_role --
--------------------------------------------
-- | Handler for @remove_country_leader_role@, which stops a character leading
-- the country for one ideology. In a character scope the character is the one
-- in scope and is not named again.
removeCountryLeaderRole :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
removeCountryLeaderRole stmt@[pdx| %_ = @scr |] =
    case foldl' addLine (Nothing, Nothing) scr of
        (mchar, Just ideo) -> do
            ideoloc <- partyIcon ideo
            charloc <- traverse getCharacterName mchar
            msgToPP (MsgRemoveCountryLeaderRole (fromMaybe "" charloc) ideoloc)
        _ -> preStatement stmt
    where
        addLine (c, i) [pdx| character = $ch |] = (Just ch, i)
        addLine (c, i) [pdx| character = ?ch |] = (Just ch, i)
        addLine (c, i) [pdx| ideology = $ideo |] = (c, Just ideo)
        addLine acc stmt = warn (UnknownSection "remove_country_leader_role" stmt) acc
removeCountryLeaderRole stmt = preStatement stmt

-- | Handler for @can_be_country_leader@, which asks whether a character is fit
-- to lead the country. The character is the one in scope, or named outright.
canBeCountryLeader :: (HOI4Info g, Monad m) => StatementHandler g m
canBeCountryLeader stmt@[pdx| %_ = yes |] = withBool MsgCanBeCountryLeader stmt
canBeCountryLeader stmt@[pdx| %_ = no |] = withBool MsgCanBeCountryLeader stmt
canBeCountryLeader stmt = withCharacter MsgCanBeCountryLeaderChar stmt
