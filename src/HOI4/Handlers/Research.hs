{-|
Module      : HOI4.Handlers.Research
Description : Research, technology and doctrines

Handlers for research bonuses and breakthroughs, scientists, technology,
doctrine costs and doctrine mastery.
-}
module HOI4.Handlers.Research (
        addTechBonus
    ,   addBreakthrough
    ,   addScientistXp
    ,   doctrinePage
    ,   addDoctrineCostReduction
    ,   setTechnology
    ,   masteryGainModifier
    ,   addMastery
    ,   addMasteryBonus
    ,   hasDoctrine
    ) where

import Data.Char (toUpper)
import qualified Data.HashMap.Strict as HM
import Data.List (elemIndex, foldl', sortOn)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (foldM)

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (typewriterText)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp)
import StatementUtils -- everything

import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything
import HOI4.WikiTables (doctrineFolderIds, doctrineFolders)

import HOI4.Handlers.Core (constantOrNumber, joinClauses, msgToPP, msgToPP', plainMsg', preMessage, preStatement, sectionLink)

--------------------
-- add_tech_boost --
--------------------

data AddTechBonus = AddTechBonus
        {   tb_name :: Maybe Text
        ,   tb_bonus :: Maybe Double
        ,   tb_uses :: Double
        ,   tb_ahead_reduction :: Maybe Double
        ,   tb_category :: [Text]
        ,   tb_technology :: [Text]
        }

newATB :: AddTechBonus
newATB = AddTechBonus Nothing Nothing 1 Nothing [] []

addTechBonus :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addTechBonus stmt@[pdx| %_ = @scr |]
    = pp_atb =<< foldM addLine newATB scr
    where
        addLine :: AddTechBonus -> GenericStatement -> PPT g m AddTechBonus
        addLine atb [pdx| name = $name |] = do
            nameloc <- getGameL10n name
            return atb { tb_name = Just nameloc }
        addLine atb [pdx| bonus = !amt |] =
            return atb { tb_bonus = Just amt }
        addLine atb [pdx| ahead_reduction = !amt |] =
            return atb { tb_ahead_reduction = Just amt }
        addLine atb [pdx| uses = !amt  |]
            = return atb { tb_uses = amt }
        addLine atb [pdx| category = $cat |] = do
            let oldcat = tb_category atb
            catloc <- getGameL10n cat
            return atb { tb_category = oldcat ++ [catloc] }
        addLine atb [pdx| technology = $tech |] = do
            let oldtech = tb_technology atb
            techloc <- getGameL10n tech
            return atb { tb_technology = oldtech ++ [techloc] }
        addLine atb stmt = warn (UnknownSection "add_tech_bonus" stmt) (return atb)
        pp_atb :: AddTechBonus -> PPT g m IndentedMessages
        pp_atb atb = do
            -- What the bonus goes towards has a page of the wiki to itself,
            -- whether script named a whole category of research or a single
            -- technology out of one.
            let techcat = joinClauses
                    [ "[[" <> tc <> "]]" | tc <- tb_category atb ++ tb_technology atb ]
                uses = tb_uses atb
                tbmsg = case (tb_bonus atb, tb_ahead_reduction atb) of
                    (Just bonus, Just ahead) ->
                        MsgAddTechBonusAheadBoth bonus ahead techcat uses
                    (Just bonus, _) ->
                        MsgAddTechBonus bonus techcat uses
                    (_, Just ahead) ->
                        MsgAddTechBonusAhead ahead techcat uses
                    _ -> warn (BadValue "add_technology_bonus" stmt) $ preMessage stmt
            msgToPP tbmsg
addTechBonus stmt = preStatement stmt

-- | Handler for @add_breakthrough_progress@ and @add_breakthrough_points@, which
-- advance a country towards its next special project breakthrough in one
-- specialization, or hand it whole breakthroughs outright.
--
-- The amount may be a number or the name of a script constant, and the
-- specialization may be @all@, in which case there is no one specialization to
-- name and the message says so instead.
addBreakthrough :: (HOI4Info g, Monad m) =>
    ScriptMessage -> (Text -> Double -> ScriptMessage) -> StatementHandler g m
addBreakthrough headmsg linemsg stmt@[pdx| %_ = @scr |] = do
    let (mspec, rest) = extractStmt (matchLhsText "specialization") scr
        (mvalue, _) = extractStmt (matchLhsText "value") rest
    mamt <- maybe (return Nothing) constantOrNumber mvalue
    case (mspec, mamt) of
        (Just [pdx| %_ = $spec |], Just amt) -> do
            specloc <- if spec == "all"
                        then return ""
                        else Doc.oneLine <$> getGameL10n spec
            headpp <- msgToPP headmsg
            linepp <- indentUp (msgToPP (linemsg specloc amt))
            return $ headpp ++ linepp
        _ -> preStatement stmt
addBreakthrough _ _ stmt = preStatement stmt

-- | Handler for @add_scientist_xp@, which is written in the scope of the
-- scientist it is about, so the line above it is what names them. The amount is
-- most often a script constant rather than a number written out.
addScientistXp :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addScientistXp stmt@[pdx| %_ = @scr |] = do
    xp <- maybe (return Nothing) constantOrNumber (fst (extractStmt (matchLhsText "experience") scr))
    case (xp, fst (extractStmt (matchLhsText "specialization") scr)) of
        (Just amt, Just [pdx| %_ = $field |]) -> do
            fieldloc <- getGameL10n field
            msgToPP $ MsgAddScientistXp amt fieldloc
        _ -> preStatement stmt
addScientistXp stmt = preStatement stmt

-------------------------------------
-- Handler for add_doctrine_cost_reduction  --
-------------------------------------
-- | The wiki page a doctrine folder is written about on.
doctrinePage :: Text -> Text
doctrinePage folder = case T.uncons (T.replace "_" " " folder) of
    Just (c, rest) -> T.cons (toUpper c) rest <> " doctrine"
    Nothing -> "Doctrine"

-- | The folders the doctrine tree is divided into, which are the doctrine pages
-- the wiki has.

-- | A link to the page a doctrine category is written about, under the name the
-- game gives the category. Only the folders have a page each; a category
-- narrower than a folder has none of its own, and is left as its name.
doctrineCatLink :: Text -> Text -> Text
doctrineCatLink cat catloc = case T.stripSuffix "_doctrine" cat of
    Just folder | folder `elem` doctrineFolderIds ->
        mconcat ["[[", doctrinePage folder, "|", catloc, "]]"]
    _ -> catloc

data DoctrineCostReduction = DoctrineCostReduction
        {   dcr_name :: Maybe Text
        ,   dcr_cost_reduction :: Double
        ,   dcr_uses :: Double
        ,   dcr_category :: [Text]
        ,   dcr_technology :: [Text]
        }

newDCR :: DoctrineCostReduction
newDCR = DoctrineCostReduction Nothing 0 1 [] []

addDoctrineCostReduction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addDoctrineCostReduction stmt@[pdx| %_ = @scr |]
    = pp_dcr =<< foldM addLine newDCR scr
    where
        addLine :: DoctrineCostReduction -> GenericStatement -> PPT g m DoctrineCostReduction
        addLine dcr [pdx| name = $name |] = do
            nameloc <- getGameL10n name
            return dcr { dcr_name = Just nameloc }
        addLine dcr [pdx| cost_reduction = !amt |] =
            return dcr { dcr_cost_reduction = amt }
        addLine dcr [pdx| uses = !amt  |]
            = return dcr { dcr_uses = amt }
        addLine dcr [pdx| category = $cat |] = do
            let oldcat = dcr_category dcr
            catloc <- getGameL10n cat
            return dcr { dcr_category = oldcat ++ [doctrineCatLink cat catloc] }
        addLine dcr [pdx| technology = $tech |] = do
            let oldtech = dcr_technology dcr
            techloc <- getGameL10n tech
            return dcr { dcr_technology = oldtech ++ ["[[" <> techloc <> "]]"] }
        addLine dcr stmt = warn (UnknownSection "doctrine cost reduction" stmt) (return dcr)
        -- What the reduction covers is said in the same breath as the
        -- reduction itself, however many doctrines that is.
        pp_dcr :: DoctrineCostReduction -> PPT g m IndentedMessages
        pp_dcr dcr = msgToPP $ MsgAddDoctrineCostReduction
            (dcr_uses dcr)
            (dcr_cost_reduction dcr)
            (joinClauses (dcr_category dcr ++ dcr_technology dcr))
addDoctrineCostReduction stmt = preStatement stmt

setTechnology :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setTechnology stmt@[pdx| %_ = @scr |] =
        let (_, rest) = extractStmt (matchLhsText "popup") scr in
        mapM (\case
            stmt2@[pdx| $tech = !addrm |] -> do
                mtechloc <- getGameL10nIfPresent tech
                case mtechloc of
                    Just techloc -> msgToPP' $ MsgSetTechnology addrm techloc
                    _ -> msgToPP' $ MsgSetTechnology addrm (typewriterText tech)
            unknowntechformat -> msgToPP' $ preMessage unknowntechformat)
            rest
setTechnology stmt = preStatement stmt

----------------------------------------
-- Handlers for the doctrine mastery  --
----------------------------------------

-- | The name a part of the doctrine tree is written under, or 'Nothing' when no
-- key matches. Script names a folder, a track, a subdoctrine or a grand doctrine
-- by its id, and the game localizes each of them under a key built from that id
-- in a settled way. Two of those ways have exceptions to them often enough to be
-- worth following: the grand doctrines reworked for the doctrine tree keep a
-- @new_@ on the id that the key does not, and the air subdoctrines say
-- @air_subdoctrine_@ in the id where the key just says which one it is. The
-- naval subdoctrines take the name of their track on the front of the key
-- instead of the id, so those are tried as well.
--
-- Nothing here reads the doctrine files: one localization key is cheaper to keep
-- up with than another set of game files, and a part whose key is simply
-- misnamed comes out under its id, which says plainly enough what to fix.
doctrineLocLookup :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m (Maybe Text)
doctrineLocLookup kind theid = go (candidates kind (T.toLower theid))
    where
        go [] = return Nothing
        go (key:rest) = do
            mloc <- getGameL10nIfPresent key
            case mloc of
                Just loc | not (T.null loc) -> Just <$> getGameL10n key
                _ -> go rest
        candidates "folder" i = [i <> "_doctrine_folder"]
        candidates "track" i = ["DOCTRINE_TRACK_" <> T.toUpper i]
        candidates "grand_doctrine" i =
            ["GRAND_DOCTRINE_" <> T.toUpper (fromMaybe i (T.stripPrefix "new_" i))
            ,"GRAND_DOCTRINE_" <> T.toUpper i]
        candidates _ i =
            let bare = fromMaybe i (T.stripPrefix "air_subdoctrine_" i)
                stem = fromMaybe bare (T.stripSuffix "_no_lar" bare)
            in ["SUBDOCTRINE_" <> T.toUpper stem]
               ++ [ "SUBDOCTRINE_" <> t <> T.toUpper stem
                  | t <- ["SCREEN_", "SUBMARINE_", "CARRIER_"] ]

-- | A link to the part of the doctrine tree script has named, on the wiki page
-- for the folder it belongs to. A folder is the page itself; anything under one
-- is a heading on it.
doctrineLink :: (HOI4Info g, Monad m) => Text -> Text -> PPT g m Text
doctrineLink "folder" theid = return ("[[" <> doctrinePage theid <> "]]")
doctrineLink kind theid = do
    mname <- doctrineLocLookup kind theid
    case (mname, HM.lookup (T.toLower theid) doctrineFolders) of
        (Just name, Just folder) -> return $ sectionLink (doctrinePage folder) name name
        -- A part we know the page of but not the name, or the name but not the
        -- page, has nothing to make a heading out of, so it is left as the id
        -- script called it by. That is also what says which key to go and fix.
        (Just name, Nothing) -> return name
        _ -> return (typewriterText theid)

-- | The part of the doctrine tree a @..._mastery_gain_factor@ modifier is about,
-- together with the sentence the game words that modifier with. The modifier is
-- named after the part with nothing to say which kind of part it is, beyond a
-- @_track@ that the tracks carry, so the other kinds are tried in turn.
masteryGainModifier :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe (Text, Text))
masteryGainModifier lmod = case T.stripSuffix "_mastery_gain_factor" lmod of
    Nothing -> return Nothing
    Just stem -> case T.stripSuffix "_track" stem of
        Just track -> named "mod_track_mastery_gain_factor" "track" track
        Nothing -> go
            [ ("mod_folder_mastery_gain_factor", "folder")
            , ("mod_grand_doctrine_mastery_gain_factor", "grand_doctrine")
            , ("mod_subdoctrine_mastery_gain_factor", "sub_doctrine") ]
            stem
    where
        go [] _ = return Nothing
        go ((wrapper, kind):rest) stem = do
            found <- named wrapper kind stem
            maybe (go rest stem) (return . Just) found
        named wrapper kind theid = fmap ((,) wrapper) <$> doctrineLocLookup kind theid

-- | Handler for @add_mastery@ and @add_daily_mastery@, which hand a country
-- mastery towards part of its doctrine tree, in one go or so much a day for a
-- while.
-- | The shape the mastery effects share: an amount under some label, maybe a
-- number of days, and conditions saying which part of the tree is meant. The
-- message is picked from the amount and days; 'Nothing' falls back to the raw
-- statement.
masteryStmt :: forall g m. (HOI4Info g, Monad m) =>
    Text -- ^ label the amount is written under
    -> String -- ^ effect name, for trace messages
    -> (Maybe Double -> Maybe Double -> Maybe (Text -> ScriptMessage))
    -> StatementHandler g m
masteryStmt amtlabel what mkmsg stmt@[pdx| %_ = @scr |] =
    case mkmsg amount days of
        Just msg -> masteryTargets msg targets
        _ -> preStatement stmt
    where
        (amount, days, targets) = foldl' addLine (Nothing, Nothing, []) scr
        addLine (amt, d, t) [pdx| $lbl = !n |]
            | lbl == amtlabel = (Just n, d, t)
            | lbl == "days" = (amt, Just n, t)
        -- The name is what a later effect takes the mastery away by, and says
        -- nothing to a reader.
        addLine acc [pdx| name = %_ |] = acc
        addLine (amt, d, ts) [pdx| $kind = $theid |]
            | kind `elem` masteryConditions = (amt, d, ts ++ [(kind, theid)])
        addLine acc stmt = warn (UnknownSection (T.pack what) stmt) acc
masteryStmt _ _ _ stmt = preStatement stmt

addMastery :: forall g m. (HOI4Info g, Monad m) => Bool -> StatementHandler g m
addMastery daily = masteryStmt "amount" "add_mastery" $ \mamt mdays -> case mamt of
    Just amt | not daily -> Just (MsgAddMastery amt)
             | Just d <- mdays -> Just (MsgAddDailyMastery amt d)
    _ -> Nothing

-- | Handler for @add_mastery_bonus@, which raises how fast mastery comes in for
-- part of the doctrine tree, for a while.
addMasteryBonus :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addMasteryBonus = masteryStmt "bonus" "add_mastery_bonus" $ \mb md -> case (mb, md) of
    (Just b, Just d) -> Just (MsgAddMasteryBonus b d)
    _ -> Nothing

-- | The ways script narrows down which part of the doctrine tree a mastery
-- effect is aimed at, in the order the game sets them out in.
masteryConditions :: [Text]
masteryConditions = ["folder", "grand_doctrine", "sub_doctrine", "track"]

-- | How the part of the doctrine tree a mastery effect is aimed at is written
-- out. Script may narrow it down in more than one way at once -- a track of a
-- given type, under a given grand doctrine -- and the game sets those out one
-- to a line below the effect, since none of them alone says where the mastery
-- goes. A single one is said in the same breath as the effect, and with none at
-- all the mastery goes to the whole tree.
masteryTargets :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> [(Text, Text)] -> PPT g m IndentedMessages
masteryTargets msg [] = msgToPP (msg "every doctrine")
masteryTargets msg [cond] = msgToPP . msg =<< masteryTarget cond
masteryTargets msg conds = do
    header <- msgToPP (msg "all tracks that:")
    said <- indentUp (traverse conditionLine (sortOn order conds))
    return (header ++ said)
    where
        order (kind, _) = fromMaybe (length masteryConditions) (elemIndex kind masteryConditions)
        conditionLine (kind, theid) = plainMsg' . conditionText kind =<< doctrineLink kind theid
        conditionText "folder" link = "Are in the " <> link <> " folder"
        conditionText "grand_doctrine" link = "Have the " <> link <> " grand doctrine"
        conditionText "sub_doctrine" link = "Have the " <> link <> " subdoctrine"
        conditionText "track" link = "Are of type " <> link
        conditionText _ link = link

-- | How one way of narrowing down where the mastery goes is written out on its
-- own. A track holds a run of subdoctrines rather than being one thing, so it is
-- spoken of in the plural.
masteryTarget :: (HOI4Info g, Monad m) => (Text, Text) -> PPT g m Text
masteryTarget ("track", theid) = do
    link <- doctrineLink "track" theid
    return ("all " <> link <> " tracks")
masteryTarget (kind, theid) = doctrineLink kind theid

-- | Handler for @has_doctrine@, which asks whether a grand doctrine or one of
-- the subdoctrines under it is the one the country is running. The statement
-- does not say which kind of part it names, so the grand doctrines are tried
-- first and anything else is taken for a subdoctrine.
hasDoctrine :: (HOI4Info g, Monad m) => StatementHandler g m
hasDoctrine [pdx| %_ = $theid |] = do
    mgrand <- doctrineLocLookup "grand_doctrine" theid
    link <- doctrineLink (maybe "subdoctrine" (const "grand_doctrine") mgrand) theid
    msgToPP (MsgHasDoctrine link)
hasDoctrine stmt = preStatement stmt
