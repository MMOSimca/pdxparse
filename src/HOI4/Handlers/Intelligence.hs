{-|
Module      : HOI4.Handlers.Intelligence
Description : Intelligence and operatives

Handlers for intel and decryption, the intelligence agency and its
upgrades, and operatives.
-}
module HOI4.Handlers.Intelligence (
        addIntel
    ,   addDecryption
    ,   createOperativeLeader
    ,   createIntelligenceAgency
    ,   agencyUpgradeLink
    ) where

import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import MessageTools (typewriterText)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, concatMapM)
import StatementUtils -- everything

import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything
import HOI4.WikiTables (agencyUpgradeBranches)

import HOI4.Handlers.Core (getbaretraits, msgToPP, plainMsg', preStatement, sectionLink)
import HOI4.Handlers.Characters (getUnitTraits)

------------------------------------------------
-- handlers for the intelligence effects       --
------------------------------------------------

-- | Intel gained on another country, which is counted separately for each of
-- the four branches.
addIntel :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addIntel stmt@[pdx| %_ = @scr |] = do
    let (mtarget, kinds) = extractStmt (matchLhsText "target") scr
    case mtarget of
        Just [pdx| %_ = ?who |] -> do
            whoflag <- flagText (Just HOI4Country) who
            header <- msgToPP $ MsgAddIntel whoflag
            branches <- indentUp (concat <$> traverse ppKind kinds)
            return $ header ++ branches
        _ -> preStatement stmt
    where
        ppKind :: GenericStatement -> PPT g m IndentedMessages
        ppKind [pdx| $kind = !num |] = do
            kindloc <- getGameL10n (intelKey kind)
            msgToPP $ MsgAddIntelKind kindloc num
        ppKind st = preStatement st
        -- The ledger the game names these on calls the air branch AIR_INTEL,
        -- where script writes airforce_intel.
        intelKey "airforce_intel" = "AIR_INTEL"
        intelKey kind = T.toUpper kind
addIntel stmt = preStatement stmt

-- | Decryption gained against another country, given either as a flat amount or
-- as a share of what the target has to defend with.
addDecryption :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addDecryption stmt@[pdx| %_ = @scr |] = do
    let (mtarget, rest) = extractStmt (matchLhsText "target") scr
        (mratio, rest') = extractStmt (matchLhsText "ratio") rest
        (mamount, _) = extractStmt (matchLhsText "amount") rest'
    whoflag <- case mtarget of
        Just [pdx| %_ = ?who |] -> flagText (Just HOI4Country) who
        _ -> return ""
    case (mratio, mamount) of
        (Just [pdx| %_ = !num |], _) -> msgToPP $ MsgAddDecryptionRatio whoflag num
        (_, Just [pdx| %_ = !num |]) -> msgToPP $ MsgAddDecryptionAmount whoflag num
        _ -> preStatement stmt
addDecryption stmt = preStatement stmt

-- operatives

data CreateOperative = CreateOperative
        {   co_bypass_recruitment :: Bool
        ,   co_name :: Text
        ,   co_traits :: Maybe [Text]
        ,   co_nationalities :: Maybe [Text]
        ,   co_available_to_spy_master :: Bool
        ,   co_gender :: Maybe Text -- ^ @male@ or @female@, where script picks one
        }

newCO :: CreateOperative
newCO = CreateOperative False "" Nothing Nothing False Nothing

createOperativeLeader :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createOperativeLeader stmt@[pdx| %_ = @scr |]
    = ppCO (foldl' addLine newCO scr)
    where
        addLine :: CreateOperative -> GenericStatement -> CreateOperative
        addLine co [pdx| bypass_recruitment = %rhs |]
            | GenericRhs "yes" [] <- rhs = co { co_bypass_recruitment = True }
            | GenericRhs "no" [] <- rhs = co { co_bypass_recruitment = False }
        addLine co [pdx| name = ?txt |] = co {co_name = txt}
        addLine co [pdx| traits = @arr |] =
            let traits = mapMaybe getbaretraits arr
            in co {co_traits = Just traits}
        addLine co [pdx| nationalities = @arr |] =
            let nats = mapMaybe getbaretraits arr
            in co {co_nationalities = Just nats}
        addLine co [pdx| available_to_spy_master = %rhs |]
            | GenericRhs "yes" [] <- rhs = co { co_available_to_spy_master = True }
            | otherwise = co
        -- Script fixes the operative's gender under either label; left alone,
        -- the game picks one at random.
        addLine co [pdx| female = %rhs |]
            | GenericRhs "yes" [] <- rhs = co { co_gender = Just "female" }
            | GenericRhs "no" [] <- rhs = co { co_gender = Just "male" }
        addLine co [pdx| gender = $g |]
            | T.toLower g `elem` ["male", "female"] = co { co_gender = Just (T.toLower g) }
        -- Portrait graphics: which picture the operative gets, or (with
        -- @portrait_tag_override@) which country's portrait pool it is
        -- drawn from. Nothing of it survives onto the page.
        addLine co [pdx| $lhs = %_ |]
            | T.toLower lhs == "gfx" = co
            | T.toLower lhs == "portrait_tag_override" = co
        addLine co stmt = warn (UnknownSection "create_operative_leader" stmt) co

        ppCO co = do
            natmsg <- case co_nationalities co of
                    Just nats -> do
                        flagged <- mapM (flagText (Just HOI4Country)) nats
                        return $ T.intercalate ", " flagged
                    _ -> return ""
            -- Script names the operative by a localization key as often as by
            -- a quoted name; the key resolves where one exists and stands as
            -- written where it does not.
            nameloc <- getGameL10n (co_name co)
            basemsg <- msgToPP $ MsgCreateOperativeLeader nameloc natmsg (fromMaybe "" (co_gender co)) (co_bypass_recruitment co) (co_available_to_spy_master co)
            traitsmsg <- case co_traits co of
                Just traits -> concatMapM (\t -> do
                    tloc <- getGameL10n t
                    namemsg <- indentUp $ plainMsg' ("'''" <> tloc <> "'''")
                    traitmsg <- indentUp $ indentUp $ getUnitTraits t
                    return $ namemsg : traitmsg
                    ) traits
                _ -> return []
            return $ basemsg ++ traitsmsg
createOperativeLeader stmt = preStatement stmt

------------------------------------------
-- Handlers for the intelligence agency --
------------------------------------------

-- | Handler for @create_intelligence_agency@, whose block gives the agency a
-- name and picks a logo for it. The logo is a graphics id and says nothing to a
-- reader, so only the name is written out. A name is sometimes spelled out in
-- the block and sometimes given as a key to look up.
createIntelligenceAgency :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
createIntelligenceAgency [pdx| %_ = @scr |] = case foldl' addLine Nothing scr of
    Nothing -> msgToPP MsgCreateIntelligenceAgencyPlain
    Just name -> do
        nameloc <- getGameL10nIfPresent name
        msgToPP (MsgCreateIntelligenceAgency (fromMaybe name nameloc))
    where
        addLine :: Maybe Text -> GenericStatement -> Maybe Text
        addLine _ [pdx| name = ?name |] = Just name
        addLine acc [pdx| icon = %_ |] = acc
        addLine acc stmt = warn (UnknownSection "create_intelligence_agency" stmt) acc
-- Written bare wherever the agency takes the country's own name.
createIntelligenceAgency [pdx| %_ = yes |] = msgToPP MsgCreateIntelligenceAgencyPlain
createIntelligenceAgency stmt = preStatement stmt

-- | Handler for @upgrade_intelligence_agency@, which puts one upgrade of the
-- agency into effect.
upgradeIntelligenceAgency :: (HOI4Info g, Monad m) => StatementHandler g m
upgradeIntelligenceAgency [pdx| %_ = $upgrade |] =
    msgToPP . MsgUpgradeIntelligenceAgency =<< agencyUpgradeLink upgrade
upgradeIntelligenceAgency stmt = preStatement stmt

-- | Handler for @has_done_agency_upgrade@, which asks whether an upgrade is
-- already in effect.
hasDoneAgencyUpgrade :: (HOI4Info g, Monad m) => StatementHandler g m
hasDoneAgencyUpgrade [pdx| %_ = $upgrade |] =
    msgToPP . MsgHasDoneAgencyUpgrade =<< agencyUpgradeLink upgrade
hasDoneAgencyUpgrade stmt = preStatement stmt

-- | A link to an agency upgrade, on the wiki page the agency is written about
-- on, where the branch of upgrades it belongs to is a heading. Script names an
-- upgrade without saying which branch holds it, and the branches live in a file
-- that says little else we want, so which branch each upgrade sits in is listed
-- here instead.
agencyUpgradeLink :: (HOI4Info g, Monad m) => Text -> PPT g m Text
agencyUpgradeLink theid = do
    mname <- getGameL10nIfPresent theid
    case (mname, HM.lookup (T.toLower theid) agencyUpgradeBranches) of
        (Just name, Just branch) -> do
            branchloc <- getGameL10n branch
            return $ sectionLink "Intelligence agency" (branchloc <> " Branch") name
        -- An upgrade we know the name of but not the branch has no heading to
        -- jump to; one we do not even have a name for is left as the id script
        -- called it by, which is also what says the key needs fixing.
        (Just name, Nothing) -> return name
        _ -> return (typewriterText theid)
