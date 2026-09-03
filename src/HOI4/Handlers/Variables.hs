{-|
Module      : HOI4.Handlers.Variables
Description : Script variables and arrays

Handlers for setting, clamping, checking and exporting variables, and for
the loops over arrays and their sizes.
-}
module HOI4.Handlers.Variables (
        setVariable
    ,   clampVariable
    ,   checkVariable
    ,   exportVariable
    ,   setVariableToRandom
    ,   arrayLoop
    ,   collectionSize
    ,   arrayValue
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (typewriterText, plainNum, plainNumMin)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, withCurrentIndent, LocArg (..))
import StatementUtils -- everything

import {-# SOURCE #-} HOI4.Common (ppMany)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (headed, msgToPP, preMessage, preStatement)

------------------------------
-- Handler for xxx_variable --
------------------------------

data SetVariable = SetVariable
        { sv_which  :: Maybe Text
        , sv_which2 :: Maybe Text
        , sv_value  :: Maybe Double
        , sv_tooltip  :: Maybe Text
        }

newSV :: SetVariable
newSV = SetVariable Nothing Nothing Nothing Nothing

setVariable :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) ->
    (Text -> Double -> ScriptMessage) ->
    StatementHandler g m
setVariable msgWW msgWV stmt@[pdx| %_ = @scr |]
    = pp_sv =<< resolveConstant (foldl' addLine newSV scr)
    where
        addLine :: SetVariable -> GenericStatement -> SetVariable
        addLine sv [pdx| var = ?val |]
            = if isNothing (sv_which sv) then
                sv { sv_which = Just val }
              else
                sv { sv_which2 = Just val }
        addLine sv [pdx| value = !val |]
            = sv { sv_value = Just val }
        addLine sv [pdx| value = ?val |]
            = sv { sv_which2 = Just val }
        addLine sv [pdx| tooltip = ?txt |]
            = sv { sv_tooltip = Just txt }
        addLine sv [pdx| $var = !val |]
            = sv { sv_which = Just var, sv_value = Just val }
        addLine sv [pdx| $var = $val |]
            = sv { sv_which = Just var, sv_which2 = Just val }
        addLine sv [pdx| $vartag:$var = !val |]
            = sv { sv_which = Just (vartag <> ":" <> var), sv_value = Just val }
        addLine sv [pdx| $vartag:$var = $valtag:$val |]
            = sv { sv_which = Just (vartag <> ":" <> var), sv_which2 = Just (valtag <> ":" <> val) }
        addLine sv [pdx| $var = $valtag:$val |]
            = sv { sv_which = Just var, sv_which2 = Just (valtag <> ":" <> val) }
        addLine sv [pdx| $vartag:$var = $val |]
            = sv { sv_which = Just (vartag <> ":" <> var), sv_which2 = Just val }
        addLine sv _ = warn (BadValue "set_variable" stmt) sv
        toTT :: Text -> Text
        toTT = typewriterText
        -- A value script names as a script constant is a number like any other,
        -- and the same number every time, so it is written as that number rather
        -- than as a name only the script knows the meaning of.
        resolveConstant :: SetVariable -> PPT g m SetVariable
        resolveConstant sv = case T.stripPrefix "constant:" =<< sv_which2 sv of
            Just path -> do
                mval <- constantValue path
                return $ case mval of
                    Just val -> sv { sv_which2 = Nothing, sv_value = Just val }
                    Nothing -> sv
            Nothing -> return sv
        -- The modifier localization a tooltip names has a slot for the value
        -- being written, which the game fills in as it draws the tooltip and the
        -- statement has just told us.
        rightArg :: SetVariable -> HashMap Text LocArg
        rightArg sv = case (sv_value sv, sv_which2 sv) of
            (Just val, _) -> HM.singleton "RIGHT" (LocNum val)
            (_, Just var) -> HM.singleton "RIGHT" (LocText (toTT var))
            _ -> HM.empty
        pp_sv :: SetVariable -> PPT g m IndentedMessages
        pp_sv sv = case (sv_which sv, sv_which2 sv, sv_value sv) of
            (Just v1, Just v2, Nothing) -> withTooltip sv (msgWW (toTT v1) (toTT v2)) (msgWW v1 v2)
            (Just v,  Nothing, Just val) -> withTooltip sv (msgWV (toTT v) val) (msgWV v val)
            _ -> preStatement stmt
        -- A statement with a tooltip is shown by that tooltip: it is the
        -- game's own sentence for what the change means, where the statement
        -- itself only says what it does to a name no reader knows. The
        -- literal reading goes behind a hover after it, for whoever wants the
        -- variable itself -- written without markup, like every other hover,
        -- since it ends up in a title attribute where tags don't render.
        withTooltip :: SetVariable -> ScriptMessage -> ScriptMessage -> PPT g m IndentedMessages
        withTooltip sv marked plain = case sv_tooltip sv of
            Nothing -> msgToPP marked
            Just tt -> do
                ttloc <- getGameL10nArgs (rightArg sv) tt
                literal <- messageText plain
                msgToPP $ MsgVariableTooltip ttloc literal
setVariable _ _ stmt = preStatement stmt

data ClampVariable = ClampVariable
        { clv_which  :: Text
        , clv_var :: Maybe Text
        , clv_var2 :: Maybe Text
        , clv_value  :: Maybe Double
        , clv_value2  :: Maybe Double
        }

newCLV :: ClampVariable
newCLV = ClampVariable undefined Nothing Nothing Nothing Nothing

clampVariable :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Double -> Double -> ScriptMessage) ->
    (Text -> Double -> Text -> ScriptMessage) ->
    (Text -> Text -> Double -> ScriptMessage) ->
    (Text -> Text -> Text -> ScriptMessage) ->
    StatementHandler g m
clampVariable msgVV msgVW msgWV msgWW stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_clv (foldl' addLine newCLV scr)
    where
        addLine :: ClampVariable -> GenericStatement -> ClampVariable
        addLine clv [pdx| var = $var |]
            = clv { clv_which = var }
        addLine clv [pdx| variable = $var |]
            = clv { clv_which = var }
        addLine clv [pdx| min = !val |]
            = clv { clv_value = Just val }
        addLine clv [pdx| max = !val |]
            = clv { clv_value2 = Just val }
        addLine clv [pdx| min = $var |]
            = clv { clv_var = Just var}
        addLine clv [pdx| max = $var |]
            = clv { clv_var2 = Just var }
        addLine clv stmt = warn (UnknownSection "clamp_variable" stmt) clv
        toTT :: Text -> Text
        toTT = typewriterText
        pp_clv :: ClampVariable -> PPT g m ScriptMessage
        pp_clv clv = case (clv_value clv,  clv_value2 clv,
                             clv_var clv, clv_var2 clv) of
            (Just val, Just val2, Nothing, Nothing) -> do return $ msgVV (clv_which clv) val val2
            (Just val, Nothing, Nothing, Just v2) -> do return $ msgVW (clv_which clv) val (toTT v2)
            (Nothing, Just val2, Just v1, Nothing) -> do return $ msgWV (clv_which clv) (toTT v1) val2
            (Nothing, Nothing, Just v1, Just v2) -> do return $ msgWW (clv_which clv) (toTT v1) (toTT v2)
            _ ->  do return $ preMessage stmt
clampVariable _ _ _ _ stmt = preStatement stmt

data CheckVariable = CheckVariable
        { cv_which  :: Maybe Text
        , cv_which2 :: Maybe Text
        , cv_value  :: Maybe Double
        , cv_comp   :: Text
        }

newCV :: CheckVariable
newCV = CheckVariable Nothing Nothing Nothing "greater than or equals"

checkVariable :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> ScriptMessage) ->
    (Text -> Text -> Double -> ScriptMessage) ->
    StatementHandler g m
checkVariable msgWW msgWV stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_cv (foldl' addLine newCV scr)
    where
        addLine :: CheckVariable -> GenericStatement -> CheckVariable
        -- Names the text the game shows in place of the comparison. The
        -- comparison is still what is checked, so it is what gets written
        -- out; the key is known here so the short forms below do not take
        -- it for the variable.
        addLine cv [pdx| tooltip = %_ |] = cv
        addLine cv [pdx| var = $val |]
            = cv { cv_which = Just val }
        addLine cv [pdx| var = $vartag:$val |]
            = cv { cv_which = Just (vartag <> ":" <> val) }
        addLine cv [pdx| value = !val |]
            = cv { cv_value = Just val }
        addLine cv [pdx| value > !val |]
            = cv { cv_value = Just val, cv_comp = "greater than" }
        addLine cv [pdx| value < !val |]
            = cv { cv_value = Just val, cv_comp = "less than" }
        addLine cv [pdx| value = $val |]
            = cv { cv_which2 = Just val }
        addLine cv [pdx| value > $val |]
            = cv { cv_which2 = Just val, cv_comp = "greater than" }
        addLine cv [pdx| value < $val |]
            = cv { cv_which2 = Just val, cv_comp = "less than" }
        addLine cv [pdx| value = $vartag:$val |]
            = cv { cv_which2 = Just (vartag <> ":" <> val) }
        addLine cv [pdx| value > $vartag:$val |]
            = cv { cv_which2 = Just (vartag <> ":" <> val), cv_comp = "greater than" }
        addLine cv [pdx| value < $vartag:$val |]
            = cv { cv_which2 = Just (vartag <> ":" <> val), cv_comp = "less than" }
        addLine cv [pdx| compare = $comp |]
            = cv { cv_comp = T.replace "_" " " comp}
        -- The short forms below name the variable on the left, so they match any
        -- statement at all. A field we do not know -- @tooltip@, which names the
        -- text the game shows in place of the comparison -- would be read as one
        -- of them and overwrite the variable already found, so a short form is
        -- only taken while nothing has claimed the slot.
        addLine cv [pdx| $var = !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_value = Just val, cv_comp = "equals" }
        addLine cv [pdx| $var < !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_value = Just val, cv_comp = "less than" }
        addLine cv [pdx| $var > !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_value = Just val, cv_comp = "greater than" }
        addLine cv [pdx| $var = $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just val, cv_comp = "equals" }
        addLine cv [pdx| $var < $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just val, cv_comp = "less than" }
        addLine cv [pdx| $var > $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just val, cv_comp = "greater than" }
        addLine cv [pdx| $var = $vartag:$val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just (vartag <> ":" <> val), cv_comp = "equals" }
        addLine cv [pdx| $var < $vartag:$val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just (vartag <> ":" <> val), cv_comp = "less than" }
        addLine cv [pdx| $var > $vartag:$val |] | isNothing (cv_which cv)
            = cv { cv_which = Just var, cv_which2 = Just (vartag <> ":" <> val), cv_comp = "greater than" }
        -- A variable of another country's is reached through that country, which
        -- leaves the name on the left carrying tags of its own. The same
        -- happens to an array element picked out by another variable
        -- (@arr@var:idx@), so the two-segment forms cover both.
        addLine cv [pdx| $a:$b = !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just (a <> ":" <> b), cv_value = Just val, cv_comp = "equals" }
        addLine cv [pdx| $a:$b < !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just (a <> ":" <> b), cv_value = Just val, cv_comp = "less than" }
        addLine cv [pdx| $a:$b > !val |] | isNothing (cv_which cv)
            = cv { cv_which = Just (a <> ":" <> b), cv_value = Just val, cv_comp = "greater than" }
        addLine cv [pdx| $a:$b = $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just (a <> ":" <> b), cv_which2 = Just val, cv_comp = "equals" }
        addLine cv [pdx| $a:$b < $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just (a <> ":" <> b), cv_which2 = Just val, cv_comp = "less than" }
        addLine cv [pdx| $a:$b > $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just (a <> ":" <> b), cv_which2 = Just val, cv_comp = "greater than" }
        addLine cv [pdx| $a:$b:$c = $val |] | isNothing (cv_which cv)
            = cv { cv_which = Just (a <> ":" <> b <> ":" <> c), cv_which2 = Just val, cv_comp = "equals" }
        addLine cv stmt = warn (UnknownSection "check_variable" stmt) cv
        toTT :: Text -> Text
        toTT = typewriterText
        pp_cv :: CheckVariable -> PPT g m ScriptMessage
        pp_cv cv = case (cv_which cv, cv_which2 cv, cv_value cv, cv_comp cv) of
            (Just v1, Just v2, Nothing, comp) -> do
                mconst <- if "constant:" `T.isPrefixOf` v2
                    then constantValue v2
                    else return Nothing
                case mconst of
                    Just val -> return $ msgWV comp (toTT v1) val
                    Nothing -> return $ msgWW comp (toTT v1) (toTT v2)
            (Just v,  Nothing, Just val, comp) -> do return $ msgWV comp (toTT v) val
            _ -> do return $ preMessage stmt
checkVariable _ _ stmt = preStatement stmt

-------------------------------------
-- Handler for export_to_variable  --
-------------------------------------

data ExportVariable = ExportVariable
        { ev_which  :: Maybe Text
        , ev_value :: Maybe Text
        , ev_who :: Maybe Text
        } deriving Show

newEV :: ExportVariable
newEV = ExportVariable Nothing Nothing Nothing

exportVariable :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
exportVariable stmt@[pdx| %_ = @scr |] = msgToPP =<< pp_ev (foldl' addLine newEV scr)
    where
        addLine :: ExportVariable -> GenericStatement -> ExportVariable
        addLine ev [pdx| which = ?val |]
            = ev { ev_which = Just val }
        addLine ev [pdx| variable_name = ?val |]
            = ev { ev_which = Just val }
        addLine ev [pdx| value = ?val |]
            = ev { ev_value = Just val }
        addLine ev [pdx| who = ?val |]
            = ev { ev_who = Just val }
        addLine ev stmt = warn (UnknownSection "export_to_variable" stmt) ev
        toTT :: Text -> Text
        toTT = typewriterText
        pp_ev :: ExportVariable -> PPT g m ScriptMessage
        pp_ev ExportVariable { ev_which = Just which, ev_value = Just value, ev_who = Nothing } =
            return $ MsgExportVariable (toTT which) value
        pp_ev ExportVariable { ev_which = Just which, ev_value = Just value, ev_who = Just who } = do
            whoLoc <- Doc.doc2text <$> allowPronoun (Just HOI4Country) (fmap Doc.strictText . getGameL10n) who
            return $ MsgExportVariableWho (toTT which) value whoLoc
        pp_ev _ = return $ warn (BadValue "export_to_variable" stmt) $ preMessage stmt
exportVariable stmt = warn (UnknownSection "export_to_variable" stmt) $ preStatement stmt

------------------------------------
-- handler for set_variable_to_random --
------------------------------------

-- | Puts a random number in a variable. Written bare it is a number from 0 up
-- to 1; the block form gives the range and says whether it is a whole number.
setVariableToRandom :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
setVariableToRandom [pdx| %_ = @scr |] = do
    let (mvar, s1) = extractStmt (matchLhsText "var") scr
        (mmin, s2) = extractStmt (matchLhsText "min") s1
        (mmax, s3) = extractStmt (matchLhsText "max") s2
        (minteger, _) = extractStmt (matchLhsText "integer") s3
        bound def mstmt = case mstmt of
            Just [pdx| %_ = !num |] -> Doc.doc2text (plainNumMin (num :: Double))
            Just [pdx| %_ = $vartag:$var |] -> vartag <> ":" <> var
            Just [pdx| %_ = $var |] -> var
            _ -> def
        varname = case mvar of
            Just [pdx| %_ = $vartag:$var |] -> vartag <> ":" <> var
            Just [pdx| %_ = ?var |] -> var
            _ -> "<!-- Check Script -->"
        isint = case minteger of
            Just [pdx| %_ = yes |] -> True
            _ -> False
    msgToPP $ MsgSetVariableToRandom varname (bound "0" mmin) (bound "1" mmax) isint
setVariableToRandom [pdx| %_ = ?var |] = msgToPP $ MsgSetVariableToRandom var "0" "1" False
setVariableToRandom stmt = preStatement stmt

-------------------------------------------
-- handler for the loops over an array   --
-------------------------------------------

-- | A loop or a test that runs over an array: @any_of@, @all_of@,
-- @for_each_loop@. Which temporary variables the loop writes its value and its
-- index into says nothing about what it does with them, and the script under it
-- names them itself wherever it reads them.
arrayLoop :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -- ^ Message to use as the block header
        -> StatementHandler g m
arrayLoop header [pdx| %_ = @scr |] = withCurrentIndent $ \i -> do
    let (marray, s1) = extractStmt (matchLhsText "array") scr
        (_, s2) = extractStmt (matchLhsText "value") s1
        (_, rest) = extractStmt (matchLhsText "index") s2
        arr = case marray of
            Just [pdx| %_ = ?a |] -> a
            _ -> "<!-- Check Script -->"
    script_pp'd <- ppMany rest
    return (headed i (header arr) script_pp'd)
arrayLoop _ stmt = preStatement stmt

-------------------------------
-- handler for collection_size --
-------------------------------

-- | How many things a collection gathers. The collection is either named or
-- written out in full; where it is written out, saying so is as much as the
-- line can carry, since the whole of it stands in the script beside.
collectionSize :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
collectionSize stmt@[pdx| %_ = @scr |] = do
    let (minput, rest) = extractStmt (matchLhsText "input") scr
        (mvalue, _) = extractStmt (matchLhsText "value") rest
    whatloc <- case minput of
        Just [pdx| %_ = ?inp |] -> do
            let collkey = fromMaybe inp (T.stripPrefix "collection:" inp)
            getGameL10n ("COLLECTION_" <> T.toUpper collkey)
        _ -> return "The collection"
    case mvalue of
        Just [pdx| %_ > !num |] -> msgToPP $ MsgCollectionSize "" whatloc "more than" num
        Just [pdx| %_ < !num |] -> msgToPP $ MsgCollectionSize "" whatloc "less than" num
        Just [pdx| %_ = !num |] -> msgToPP $ MsgCollectionSize "" whatloc "exactly" num
        Just [pdx| %_ > $var |] -> msgToPP $ MsgCollectionSizeVar "" whatloc "more than" var
        Just [pdx| %_ < $var |] -> msgToPP $ MsgCollectionSizeVar "" whatloc "less than" var
        _ -> preStatement stmt
collectionSize stmt = preStatement stmt

-------------------------
-- Handlers for arrays --
-------------------------
-- | The array a statement names and the value it carries, written either as
-- @array = name value = x@ or in the short form that names the array on the
-- left and the value on the right.
arrayAndValue :: GenericScript -> Maybe (Text, Maybe Text)
arrayAndValue scr = case foldl' addLine (Nothing, Nothing, Nothing) scr of
    (Just arr, val, _) -> Just (arr, val)
    (Nothing, _, Just short) -> Just short
    _ -> Nothing
    where
        addLine (arr, val, short) [pdx| array = $a |] = (Just a, val, short)
        addLine (arr, val, short) [pdx| array = ?a |] = (Just a, val, short)
        addLine (arr, val, short) [pdx| value = ?v |] = (arr, Just v, short)
        addLine (arr, val, short) [pdx| value = !v |] =
            (arr, Just (Doc.doc2text (plainNum (v :: Double))), short)
        addLine acc [pdx| index = %_ |] = acc
        -- Short form. Only taken if no @array@ line has claimed the slot, so
        -- that the long form is never read as one of these.
        addLine (arr, val, short) [pdx| $a = ?v |] = (arr, val, Just (a, Just v))
        addLine (arr, val, short) [pdx| $a = !v |] =
            (arr, val, Just (a, Just (Doc.doc2text (plainNum (v :: Double)))))
        addLine acc stmt = warn (UnknownSection "array effect" stmt) acc

-- | Handler for @add_to_array@ and @is_in_array@, which take an array and a
-- value. A value left out of @add_to_array@ means whatever is in scope.
arrayValue :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> StatementHandler g m
arrayValue msg stmt@[pdx| %_ = @scr |] = case arrayAndValue scr of
    Just (arr, val) -> msgToPP (msg arr (fromMaybe "" val))
    Nothing -> preStatement stmt
arrayValue _ stmt = preStatement stmt
