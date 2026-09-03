{-|
Module      : HOI4.Handlers.Generic
Description : Handlers parameterized by message rather than by statement

A localized atom, a number, a comparison, a flag, a yes or no, a text and
value pair: these shapes cover most of the script, and the handler table in
"HOI4.Common" pairs each with the message its statement wants.
-}
module HOI4.Handlers.Generic (
        withLocAtom'
    ,   withLocAtom
    ,   withLocAtomName
    ,   withLookupAtom
    ,   withLookupAtomKey
    ,   withLocAtomCompound
    ,   withLocAtomKey
    ,   withLocAtomIcon
    ,   withState
    ,   withNonlocAtom
    ,   numeric
    ,   numericOrVar
    ,   numericCompare
    ,   numericCompareCompound
    ,   numericCompareCompoundLoc
    ,   withFlag
    ,   withBool
    ,   withBoolHOI4Scope
    ,   withFlagOrBool
    ,   numericIcon
    ,   numericIconLoc
    ,   numericLoc
    ,   TextValue (..)
    ,   parseTV
    ,   textValue
    ,   textValueKey
    ,   textValueCompare
    ,   withNonlocTextValue
    ,   ValueValue (..)
    ,   parseVV
    ,   valueValue
    ,   withProvince
    ,   TextAtom (..)
    ,   parseTA
    ,   textAtom
    ,   textAtomKey
    ,   taTypeFlag
    ,   simpleEffectNum
    ,   simpleEffectAtom
    ,   withMaybelocAtom2
    ,   withRegion
    ) where

import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (typewriterText)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, IsGameState (..), GameState (..))
import StatementUtils -- everything

import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (msgToPP, plainMsg, preMessage, preStatement, preStatementText')

-- | Generic handler for a statement whose RHS is a localizable atom.
-- with the ability to transform the localization key
withLocAtom' :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> (Text -> Text) -> StatementHandler g m
withLocAtom' msg xform [pdx| %_ = ?key |]
    = msgToPP . msg =<< getGameL10n (xform key)
withLocAtom' _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom.
withLocAtom :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage)
    -> GenericStatement -> PPT g m IndentedMessages
withLocAtom msg = withLocAtom' msg id

-- | As 'withLocAtom', localizing the @_name@ key of the atom given.
withLocAtomName :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
withLocAtomName msg = withLocAtom' msg (<> "_name")

-- | Generic handler for a statement whose RHS is an atom run through the given
-- monadic lookup: a character's name, a wiki link, an organization's name.
withLookupAtom :: (HOI4Info g, Monad m) =>
    (Text -> PPT g m Text) -> (Text -> ScriptMessage) -> StatementHandler g m
withLookupAtom look msg [pdx| %_ = ?key |] = msgToPP . msg =<< look key
withLookupAtom _ _ stmt = preStatement stmt

-- | As 'withLookupAtom', passing the raw key to the message as well.
withLookupAtomKey :: (HOI4Info g, Monad m) =>
    (Text -> PPT g m Text) -> (Text -> Text -> ScriptMessage) -> StatementHandler g m
withLookupAtomKey look msg [pdx| %_ = ?key |] = do
    loc <- look key
    msgToPP $ msg loc key
withLookupAtomKey _ _ stmt = preStatement stmt

withLocAtomCompound :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomCompound msg stmt@[pdx| %_ = %rhs |] = case rhs of
    CompoundRhs [scr] -> withLocAtom msg scr
    _ -> preStatement stmt
withLocAtomCompound _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom.
-- with the ability to transform the localization key and also need the key itself
withLocAtomKey' :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -> (Text -> Text) -> StatementHandler g m
withLocAtomKey' msg xform [pdx| %_ = ?key |]
    = msgToPP . msg key =<< getGameL10n (xform key)
withLocAtomKey' _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom.
-- and need to use the
withLocAtomKey :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage)
    -> GenericStatement -> PPT g m IndentedMessages
withLocAtomKey msg = withLocAtomKey' msg id

-- | Generic handler for a statement whose RHS is a localizable atom, where we
-- also need an icon.
withLocAtomAndIcon :: (HOI4Info g, Monad m) =>
    Text -- ^ icon name - see
         -- <https://www.hoi4wiki.com/Template:Icon Template:Icon> on the wiki
        -> Bool
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomAndIcon iconkey _ msg stmt@[pdx| %_ = $vartag:$var |] = do
    mtagloc <- tagged vartag var
    case mtagloc of
        Just tagloc -> msgToPP $ msg (iconText iconkey) tagloc
        Nothing -> preStatement stmt
withLocAtomAndIcon iconkey lockey msg [pdx| %_ = ?key |]
    = do what <- Doc.doc2text <$> allowPronoun Nothing (fmap Doc.strictText . getGameL10n) key
         -- The icon template reads its key either way round, and asked for the
         -- term as well it writes it with the capitals the key was given, so a
         -- localized name goes in as it is written rather than folded down.
         lociconkey <- if lockey then getGameL10n iconkey else return iconkey
         msgToPP $ msg (iconText lociconkey) what
withLocAtomAndIcon _ _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom that
-- corresponds to an icon.
withLocAtomIcon :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage)
        -> Bool
        -> StatementHandler g m
withLocAtomIcon msg lockey stmt@[pdx| %_ = ?key |]
    = withLocAtomAndIcon key lockey msg stmt
withLocAtomIcon _ _ stmt = preStatement stmt

withState :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withState msg stmt@[pdx| %lhs = $vartag:$var |] = do
    mtagloc <- eGetState (Right (vartag, var))
    case mtagloc of
        Just tagloc -> msgToPP $ msg tagloc
        Nothing -> preStatement stmt
withState msg stmt@[pdx| %lhs = $var |] = do
    mtagloc <- eGetState (Left var)
    case mtagloc of
        Just tagloc -> msgToPP $ msg tagloc
        Nothing -> preStatement stmt
withState msg [pdx| %lhs = !stateid |]
    = msgToPP . msg =<< getStateLoc stateid
withState _ stmt = preStatement stmt

-- As withLocAtom but no l10n.
withNonlocAtom :: (HOI4Info g, Monad m) => (Text -> ScriptMessage) -> StatementHandler g m
withNonlocAtom msg [pdx| %_ = ?text |] = msgToPP $ msg text
withNonlocAtom _ stmt = preStatement stmt

-- TODO (if necessary): allow operators other than = and pass them to message
-- handler
-- | Handler for numeric statements.
numeric :: (IsGameState (GameState g), Monad m) =>
    (Double -> ScriptMessage)
        -> StatementHandler g m
numeric msg [pdx| %_ = !n |] = msgToPP $ msg n
numeric _ stmt = plainMsg $ preStatementText' stmt

-- | As 'numeric', for statements script also writes with a variable holding the
-- number instead of the number itself.
numericOrVar :: (HOI4Info g, Monad m) =>
    (Double -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
numericOrVar msg _ [pdx| %_ = !n |] = msgToPP $ msg n
numericOrVar _ msgvar [pdx| %_ = $vartag:$var |] = msgToPP $ msgvar (vartag <> ":" <> var)
numericOrVar _ msgvar [pdx| %_ = $var |] = msgToPP $ msgvar var
numericOrVar _ _ stmt = plainMsg $ preStatementText' stmt

-- | Handler for numeric compare statements.
numericCompare :: (HOI4Info g, Monad m) =>
    Text -> Text ->
    (Double -> Text -> ScriptMessage) ->
    (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericCompare gt lt msg msgvar stmt@[pdx| %_ = %num |] = case num of
    (floatRhs -> Just n) -> msgToPP $ msg n $ "equal to or " <> gt
    GenericRhs n [] -> msgToPP $ msgvar n $ "equal to or " <> gt
    GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n $ "equal to or " <> gt
    _ -> warn (BadValue "numeric comparison" stmt) $ preStatement stmt
numericCompare gt lt msg msgvar stmt@[pdx| %_ > %num |] = case num of
    (floatRhs -> Just n) -> msgToPP $ msg n gt
    GenericRhs n [] -> msgToPP $ msgvar n gt
    GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n gt
    _ -> warn (BadValue "numeric comparison" stmt) $ preStatement stmt
numericCompare gt lt msg msgvar stmt@[pdx| %_ < %num |] = case num of
    (floatRhs -> Just n) -> msgToPP $ msg n lt
    GenericRhs n [] -> msgToPP $ msgvar n lt
    GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n lt
    _ -> warn (BadValue "numeric comparison" stmt) $ preStatement stmt
numericCompare _ _ _ _ stmt = preStatement stmt

numericCompareCompound :: (HOI4Info g, Monad m) =>
    Text -> Text ->
    (Double -> Text -> ScriptMessage) ->
    (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericCompareCompound gt lt msg msgvar stmt@[pdx| %_ = %rhs |] = case rhs of
    CompoundRhs [scr] -> numericCompare gt lt msg msgvar scr
    _ -> preStatement stmt
numericCompareCompound _ _ _ _ stmt = preStatement stmt

numericCompareLoc :: (HOI4Info g, Monad m) =>
    Text -> Text ->
    (Double -> Text -> Text -> ScriptMessage) ->
    (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericCompareLoc gt lt msg msgvar stmt@[pdx| $txt = %num |] = do
    loc <- getGameL10n txt
    case num of
        (floatRhs -> Just n) -> msgToPP $ msg n ("equal to or " <> gt) loc
        GenericRhs n [] -> msgToPP $ msgvar n ("equal to or " <> gt) loc
        GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n ("equal to or " <> gt) loc
        _ -> warn (BadValue "numeric comparison" stmt) $ preStatement stmt
numericCompareLoc gt lt msg msgvar stmt@[pdx| $txt > %num |] = do
    loc <- getGameL10n txt
    case num of
        (floatRhs -> Just n) -> msgToPP $ msg n gt loc
        GenericRhs n [] -> msgToPP $ msgvar n gt loc
        GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n gt loc
        _ -> warn (BadValue "numeric comparison" stmt) $ preStatement stmt
numericCompareLoc gt lt msg msgvar stmt@[pdx| $txt < %num |] = do
    loc <- getGameL10n txt
    case num of
        (floatRhs -> Just n) -> msgToPP $ msg n lt loc
        GenericRhs n [] -> msgToPP $ msgvar n lt loc
        GenericRhs nt [nv] -> let n = nt <> nv in msgToPP $ msgvar n lt loc
        _ -> warn (BadValue "numeric comparison" stmt) $ preStatement stmt
numericCompareLoc _ _ _ _ stmt = preStatement stmt

numericCompareCompoundLoc :: (HOI4Info g, Monad m) =>
    Text -> Text ->
    (Double -> Text -> Text -> ScriptMessage) ->
    (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericCompareCompoundLoc gt lt msg msgvar stmt@[pdx| %_ = %rhs |] = case rhs of
    -- @show_current@ only asks the game to put the amount held into the
    -- tooltip, and says nothing about what is being compared. What is left is
    -- one comparison per line, each said in turn.
    CompoundRhs scr -> case snd (extractStmt (matchLhsText "show_current") scr) of
        [] -> preStatement stmt
        comparisons -> concat <$> traverse (numericCompareLoc gt lt msg msgvar) comparisons
    _ -> preStatement stmt
numericCompareCompoundLoc _ _ _ _ stmt = preStatement stmt

-- | Handler for a statement referring to a country. Use a flag.
withFlag :: (HOI4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage)  -> StatementHandler g m
withFlag msg stmt@[pdx| %_ = $vartag:$var |] = do
    mwhoflag <- eflag (Just HOI4Country) (Right (vartag, var))
    case mwhoflag of
        Just whoflag -> msgToPP $ msg whoflag ""
        Nothing -> preStatement stmt
withFlag msg [pdx| %_ = $who |] = do
    whoflag <- flagText (Just HOI4Country) who
    msgToPP $ msg whoflag who
withFlag msg [pdx| %_ = ?who |] = do
    whoflag <- flagText (Just HOI4Country) who
    msgToPP $ msg whoflag who
withFlag _ stmt = preStatement stmt

-- | Handler for yes-or-no statements.
withBool :: (HOI4Info g, Monad m) =>
    (Bool -> ScriptMessage)
        -> StatementHandler g m
withBool msg stmt = do
    fullmsg <- withBool' msg stmt
    maybe (preStatement stmt)
          return
          fullmsg

withBoolHOI4Scope :: (HOI4Info g, Monad m) =>
    (Bool -> ScriptMessage) -- ^ Message for country scope
        -> (Bool -> ScriptMessage) -- ^ Message for character scope
        -> StatementHandler g m
withBoolHOI4Scope countrymsg charactermsg stmt = do
    thescope <- getCurrentScope
    case thescope of
        Just HOI4Country -> withBool countrymsg stmt
        _ -> withBool charactermsg stmt

-- | Helper for 'withBool'.
withBool' :: (HOI4Info g, Monad m) =>
    (Bool -> ScriptMessage)
        -> GenericStatement
        -> PPT g m (Maybe IndentedMessages)
withBool' msg [pdx| %_ = ?yn |] | T.map toLower yn `elem` ["yes","no","false"]
    = fmap Just . msgToPP $ case T.toCaseFold yn of
        "yes" -> msg True
        "no"  -> msg False
        "false" -> msg False
        _     -> error "impossible: withBool matched a string that wasn't yes, no or false"
withBool' _ _ = return Nothing

-- | Handler for statements whose RHS may be "yes"/"no" or a tag.
withFlagOrBool :: (HOI4Info g, Monad m) =>
    (Bool -> ScriptMessage)
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withFlagOrBool bmsg _ [pdx| %_ = yes |] = msgToPP (bmsg True)
withFlagOrBool bmsg _ [pdx| %_ = no  |]  = msgToPP (bmsg False)
withFlagOrBool _ tmsg stmt = withFlag tmsg stmt

-- | Handler for statements that have a number and an icon.
numericIcon :: (IsGameState (GameState g), Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericIcon the_icon msg _ [pdx| %_ = !amt |]
    = msgToPP $ msg (iconText the_icon) amt
numericIcon the_icon _ msgvar [pdx| %_ = $amt |]
    = msgToPP $ msgvar (iconText the_icon) amt
numericIcon the_icon _ msgvar [pdx| %_ = $amttag:$amt |]
    = msgToPP $ msgvar (iconText the_icon) (amttag <> ":" <> amt)
numericIcon _ _ _ stmt = plainMsg $ preStatementText' stmt

-- | Handler for statements that have a number and an icon, plus a fixed
-- localizable atom.
numericIconLoc :: (HOI4Info g, Monad m) =>
    Text
        -> Text
        -> (Text -> Text -> Double -> ScriptMessage)
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericIconLoc the_icon what msg _ [pdx| %_ = !amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msg (iconText the_icon) whatloc amt
numericIconLoc the_icon what _ msgvar [pdx| %_ = $amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msgvar (iconText the_icon) whatloc amt
numericIconLoc the_icon what _ msgvar [pdx| %_ = $amttag:$amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msgvar (iconText the_icon) whatloc (amttag <> ":" <> amt)
numericIconLoc _ _ _ _ stmt = plainMsg $ preStatementText' stmt

-- | Handler for statements that have a number and a localizable atom.
numericLoc :: (HOI4Info g, Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> StatementHandler g m
numericLoc what msg [pdx| %_ = !amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msg whatloc amt
numericLoc _ _  stmt = plainMsg $ preStatementText' stmt

----------------------
-- Text/value pairs --
----------------------

-- $textvalue
-- This is for statements of the form
--      head = {
--          what = some_atom
--          value = 3
--      }
-- e.g.
--      num_of_religion = {
--          religion = catholic
--          value = 0.5
--      }
-- There are several statements of this form, but with different "what" and
-- "value" labels, so the first two parameters say what those label are.
--
-- There are two message parameters, one for value < 1 and one for value >= 1.
-- In the example num_of_religion, value is interpreted as a percentage of
-- provinces if less than 1, or a number of provinces otherwise. These require
-- rather different messages.
--
-- We additionally attempt to localize the RHS of "what". If it has no
-- localization string, it gets wrapped in a @<tt>@ element instead.

data TextValue = TextValue
        {   tv_what :: Maybe Text
        ,   tv_value :: Maybe Double
        ,   tv_var :: Maybe Text
        }

newTV :: TextValue
newTV = TextValue Nothing Nothing Nothing

parseTV :: Foldable t => Text -> Text -> t GenericStatement -> TextValue
parseTV whatlabel vallabel = foldl' addLine newTV
    where
        addLine :: TextValue -> GenericStatement -> TextValue
        addLine tv [pdx| $label = ?what |] | sameKey label whatlabel
            = tv { tv_what = Just what }
        addLine tv [pdx| $label = !val |] | sameKey label vallabel
            = tv { tv_value = Just val }
        addLine tv [pdx| $label = $vartag:$val |] | sameKey label vallabel
            = tv { tv_var = Just (vartag <> ":" <> val) }
        addLine tv [pdx| $label = $val |] | sameKey label vallabel
            = tv { tv_var = Just val }
        addLine nor stmt = warn (UnknownSection ("parseTV " <> whatlabel) stmt) nor

data TextValueComp = TextValueComp
        {   tvc_what :: Maybe Text
        ,   tvc_value :: Maybe Double
        ,   tvc_var :: Maybe Text
        ,   tvc_comp :: Maybe Text
        }

newTVC :: TextValueComp
newTVC = TextValueComp Nothing Nothing Nothing Nothing

parseTVC :: Foldable t =>
    Text -> Text -> Text -> Text -> t GenericStatement -> TextValueComp
parseTVC whatlabel vallabel gt lt = foldl' addLine newTVC
    where
        addLine :: TextValueComp -> GenericStatement -> TextValueComp
        addLine tvc [pdx| $label = ?what |] | sameKey label whatlabel
            = tvc { tvc_what = Just what }
        addLine tvc [pdx| $label = !val |] | sameKey label vallabel
            = tvc { tvc_value = Just val, tvc_comp = Just ("equal to or " <> gt) }
        addLine tvc [pdx| $label > !val |] | sameKey label vallabel
            = tvc { tvc_value = Just val, tvc_comp = Just gt }
        addLine tvc [pdx| $label < !val |] | sameKey label vallabel
            = tvc { tvc_value = Just val, tvc_comp = Just lt  }
        addLine tvc [pdx| $label = $vartag:$val |] | sameKey label vallabel
            = tvc { tvc_var = Just (vartag <> ":" <> val), tvc_comp = Just ("equal to or " <> gt) }
        addLine tvc [pdx| $label > $vartag:$val |] | sameKey label vallabel
            = tvc { tvc_var = Just (vartag <> ":" <> val), tvc_comp = Just gt }
        addLine tvc [pdx| $label < $vartag:$val |] | sameKey label vallabel
            = tvc { tvc_var = Just (vartag <> ":" <> val), tvc_comp = Just lt }
        addLine tvc [pdx| $label = $val |] | label == vallabel
            = tvc { tvc_var = Just val, tvc_comp = Just ("equal to or " <> gt) }
        addLine tvc [pdx| $label > $val |] | label == vallabel
            = tvc { tvc_var = Just val, tvc_comp = Just gt }
        addLine tvc [pdx| $label < $val |] | label == vallabel
            = tvc { tvc_var = Just val, tvc_comp = Just lt  }
        addLine nor stmt = warn (UnknownSection ("parseTVC " <> whatlabel) stmt) nor

textValue :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value < 1
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> (Text -> PPT g m (Text, Text)) -- ^ Action to localize and get icon (applied to RHS of "what")
        -> StatementHandler g m
textValue whatlabel vallabel valmsg varmsg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (parseTV whatlabel vallabel scr)
    where
        pp_tv :: TextValue -> PPT g m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv, tv_var tv) of
            (Just what, Just value, _) -> do
                (what_icon, what_loc) <- loc what
                return $ valmsg what_icon what_loc value
            (Just what, _, Just var) -> do
                (what_icon, what_loc) <- loc what
                return $ varmsg what_icon what_loc var
            _ -> return $ preMessage stmt
textValue _ _ _ _ _ stmt = preStatement stmt

textValueKey :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, for value
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor, for var
        -> StatementHandler g m
textValueKey whatlabel vallabel valmsg varmsg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (parseTV whatlabel vallabel scr)
    where
        pp_tv :: TextValue -> PPT g m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv, tv_var tv) of
            (Just what, Just value,_) -> do
                what_loc <- getGameL10n what
                return $ valmsg what_loc what value
            (Just what, _, Just var) -> do
                what_loc <- getGameL10n what
                return $ varmsg what_loc what var
            _ -> return $ preMessage stmt
textValueKey _ _ _ _ stmt = preStatement stmt

textValueCompare :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> Text
        -> Text
        -> (Text -> Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, for val
        -> (Text -> Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor, for var
        -> (Text -> PPT g m (Text, Text)) -- ^ Action to localize and get icon (applied to RHS of "what")
        -> StatementHandler g m
textValueCompare whatlabel vallabel gt lt valmsg varmsg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (parseTVC whatlabel vallabel gt lt scr)
    where
        pp_tv :: TextValueComp -> PPT g m ScriptMessage
        pp_tv tvc = case (tvc_what tvc, tvc_value tvc, tvc_var tvc, tvc_comp tvc) of
            (Just what, Just value, _, Just comp) -> do
                (what_icon, what_loc) <- loc what
                return $ valmsg what_icon what_loc comp value
            (Just what, _, Just var, Just comp) -> do
                (what_icon, what_loc) <- loc what
                return $ varmsg what_icon what_loc comp var
            _ -> return $ preMessage stmt
textValueCompare _ _ _ _ _ _ _ stmt = preStatement stmt

withNonlocTextValue :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> ScriptMessage                             -- ^ submessage to send
        -> (Text -> Text -> Double -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> StatementHandler g m
withNonlocTextValue whatlabel vallabel submsg valmsg varmsg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (parseTV whatlabel vallabel scr)
    where
        pp_tv :: TextValue -> PPT g m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv, tv_var tv) of
            (Just what, Just value, _) -> do
                mloc <- getGameL10nIfPresent what
                let loc = fromMaybe "" mloc
                extratext <- messageText submsg
                return $ valmsg extratext what value loc
            (Just what, _, Just var) -> do
                mloc <- getGameL10nIfPresent what
                let loc = fromMaybe "" mloc
                extratext <- messageText submsg
                return $ varmsg extratext what var loc
            _ -> return $ preMessage stmt
withNonlocTextValue _ _ _ _ _ stmt = preStatement stmt

data ValueValue = ValueValue
        {   vv_what :: Maybe Double
        ,   vv_value :: Maybe Double
        ,   vv_var :: Maybe Text
        }

newVV :: ValueValue
newVV = ValueValue Nothing Nothing Nothing

parseVV :: Foldable t => Text -> Text -> t GenericStatement -> ValueValue
parseVV whatlabel vallabel = foldl' addLine newVV
    where
        addLine :: ValueValue -> GenericStatement -> ValueValue
        addLine vv [pdx| $label = !what |] | label == whatlabel
            = vv { vv_what = Just what }
        addLine vv [pdx| $label = !val |] | label == vallabel
            = vv { vv_value = Just val }
        addLine vv [pdx| $label = !val |] | label == vallabel
            = vv { vv_value = Just val }
        addLine nor stmt = warn (UnknownSection ("parseVV " <> whatlabel) stmt) nor

valueValue :: forall g m. (HOI4Info g, Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Double -> Double -> ScriptMessage) -- ^ Message constructor, if abs value < 1
        -> (Double -> Text -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> StatementHandler g m
valueValue whatlabel vallabel valmsg varmsg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_vv (parseVV whatlabel vallabel scr)
    where
        pp_vv :: ValueValue -> PPT g m ScriptMessage
        pp_vv vv = case (vv_what vv, vv_value vv, vv_var vv) of
            (Just what, Just value, _) ->
                return $ valmsg what value
            (Just what, _, Just var) ->
                return $ varmsg what var
            _ -> return $ preMessage stmt
valueValue _ _ _ _ stmt = preStatement stmt

-- | Handler for statements whose right-hand side is a province id, shown with
-- its victory point name where it has one.
withProvince :: forall g m. (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
withProvince msg [pdx| %_ = !provid |]
    = msgToPP . msg =<< getProvinceLoc provid
withProvince _ stmt = preStatement stmt

-- | Statements of the form
-- @
--      has_trade_modifier = {
--          who = ROOT
--          name = merchant_recalled
--      }
-- @
data TextAtom = TextAtom
        {   ta_what :: Maybe Text
        ,   ta_atom :: Maybe Text
        }

newTA :: TextAtom
newTA = TextAtom Nothing Nothing

parseTA :: Foldable t => Text -> Text -> t GenericStatement -> TextAtom
parseTA whatlabel atomlabel scr = foldl' addLine newTA scr
    where
        addLine :: TextAtom -> GenericStatement -> TextAtom
        addLine ta [pdx| $label = ?what |]
            | sameKey label whatlabel
            = ta { ta_what = Just what }
        addLine ta [pdx| $label = ?at |]
            | sameKey label atomlabel
            = ta { ta_atom = Just at }
        addLine ta scr = warn (UnknownSection "parseTA" scr) ta

textAtom :: forall g m. (HOI4Info g, Monad m) =>
    Text -- ^ Label for "what" (e.g. "who")
        -> Text -- ^ Label for atom (e.g. "name")
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> PPT g m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> StatementHandler g m
textAtom whatlabel atomlabel msg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_ta (parseTA whatlabel atomlabel scr)
    where
        pp_ta :: TextAtom -> PPT g m ScriptMessage
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                mwhat_loc <- loc what
                atom_loc <- getGameL10n atom
                let what_icon = iconText what
                    what_loc = fromMaybe (typewriterText what) mwhat_loc
                return $ msg what_icon what_loc atom_loc
            _ -> return $ preMessage stmt
textAtom _ _ _ _ stmt = preStatement stmt

textAtomKey :: forall g m. (HOI4Info g, Monad m) =>
    Text -- ^ Label for "what" (e.g. "who")
        -> Text -- ^ Label for atom (e.g. "name")
        -> (Text -> Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> PPT g m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> StatementHandler g m
textAtomKey whatlabel atomlabel msg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_ta (parseTA whatlabel atomlabel scr)
    where
        pp_ta :: TextAtom -> PPT g m ScriptMessage
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                mwhat_loc <- loc what
                atom_loc <- getGameL10n atom
                let what_loc = fromMaybe (typewriterText what) mwhat_loc
                return $ msg what_loc atom_loc what atom
            _ -> return $ preMessage stmt
textAtomKey _ _ _ _ stmt = preStatement stmt

data TextFlag = TextFlag
        {   tf_what :: Maybe Text
        ,   tf_flag :: Maybe (Either Text (Text, Text))
        }

newTF :: TextFlag
newTF = TextFlag Nothing Nothing

parseTF :: Foldable t => Text -> Text -> t GenericStatement -> TextFlag
parseTF whatlabel flaglabel scr = foldl' addLine newTF scr
    where
        addLine :: TextFlag -> GenericStatement -> TextFlag
        addLine tf [pdx| $label = ?what |]
            | label == whatlabel
            = tf { tf_what = Just what }
        addLine tf [pdx| $label = $target |]
            | label == flaglabel
            = tf { tf_flag = Just (Left target) }
        addLine tf [pdx| $label = $vartag:$var |]
            | label == flaglabel
            = tf { tf_flag = Just (Right (vartag, var)) }
        addLine tf scr = warn (UnknownSection "parseTF" scr) tf

taTypeFlag :: forall g m. (HOI4Info g, Monad m) => Text -> Text -> (Text -> Text -> ScriptMessage) -> StatementHandler g m
taTypeFlag tType tFlag msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tf (parseTF tType tFlag scr)
    where
        pp_tf :: TextFlag -> PPT g m ScriptMessage
        pp_tf tf = case (tf_what tf, tf_flag tf) of
            (Just typ, Just flag) -> do
                typeLoc <- getGameL10n typ
                flagLoc <- eflag (Just HOI4Country) flag
                case flagLoc of
                   Just flagLocd -> return $ msg typeLoc flagLocd
                   _-> return $ preMessage stmt
            _ -> return $ preMessage stmt
taTypeFlag _ _ _ stmt = preStatement stmt

-- | Helper for effects, where the argument is a single statement in a clause
-- E.g. generate_traitor_advisor_effect

getEffectArg :: Text -> GenericStatement -> Maybe GenericRhs
getEffectArg tArg stmt@[pdx| %_ = @scr |] = case scr of
        [[pdx| $arg = %val |]] | T.toLower arg == tArg -> Just val
        _ -> Nothing
getEffectArg _ _ = Nothing

simpleEffectNum :: forall g m. (HOI4Info g, Monad m) => Text ->  (Double -> ScriptMessage) -> StatementHandler g m
simpleEffectNum tArg msg stmt =
    case getEffectArg tArg stmt of
        Just (FloatRhs num) -> msgToPP (msg num)
        Just (IntRhs num) -> msgToPP (msg (fromIntegral num))
        _ -> warn (UnknownSection "simpleEffectNum" stmt) $ preStatement stmt

simpleEffectAtom :: forall g m. (HOI4Info g, Monad m) => Text -> (Text -> Text -> ScriptMessage) -> StatementHandler g m
simpleEffectAtom tArg msg stmt =
    case getEffectArg tArg stmt of
        Just (GenericRhs atom _) -> do
            loc <- getGameL10n atom
            msgToPP $ msg (iconText atom) loc
        _ -> warn (UnknownSection "simpleEffectAtom" stmt) $ preStatement stmt

------------------------------------------
-- handlers for various flag statements --
------------------------------------------
withMaybelocAtom2 :: (HOI4Info g, Monad m) =>
    ScriptMessage
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withMaybelocAtom2 submsg msg [pdx| %_ = ?txt |] = do
    mloc <- getGameL10nIfPresent txt
    let loc = fromMaybe "" mloc
    extratext <- messageText submsg
    msgToPP $ msg extratext txt loc
withMaybelocAtom2 _ _ stmt = preStatement stmt

------ region handler -----

withRegion :: (HOI4Info g, Monad m) =>
        StatementHandler g m
withRegion stmt@[pdx| %lhs = $vartag:$var |] = do
    mtagloc <- tagged vartag var
    case mtagloc of
        Just tagloc -> msgToPP $ MsgRegion tagloc
        Nothing -> preStatement stmt
withRegion stmt@[pdx| %lhs = $var |]
    = msgToPP $ MsgRegion (typewriterText var)
withRegion [pdx| %lhs = !stateid |]
    = msgToPP . MsgRegion =<< getRegionLoc stateid
withRegion stmt = preStatement stmt
