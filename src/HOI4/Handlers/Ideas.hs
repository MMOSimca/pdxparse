{-|
Module      : HOI4.Handlers.Ideas
Description : Ideas

Handlers for adding, removing, swapping and timing ideas, and for writing
out what an idea is.
-}
module HOI4.Handlers.Ideas (
        amountTakenIdeas
    ,   handleSwapIdeas
    ,   handleTimedIdeas
    ,   handleIdeas
    ,   showIdea
    ,   showIdeaUnderHeading
    ) where

import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative ((<|>))

import Abstract -- everything
import qualified Doc -- everything
import MessageTools (typewriterText)
import ParseWarnings (ParseWarning (..), warn)
import QQ -- everything
import SettingsTypes (PPT, indentUp, getCurrentIndent, withCurrentIndent
                     , withCurrentIndentCustom, concatMapM, getGameInterfaceNamed
                     , getGameInterfaceIfPresent)

import {-# SOURCE #-} HOI4.Common (ppOne)
import HOI4.Localization
import HOI4.Messages -- everything
import HOI4.Types -- everything

import HOI4.Handlers.Core (getbaretraits, joinClauses, msgToPP, plainMsg', preStatement, thisContext)
import HOI4.Handlers.Generic (parseTA, TextAtom (..))
import HOI4.Handlers.Industry (mioTooltip)
import HOI4.Handlers.Characters (getLeaderTraits)
import HOI4.Handlers.Modifiers (handleModifier, handleResearchBonus, handleTargetedModifier)

-- | Handler for @amount_taken_ideas@, which counts how many of a country's idea
-- slots of a kind are filled. An advisor, a theorist and the rest are all ideas
-- as far as script is concerned, so this is how it asks how many of them a
-- country has taken on.
amountTakenIdeas :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
amountTakenIdeas stmt@[pdx| %_ = @scr |] = case foldl' addLine (Nothing, []) scr of
    (Just (comp, amt), slots@(_:_)) -> do
        slotlocs <- traverse getGameL10n slots
        msgToPP $ MsgAmountTakenIdeas comp amt (joinClauses slotlocs)
    _ -> preStatement stmt
    where
        addLine (amt, slots) [pdx| amount > !n |] = (Just ("more than", n), slots)
        addLine (amt, slots) [pdx| amount < !n |] = (Just ("fewer than", n), slots)
        addLine (amt, slots) [pdx| amount = !n |] = (Just ("exactly", n), slots)
        -- The slots are named one after another with nothing between them, or
        -- written out bare where there is only the one to name.
        addLine (amt, slots) [pdx| slots = @scr |] = (amt, slots ++ mapMaybe slotName scr)
        addLine (amt, slots) [pdx| slots = $slot |] = (amt, slots ++ [slot])
        addLine acc stmt = warn (UnknownSection "amount_taken_ideas" stmt) acc
        slotName (StatementBare (GenericLhs slot [])) = Just slot
        slotName _ = Nothing
amountTakenIdeas stmt = preStatement stmt

-----------------
-- handle idea --
-----------------

handleSwapIdeas :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleSwapIdeas stmt@[pdx| %_ = @scr |]
    = pp_si (parseTA "add_idea" "remove_idea" scr)
    where
        pp_si :: TextAtom -> PPT g m IndentedMessages
        pp_si ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                add_loc <- handleIdeaWithEffects True what
                remove_loc <- handleIdea False atom
                case (add_loc, remove_loc) of
                    (Just (addcategory, addideaIcon, addideaKey, addidea_loc, Just addeffectbox),
                     Just (category, ideaIcon, ideaKey, idea_loc, _)) ->
                        if addidea_loc == idea_loc then do
                                idmsg <- msgToPP $ MsgModifyIdea category ideaIcon ideaKey idea_loc
                                                    addcategory addideaIcon addideaKey addidea_loc
                                return $ idmsg ++ addeffectbox
                        else do
                            idmsg <- msgToPP $ MsgReplaceIdea category ideaIcon ideaKey idea_loc
                                                addcategory addideaIcon addideaKey addidea_loc
                            return $ idmsg ++ addeffectbox
                    (Just (addcategory, addideaIcon, addideaKey, addidea_loc, Nothing),
                     Just (category, ideaIcon, ideaKey, idea_loc, _)) ->
                        if addidea_loc == idea_loc then
                            msgToPP $ MsgModifyIdea category ideaIcon ideaKey idea_loc
                                addcategory addideaIcon addideaKey addidea_loc
                        else
                            msgToPP $ MsgReplaceIdea category ideaIcon ideaKey idea_loc
                                addcategory addideaIcon addideaKey addidea_loc
                    _ -> preStatement stmt
            _ -> preStatement stmt
handleSwapIdeas stmt = preStatement stmt

-- | Write out one idea from what 'handleIdea' found of it: the message, with
-- the idea's effect box under it where it has one. 'Nothing' falls back to the
-- raw statement.
ppIdeaMsg :: (HOI4Info g, Monad m) =>
    (Text -> Text -> Text -> Text -> ScriptMessage)
    -> GenericStatement
    -> Maybe (Text, Text, Text, Text, Maybe IndentedMessages)
    -> PPT g m IndentedMessages
ppIdeaMsg _ stmt Nothing = preStatement stmt
ppIdeaMsg msg _ (Just (category, ideaIcon, ideaKey, idea_loc, meffectbox)) = do
    idmsg <- msgToPP $ msg category ideaIcon ideaKey idea_loc
    return $ idmsg ++ fromMaybe [] meffectbox

-- | The unit a timed idea's run is counted in. Script gives it in any of the
-- three, and each is written out in the unit it was given in rather than turned
-- into another: a run of months is a run of months to the reader as much as to
-- the game.
data TimedIdeaUnit = InDays | InMonths | InYears

unitName :: TimedIdeaUnit -> Text
unitName InDays = "days"
unitName InMonths = "months"
unitName InYears = "years"

-- | What script says of a timed idea: which idea it is, how long it is held
-- for, and the unit that length is counted in. The length is either given
-- outright or held in a variable, which is named where the number would stand.
data TimedIdea = TimedIdea
    {   ti_idea :: Maybe Text
    ,   ti_length :: Maybe Double
    ,   ti_lengthvar :: Maybe Text
    ,   ti_unit :: TimedIdeaUnit
    }

newTI :: TimedIdea
newTI = TimedIdea Nothing Nothing Nothing InDays

-- | Handler for @add_timed_idea@ and @modify_timed_idea@.
handleTimedIdeas :: forall g m. (HOI4Info g, Monad m) =>
        (Text -> Text -> Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor for a run of days
        -> (Text -> Text -> Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor for a run of months
        -> (Text -> Text -> Text -> Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor for a run held in a variable, told the unit it is counted in
        -> StatementHandler g m
handleTimedIdeas msgdays msgmonths msgvar stmt@[pdx| %_ = @scr |]
    = case (ti_idea ti, mrun) of
        (Just what, Just msg) -> ppIdeaMsg msg stmt =<< handleIdea True what
        _ -> preStatement stmt
    where
        ti = foldl' addLine newTI scr
        addLine :: TimedIdea -> GenericStatement -> TimedIdea
        addLine t [pdx| idea = ?txt |] = t { ti_idea = Just txt }
        addLine t [pdx| days = !n |] = t { ti_length = Just n, ti_unit = InDays }
        addLine t [pdx| months = !n |] = t { ti_length = Just n, ti_unit = InMonths }
        addLine t [pdx| years = !n |] = t { ti_length = Just n, ti_unit = InYears }
        addLine t [pdx| days = ?txt |] = t { ti_lengthvar = Just txt, ti_unit = InDays }
        addLine t [pdx| months = ?txt |] = t { ti_lengthvar = Just txt, ti_unit = InMonths }
        addLine t [pdx| years = ?txt |] = t { ti_lengthvar = Just txt, ti_unit = InYears }
        addLine t stmt = warn (UnknownSection "timed idea" stmt) t
        -- A year is twelve months, which is the one turn between units that
        -- loses nothing. A run held in a variable is left in the unit it was
        -- given in, there being no number to turn.
        mrun = case (ti_length ti, ti_lengthvar ti) of
            (Just n, _) -> Just $ case ti_unit ti of
                InDays -> \c i k l -> msgdays c i k l n
                InMonths -> \c i k l -> msgmonths c i k l n
                InYears -> \c i k l -> msgmonths c i k l (n * 12)
            (_, Just var) -> Just (\c i k l -> msgvar c i k l var (unitName (ti_unit ti)))
            _ -> Nothing
handleTimedIdeas _ _ _ stmt = preStatement stmt

handleIdeas :: forall g m. (HOI4Info g, Monad m) =>
    Bool ->
    (Text -> Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
handleIdeas addIdea msg stmt@[pdx| $lhs = %idea |] = case idea of
    CompoundRhs ideas -> if length ideas == 1 then
                ppIdeaMsg msg stmt =<< handleIdea addIdea (mconcat $ map getbareidea ideas)
            else do
                ideashandle <- mapM (handleIdea addIdea . getbareidea) ideas
                let ideashandled = catMaybes ideashandle
                    ideasmsgd :: [(Text, Text, Text, Text, Maybe IndentedMessages)] -> PPT g m [IndentedMessages]
                    ideasmsgd ihs = mapM (\ih ->
                            let (category, ideaIcon, ideaKey, idea_loc, effectbox) = ih in
                            withCurrentIndent $ \i -> case effectbox of
                                    Just boxNS -> return ((i, msg category ideaIcon ideaKey idea_loc):boxNS)
                                    _-> return [(i, msg category ideaIcon ideaKey idea_loc)]
                                ) ihs
                ideasmsgdd <- ideasmsgd ideashandled
                return $ mconcat ideasmsgdd
    GenericRhs txt [] -> ppIdeaMsg msg stmt =<< handleIdea addIdea txt
    _ -> preStatement stmt
handleIdeas _ _ stmt = preStatement stmt

getbareidea :: GenericStatement -> Text
getbareidea (StatementBare (GenericLhs e [])) = e
-- Script also names the idea in a field of its own rather than writing it bare.
getbareidea [pdx| idea = $e |] = e
getbareidea _ = "<!-- Check Script -->"

handleIdea :: (HOI4Info g, Monad m) =>
        Bool -> Text ->
           PPT g m (Maybe (Text, Text, Text, Text, Maybe IndentedMessages))
handleIdea = handleIdea' False

-- | As 'handleIdea', for an idea named for what it grants rather than to say
-- which one it is. A national spirit says what it grants wherever it turns up,
-- while a designer or a company is usually only named -- but where one is
-- swapped for an improved version, what the new one grants is the whole of what
-- the swap does, so it is written out whatever kind of idea it is.
handleIdeaWithEffects :: (HOI4Info g, Monad m) =>
        Bool -> Text ->
           PPT g m (Maybe (Text, Text, Text, Text, Maybe IndentedMessages))
handleIdeaWithEffects = handleIdea' True

handleIdea' :: (HOI4Info g, Monad m) =>
        Bool -> Bool -> Text ->
           PPT g m (Maybe (Text, Text, Text, Text, Maybe IndentedMessages))
handleIdea' always addIdea ide = do
    ides <- getIdeas
    charto <- getCharToken
    -- Script writes an idea's name in whatever case it likes and the game finds
    -- it either way, so a miss is looked for again without regard for case.
    let ciLookup name table = HM.lookup name table <|>
            listToMaybe [v | (k, v) <- HM.toList table, T.toLower k == T.toLower name]
        midea = ciLookup ide ides
    case midea of
        Just iidea -> do
            let ideaKey = id_id iidea
                ideaname = id_name iidea
            ideaIcon <- getIdeaIcon iidea
            -- The idea's name and what it grants are drawn for the country
            -- gaining it, which is the current scope, whose own ROOT it is.
            idea_loc <- thisContext $ getGameL10n ideaname
            category <- if id_category iidea == "country" then getGameL10n "FE_COUNTRY_SPIRIT" else getGameL10n $ id_category iidea
            effectbox <- thisContext $ modmessage iidea idea_loc ideaKey ideaIcon
            effectboxNS <- if addIdea && (always || id_category iidea == "country")
                              then return $ Just effectbox else return Nothing
            -- The wiki has an icon of its own for each of the country's laws,
            -- keyed by the law's name, and shows a law by that rather than by
            -- the picture the game draws it with. Every other idea has no such
            -- icon and is shown by its picture.
            let shown | id_law iidea = iconText idea_loc
                      | T.null ideaIcon = ""
                      | otherwise = "[[File:" <> ideaIcon <> ".png|28px]]"
            return $ Just (category, shown, ideaKey, idea_loc, effectboxNS)
        Nothing -> case ciLookup ide charto of
            -- Not an idea of the files we read: a name built at run time out of
            -- variables, or one a mod adds. What the script calls it is what
            -- there is to name it by, and saying that much beats saying nothing.
            Nothing -> do
                category <- getGameL10n "FE_COUNTRY_SPIRIT"
                mloc <- getGameL10nIfPresent ide
                return $ Just (category, "", ide, fromMaybe (typewriterText ide) mloc, Nothing)
            Just cchat -> do
                let namekey = adv_cha_id cchat
                mloc <- getGameL10nIfPresent $ adv_cha_name cchat
                name_loc <- case mloc of
                    Just nloc -> return nloc
                    _ -> getGameL10n $ adv_idea_token cchat
                slot <- getGameL10n (adv_advisor_slot cchat)
                return $ Just (slot, "", namekey, name_loc, Nothing)

-- | The picture the game shows an idea with, falling back on the generic one.
getIdeaIcon :: (HOI4Info g, Monad m) => HOI4Idea -> PPT g m Text
getIdeaIcon iidea = do
    micon <- getGameInterfaceIfPresent ("GFX_idea_" <> id_id iidea)
    case micon of
        Nothing -> getGameInterfaceNamed (id_picture iidea)
        Just idicon -> return idicon

-- | What an idea named by a @show_ideas_tooltip@ grants. An idea in the idea
-- files gets the same effect box as one that is handed out for good; an advisor
-- has no such box, since their entry holds little beyond their name and the
-- trait that comes with the post.
showIdea :: (HOI4Info g, Monad m) => StatementHandler g m
showIdea = showIdeaWith id

-- | As 'showIdea', for an idea listed under a heading announcing its slot. An
-- effect box is a block in its own right, so it stays at the heading's level,
-- while everything shown as a list item belongs one level under it.
showIdeaUnderHeading :: (HOI4Info g, Monad m) => StatementHandler g m
showIdeaUnderHeading = showIdeaWith indentUp

showIdeaWith :: (HOI4Info g, Monad m) =>
    (PPT g m IndentedMessages -> PPT g m IndentedMessages) -> StatementHandler g m
showIdeaWith nest stmt@[pdx| %_ = $idea |] = do
    ides <- getIdeas
    charto <- getCharToken
    case HM.lookup idea ides of
        Just iidea -> do
            -- Drawn for the country whose idea it is: see 'handleIdea''.
            idea_loc <- thisContext $ getGameL10n (id_name iidea)
            ideaIcon <- getIdeaIcon iidea
            thisContext $ modmessage iidea idea_loc (id_id iidea) ideaIcon
        Nothing -> case HM.lookup idea charto of
            Just ccharto -> nest $ showAdvisor idea ccharto
            -- Script also points this at things that are not ideas at all, a
            -- military industrial organization most often. There is nothing to
            -- list for those, but they are localized, so they can at least be
            -- named rather than left as script.
            Nothing -> nest $ mioTooltip MsgShowMio stmt
showIdeaWith _ stmt = preStatement stmt

-- | Name an advisor, with the trait their post comes with, and list everything
-- the post grants under them. Which of the modifiers the trait rather than the
-- advisor's own entry supplies is of no interest on the wiki, so they are shown
-- as one list.
showAdvisor :: (HOI4Info g, Monad m) => Text -> HOI4Advisor -> PPT g m IndentedMessages
showAdvisor token adv = do
    let traits = fromMaybe [] (adv_traits adv)
    mloc <- getGameL10nIfPresent (adv_cha_name adv)
    name_loc <- maybe (getGameL10n (adv_idea_token adv)) return mloc
    -- Some trait names are laid out over two lines ("Armor\n(Expert)"), which
    -- would end the list item they are written into.
    traitlocs <- traverse (fmap Doc.oneLine . getGameL10n) traits
    basemsg <- msgToPP $ MsgShowAdvisor name_loc token (T.intercalate ", " traitlocs)
    bonusmsg <- indentUp $ do
        traitmsg <- concatMapM getLeaderTraits traits
        modmsg <- maybe (return []) handleModifier (adv_modifier adv)
        resmsg <- maybe (return []) handleResearchBonus (adv_research_bonus adv)
        return $ traitmsg ++ modmsg ++ resmsg
    return $ basemsg ++ bonusmsg

modmessage :: forall g m. (HOI4Info g, Monad m) => HOI4Idea -> Text -> Text -> Text -> PPT g m IndentedMessages
modmessage iidea idea_loc ideaKey ideaIcon = do
        curind <- getCurrentIndent
        curindent <- case curind of
            Just curindt -> return curindt
            _ -> return 1
        withCurrentIndentCustom 1 $ \_ -> do
            ideaDesc <- case id_desc_loc iidea of
                Just desc -> return $ Doc.nl2br desc
                _ -> return ""
            -- A company's or an advisor's trait names the kind of thing it is
            -- ("Railway Company"), which the box's own title already conveys;
            -- only a national spirit's traits are worth a heading of their own.
            let namedTraits = id_category iidea == "country"
            traitmsg <- case id_traits iidea of
                Just arr -> do
                    let traitbare = mapMaybe getbaretraits arr
                    concatMapM (\t-> if not namedTraits then getLeaderTraits t else do
                        traitloc <- Doc.oneLine <$> getGameL10n t
                        namemsg <- plainMsg' ("'''" <> traitloc <> "'''")
                        traitmsg <- indentUp $ getLeaderTraits t
                        return $ namemsg : traitmsg) traitbare
                _-> return []
            modifier <- maybe (return []) ppOne (id_modifier iidea)
            targeted_modifier <-
                maybe (return []) (concatMapM handleTargetedModifier) (id_targeted_modifier iidea)
            research_bonus <- maybe (return []) ppOne (id_research_bonus iidea)
            equipment_bonus <- maybe (return []) ppOne (id_equipment_bonus iidea)
            let boxend = [(0, MsgEffectBoxEnd curindent)]
            withCurrentIndentCustom curindent $ \_ -> do
                let ideamods = traitmsg ++modifier ++ targeted_modifier ++ research_bonus ++ equipment_bonus ++ boxend
                return $ (0, MsgEffectBox idea_loc ideaKey ideaIcon ideaDesc) : ideamods
