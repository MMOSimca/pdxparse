module HOI4.Common (
        ppScript
    ,   ppMany
    ,   ppOne
    ) where

import Abstract (GenericScript, GenericStatement)
import Doc (Doc)
import HOI4.Messages (IndentedMessages, StatementHandler)
import SettingsTypes (PPT)
import HOI4.Types (HOI4Info)
import Data.Text (Text)

ppScript :: (HOI4Info g, Monad m) => GenericScript -> PPT g m Doc
ppMany :: (HOI4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppOne :: (HOI4Info g, Monad m) => StatementHandler g m
