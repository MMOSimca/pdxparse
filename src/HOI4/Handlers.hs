{-|
Module      : HOI4.Handlers
Description : Every statement handler, gathered from the modules under it

The handlers are written in modules under this one, each for one part of
the game. This module re-exports them all so that the handler table in
"HOI4.Common" can name any of them.
-}
module HOI4.Handlers (
        module HOI4.Handlers.Core
    ,   module HOI4.Handlers.Generic
    ,   module HOI4.Handlers.Control
    ,   module HOI4.Handlers.Variables
    ,   module HOI4.Handlers.Weights
    ,   module HOI4.Handlers.Events
    ,   module HOI4.Handlers.Flags
    ,   module HOI4.Handlers.Politics
    ,   module HOI4.Handlers.Diplomacy
    ,   module HOI4.Handlers.Military
    ,   module HOI4.Handlers.Industry
    ,   module HOI4.Handlers.Research
    ,   module HOI4.Handlers.States
    ,   module HOI4.Handlers.Intelligence
    ,   module HOI4.Handlers.Characters
    ,   module HOI4.Handlers.Ideas
    ,   module HOI4.Handlers.Modifiers
    ,   module HOI4.Handlers.Chunks
    ,   module HOI4.Handlers.Tooltips
    ) where

import HOI4.Handlers.Core
import HOI4.Handlers.Generic
import HOI4.Handlers.Control
import HOI4.Handlers.Variables
import HOI4.Handlers.Weights
import HOI4.Handlers.Events
import HOI4.Handlers.Flags
import HOI4.Handlers.Politics
import HOI4.Handlers.Diplomacy
import HOI4.Handlers.Military
import HOI4.Handlers.Industry
import HOI4.Handlers.Research
import HOI4.Handlers.States
import HOI4.Handlers.Intelligence
import HOI4.Handlers.Characters
import HOI4.Handlers.Ideas
import HOI4.Handlers.Modifiers
import HOI4.Handlers.Chunks
import HOI4.Handlers.Tooltips
