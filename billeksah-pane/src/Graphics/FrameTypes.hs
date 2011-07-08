{-# Language ExistentialQuantification, TypeFamilies, MultiParamTypeClasses,
    DeriveDataTypeable, FlexibleInstances, StandaloneDeriving #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.FrameTypes
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Splittable panes containing notebooks with any widgets
--
---------------------------------------------------------------------------------

module Graphics.FrameTypes (
    triggerFrameEvent
,   FrameEvent(..)
,   ActionDescr(..)
,   ActionType(..)
,   MenuPosition(..)
,   ToolPosition(..)
,   FrameSelector(..)
,   panePluginName
,   menuBarStateName
,   registerFrameEvent
) where

import Base

import Graphics.UI.Gtk (UIManager, MenuItem)
import Data.Typeable
       (TypeRep(..), typeRepKey, cast, Typeable(..))
import Base.State (StateM)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

panePluginName = "billeksah-pane"
menuBarStateName = panePluginName ++ ".menuBar"

-- ----------------------------------
-- * Events
--

--
-- | Events the gui frame triggers
--
data FrameEvent =
      ActivatePane String
    | DeactivatePane String
    | MovePane String
    | ChangeLayout
    | RegisterActions [ActionDescr]
        deriving Typeable

-- | ActionDescr is a data structure used for
--   menus, toolbars, and accelerator keystrokes. In this implementation
--   GtkActions are build from this description
data ActionDescr = AD {
    adName        ::   String -- ^ has to be unique, so allways prepend the plugin name,
                            -- seperated with a dot.
,   adLabel       ::   String -- ^ what is displayed in the menu
,   adSynopsis    ::   Maybe String   -- ^ maybe a text to display in a tooltip
,   adStockID     ::   Maybe String   -- ^ maybe a text for a gtk standard action
,   adAction      ::   StateM ()      -- ^ the action to perform
    -- which then may show a special icon, ...
,   adAccelerator ::   Maybe String   -- ^ Keyboard accelerator
        -- ^ The format looks like "<Control>a" or "<Shift><Alt>F1" or "<Release>z"
,   adActionType  ::   ActionType
,   adMenu        ::   MenuPosition
,   adToolbar     ::   ToolPosition
,   adSensitivities :: [GenSelector]
}


data FrameSelector = FrameEventSel | PaneActiveSens | FrameStateSel | ActionStateSel
    deriving (Eq, Ord, Show, Typeable)

instance Selector FrameSelector

type WithSeparator = Bool

data MenuPosition =
    MPFirst [String]                      -- ^ Add this item in the first position using path.
    | MPLast [String] WithSeparator       -- ^ Add this item in the last position using path.
    | MPAfter [String]
        WithSeparator                     -- ^ Add this item after the first arg string
                                          --   If the Bool is true add a separator between.
    | MPBefore [String]            -- ^ Add this item before the first arg string
                                          --   in the last position using path (second arg).
    | MPAppend WithSeparator              -- ^ Append this after the last added item.
                                          --   If the Bool is true add a separator between.
    | MPOr MenuPosition MenuPosition      -- ^ Try the first position.
                                          --   If this fails try the next.
    | MPNo                                -- ^ This action doesn't appear in a menu
    deriving Eq

data ToolPosition =
    TPFirst                             -- ^ Add this item in the first position.
    | TPLast WithSeparator              -- ^ Add this item in the last position.
    | TPAfter String WithSeparator      -- ^ Add this item after the first arg string
    | TPBefore String                   -- ^ Add this item before the first arg string
                                        --   in the last position using path (second arg).
    | TPAppend WithSeparator            -- ^ Append this after the last added item.
    | TPOr ToolPosition ToolPosition    -- ^ Try the first position.
                                        --   If this fails try the next.
    | TPNo                              -- ^ This action doesn't appear in the toolbar
    deriving Eq

-- | Beside Standard action we have actions which toggles a state or select from a
-- number of possible states
data ActionType = ActionNormal | ActionToggle | ActionSubmenu -- TODO ActionSelect alpha

    deriving Eq

type FramePrefs = UIManager

makeFrameEvent :: StateM(PEvent FrameEvent)
makeFrameEvent = makeEvent FrameEventSel

triggerFrameEvent :: FrameEvent -> StateM(FrameEvent)
triggerFrameEvent          = triggerEvent FrameEventSel

getFrameEvent :: StateM (PEvent FrameEvent)
getFrameEvent              = getEvent FrameEventSel

registerFrameEvent :: Handler FrameEvent -> StateM HandlerID
registerFrameEvent handler = getFrameEvent >>= \e -> registerEvent e handler
