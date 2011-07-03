{-# Language DeriveDataTypeable, RankNTypes #-}
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
,   Position(..)
,   ActionDescr(..)
,   ActionType(..)
,   pluginName
,   menuBarStateName
) where

import Base

import Graphics.UI.Gtk (UIManager, MenuItem)
import Data.Typeable (Typeable)
import Base.State (StateM)

pluginName = "billeksah-pane"
menuBarStateName = pluginName ++ ".menuBar"

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
    | RegisterMenu [(MenuItem,Position)]
        deriving Typeable

data Position = Last
-- TODO
--    | First
--    | Before [String] (Maybe Position)
--    | After [String] (Maybe Position)


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
,   adAccelerators ::   [String]
        -- ^ Keyboard accelerators
        -- ^ The format looks like "<Control>a" or "<Shift><Alt>F1" or "<Release>z"
,   adActionType  ::   forall alpha . Ord alpha => ActionType alpha
--,   sensitivity ::   SensitivityMask m => m -- TODO Add later
}
-- TODO
--class SensitivityMask where
--    masks :: ActionDescr ->

-- | Beside Standard action we have actions which toggles a state or select from a
-- number of possible states
data ActionType alpha = ActionNormal | ActionToggle | ActionSelect alpha

type FramePrefs = UIManager

makeFrameEvent :: StateM(PEvent FrameEvent)
makeFrameEvent = makeEvent pluginName

triggerFrameEvent :: FrameEvent -> StateM(FrameEvent)
triggerFrameEvent          = triggerEvent pluginName

getFrameEvent :: StateM (PEvent FrameEvent)
getFrameEvent              = getEvent pluginName

registerFrameEvent :: Handler FrameEvent -> StateM HandlerID
registerFrameEvent handler = getFrameEvent >>= \e -> registerEvent e handler
