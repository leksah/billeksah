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
    panePluginName
,   ActionDescr(..)
,   ActionType(..)
,   MenuPosition(..)
,   ToolPosition(..)
,   FrameSelector(..)
,   SessionExtension(..)
,   GenSessionExtension(..)
,   CompDescr(..)
,   CompPosition(..)
,   CompName
,   CompWidget(..)
) where

import Base

import Graphics.UI.Gtk
import Data.Typeable
       (TypeRep(..), typeRepKey, cast, Typeable(..))
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

panePluginName = "billeksah-pane"

data FrameSelector = FrameEventSel | PaneActiveSens | FrameStateSel | ActionStateSel
    deriving (Eq, Ord, Show, Typeable)

instance Selector FrameSelector

---------------------------------------------------------------------------------------
-- * Types for Actions, Menus, Toolbars

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
,   adMenu        ::   Maybe MenuPosition
,   adToolbar     ::   Maybe ToolPosition
,   adSensitivities :: [GenSelector]
}

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
    deriving Eq

-- | Beside Standard action we have actions which toggles a state or select from a
-- number of possible states
data ActionType = ActionNormal | ActionToggle | ActionSubmenu -- TODO ActionSelect alpha
    deriving Eq

---------------------------------------------------------------------------------------
-- * Types for Sessions

data (Read alpha, Show alpha) => SessionExtension alpha = SessionExtension {
    seName       :: String,
    seRetriever  :: StateM alpha,
    seApplicator :: alpha -> StateM ()}

data GenSessionExtension = forall alpha . (Read alpha, Show alpha) => GenS (SessionExtension alpha)

---------------------------------------------------------------------------------------
-- * Types for Statusbar

type CompName = String

--
-- | Description of a Statusbar compartment
--
data CompDescr =
    TextCompDescr
        {scName          :: CompName,
         scHasResizeGrip :: Bool,
         scRequestedSize :: Int,
         scPacking       :: Packing,
         scPosition      :: CompPosition}

--
-- | Description of a Statusbar position
--
data CompPosition =
    CPFirst                             -- ^ Add this item in the first position.
    | CPLast                            -- ^ Add this item in the last position.
    | CPAfter String                    -- ^ Add this item after the first arg string
    | CPBefore String                   -- ^ Add this item before the first arg string
    deriving Eq


data CompWidget = CompWText Statusbar


