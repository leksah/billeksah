
{-# Language
    ExistentialQuantification,
    MultiParamTypeClasses,
    FunctionalDependencies,
    CPP,
    DeriveDataTypeable,
    EmptyDataDecls,
    StandaloneDeriving,
    TypeFamilies,
    FlexibleContexts,
    ScopedTypeVariables,
    RankNTypes,
    FlexibleInstances,
    TypeSynonymInstances #-}
   -- TypeFamilies
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Panes
-- Copyright   :  Juergen Nicklisch-Franken
-- License     :  LGPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :  portabel
--
-- | The basic definitions for all panes
--
-------------------------------------------------------------------------------

module Graphics.Panes (

-- * Panes and pane layout
    Direction(..)
,   PaneDirection(..)
,   PanePathElement(..)
,   PanePath
,   PaneLayout(..)
,   PaneName
,   Connection
,   Connections
,   castCID
,   PaneInterface(..)

,   PanePrefs(..)

,   panePluginName
,   ActionDescr(..)
,   ActionType(..)
,   MenuPosition(..)
,   ToolPosition(..)
,   SessionExtension(..)
,   GenSessionExtension(..)
,   CompDescr(..)
,   CompPosition(..)
,   CompName
,   CompWidget(..)

-- * Other
,   signalDisconnectAll

,   initialLayout

,   postSyncState
,   postAsyncState

) where

import Base

import Graphics.UI.Gtk hiding (get)
import System.Glib.GObject
import System.Glib.Signals
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Control.Monad.Trans
import Data.List (isPrefixOf, stripPrefix, findIndex)
import Control.Monad (liftM, when)
import qualified Data.Set as Set (member)
import Data.IORef (newIORef)
import Data.Version (Version(..))
import Foreign.C (CULong)



-- ---------------------------------------------------------------------
-- * Panes and pane layout
--

type PaneName = String

--
-- | The direction of a split
--
data Direction      =   Horizontal | Vertical
    deriving (Eq,Show)

--
-- | A path to a pane
--
type PanePath       =   [PanePathElement]

--
-- | An element of a path to a pane
--
data PanePathElement = SplitP PaneDirection | GroupP String
    deriving (Eq,Show,Read)

--
-- | The relative direction to a pane from the parent
--
data PaneDirection  =   TopP | BottomP | LeftP | RightP
    deriving (Eq,Show,Read)

--
-- | Description of a window layout
-- Horizontal: top bottom Vertical: left right
--
data PaneLayout =       HorizontalP PaneLayout PaneLayout Int
                    |   VerticalP PaneLayout PaneLayout Int
                    |   TerminalP {
                                paneGroups   :: Map String PaneLayout
                            ,   paneTabs     :: Maybe PaneDirection
                            ,   currentPage  :: Int
                            ,   detachedId   :: Maybe String
                            ,   detachedSize :: Maybe (Int, Int) }
    deriving (Eq,Show,Read)

--
-- | Empty initial layout
--
initialLayout = TerminalP {
    paneGroups   =  Map.empty
,   paneTabs     =  Nothing
,   currentPage  =  (-1)
,   detachedId   = Nothing
,   detachedSize = Nothing}


data PanePrefs = PanePrefs {
    ppCategoryForPane :: [(String,String)],
    ppPathForCategory :: [(String,PanePath)],
    ppDefaultPath :: PanePath}
    deriving (Eq,Typeable)



--
-- | The class which describes the minimal implementation
--
class (Typeable alpha, Show (PaneState alpha), Read (PaneState alpha)) => PaneInterface alpha where

    data PaneState alpha :: *
    type PaneArgs  alpha :: *

    primPaneName    ::   alpha -> String
    -- ^ gets a string which names this pane


    paneType          :: alpha -> String
    -- ^ gets a unique id for this type of pane

    builder         ::   PaneArgs alpha -> PanePath -> Notebook -> Window -> StateM (Maybe alpha,Connections)
    -- ^ A function, which builds this pane

    getTopWidget    ::   alpha -> Widget
    -- ^ gets the top Widget of this pane

    saveState       ::   alpha -> StateM (Maybe (PaneState alpha))
    -- ^ Returns the state of this pane

    recoverState    ::   PanePath -> PaneState alpha -> StateM (Maybe alpha)
    -- ^ Sets the state for this pane


panePluginName = "billeksah-pane"







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





--
-- | Signal handlers for the different pane types
--
type Connection =  ConnectId Widget

type Connections = [Connection]

castCID :: GObjectClass alpha  => ConnectId alpha -> ConnectId Widget
castCID (ConnectId ui o) = (ConnectId ui (castToWidget o))

-- TODO recover somewhere, needs prefs

signalDisconnectAll :: Connections -> IO ()
signalDisconnectAll = mapM_ (\ s -> signalDisconnect s)

instance Show Window where
    show _ = "a Window"

instance Show UIManager where
    show _ = "a UIManager"

instance Show Connection where
    show _ = "a Connection"

instance Show Notebook where
    show _ = "a Notebook"

deriving instance Typeable UIManager

--
postSyncState :: StateM a -> StateM a
postSyncState f = reifyState (\ideR -> postGUISync (reflectState f ideR))

--
postAsyncState :: StateM () -> StateM ()
postAsyncState f = reifyState (\ideR -> postGUIAsync (reflectState f ideR))



--  ----------------------------------------
-- * Necessary with pre 10.1 verion of gtk2hs
--

#ifdef MIN_VERSION_gtk
#if MIN_VERSION_gtk(0,10,1)
#else
instance Eq Notebook
    where (==) a b = let (GObject pa, GObject pb) = (toGObject a, toGObject b)
                    in pa == pb
instance Ord Notebook
    where (<=) a b = let (GObject pa, GObject pb) = (toGObject a, toGObject b)
                    in pa <= pb
instance Eq Window
    where (==) a b = let (GObject pa, GObject pb) = (toGObject a, toGObject b)
                    in pa == pb
instance Eq Widget
    where (==) a b = let (GObject pa, GObject pb) = (toGObject a, toGObject b)
                    in pa == pb
#endif
#endif


