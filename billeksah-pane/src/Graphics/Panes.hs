
{-# Language
    ExistentialQuantification,
    MultiParamTypeClasses,
    FunctionalDependencies,
    CPP,
    DeriveDataTypeable,
    EmptyDataDecls,
    StandaloneDeriving #-}
   -- TypeFamilies
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Panes
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional21
-- Portability :  portable
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
,   Connection(..)
,   Connections
,   PaneInterface(..)

,   signalDisconnectAll
,   runInIO

,   panePathForGroup
,   initialLayout

,   postSyncState
,   postAsyncState

) where

import Base.State

import Base.Event
import Base.MyMissing
import Base.PluginTypes

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
import Debug.Trace (trace)
import Data.Version (Version(..))


-- ---------------------------------------------------------------------
-- * Panes and pane layout
--

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


type PaneName = String

--
-- | The class which describes the minimal implementation
--
class (Typeable alpha, Show beta, Read beta) => PaneInterface alpha beta  | beta  -> alpha, alpha -> beta   where

    getTopWidget    ::   alpha -> Widget
    -- ^ gets the top Widget of this pane

    primPaneName    ::   alpha -> String
    -- ^ gets a string which names this pane

    paneId          ::   alpha -> String
    -- ^ gets a unique id for this pane

    saveState       ::   alpha -> StateM (Maybe beta)
    -- ^ Returns the state of this pane

    recoverState    ::   PanePath -> beta -> StateM (Maybe alpha)
    -- ^ Sets the state for this pane

    builder         ::   PanePath -> Notebook -> Window -> StateM (Maybe alpha,Connections)
    -- ^ A function, which builds this pane

--
-- | Signal handlers for the different pane types
--
data Connection =  forall alpha . GObjectClass alpha => ConnectC (ConnectId alpha)

type Connections = [Connection]

-- TODO recover somewhere, needs prefs

panePathForGroup::  String -> delta PanePath
panePathForGroup groupName = undefined

signalDisconnectAll :: Connections -> IO ()
signalDisconnectAll = mapM_ (\ (ConnectC s) -> signalDisconnect s)

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

