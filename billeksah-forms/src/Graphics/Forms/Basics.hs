{-# Language MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, RankNTypes,
    ExistentialQuantification, DeriveDataTypeable, TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Forms.Basics
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for the basiscs of composing GUIs from editors
--
-----------------------------------------------------------------------------------

module Graphics.Forms.Basics (
-- * Types
    Getter
,   Setter
,   Injector
,   Extractor
,   Applicator
,   Editor

,   GUIEvent(..)
,   GUIEventSelector(..)
,   GEvent
,   GenSelection(..)
,   genericGUIEvents
,   allGUIEvents

,   pluginNameForms

) where

import Graphics.Forms.Parameters
import Graphics.UI.Gtk
import Base

import qualified Graphics.UI.Gtk.Gdk.Events as Gtk
import Data.Unique
import Data.IORef
import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map as Map  (delete,insert,lookup,empty)
import Data.Maybe (isJust,fromJust)
import Control.Arrow (first)
import Base.MyMissing (allOf)
import Data.Typeable (Typeable)

pluginNameForms = "billeksah-forms"


-- ---------------------------------------------------------------------
-- * Basic Types
--

--
-- | A type for getting a field of a record
--
type Getter alpha beta     =   alpha -> beta
--
-- | A type for setting the field of a record
--
type Setter alpha beta     =   beta -> alpha -> alpha

--
-- | A type for injecting a value into an editor
--
type Injector beta     =   beta -> StateM ()
--
-- | A type for extracting a value from an editor
--
type Extractor beta    =   StateM (Maybe beta)
--
-- | A type for the application of a value to be reflected in the GUI
--
type Applicator beta  =   beta -> StateM ()

--
-- | A type to describe an editor.
-- alpha is the type of the individual field of the record
type Editor alpha  =   Parameters -> GEvent -> StateM(Widget, Injector alpha , Extractor alpha)

type GEvent = PEvent GUIEvent

--
-- | A type for an event in the GUI
--
data GUIEvent = GUIEvent {
    geSelector    :: GUIEventSelector
,   geGtkEvent    :: Gtk.Event
,   geText        :: String
,   geMbSelection :: Maybe GenSelection
,   geGtkReturn   :: Bool -- ^ True means that the event has been completely handled,
                      --  gtk shoudn't do any further action about it (Often not
                      --  a good idea
} deriving Typeable

data GUIEventSelector = FocusOut        -- ^ generic, the widget looses the focus
                    |   FocusIn         -- ^ generic, the widget gets the focus
                    |   ButtonPressed   -- ^ generic, a mouse key has been pressed and released, while the widget has the focus
                    |   KeyPressed      -- ^ generic, a keyboard key has been pressed and released, while the widget has the focus
                    |   Clicked         -- ^ button specific, the button has been pressed
                    |   MayHaveChanged  -- ^ generic, no gui event, the contents of the widget may have changed
                    |   ValidationError -- ^ validation of a contents has failed
                    |   Selection
                    |   Dummy
    deriving (Eq,Ord,Show,Bounded, Typeable, Enum)

instance Selector GUIEventSelector where
    type ValueType GUIEventSelector = PEvent GUIEvent

instance EventSelector GUIEventSelector where
    type BaseType GUIEventSelector = GUIEvent

data GenSelection = forall alpha . Typeable alpha => GenSelection alpha

genericGUIEvents = [FocusOut,FocusIn,ButtonPressed,KeyPressed]
allGUIEvents :: [GUIEventSelector]
allGUIEvents = allOf
