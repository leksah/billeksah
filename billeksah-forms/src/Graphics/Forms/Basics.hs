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

,   FieldDescription(..)
,   GenFieldDescription(..)
,   GenValue(..)

,   SectionName
,   PrefsDescrState(..)

,   FormsEvent(..)
,   FormsEventSel(..)
,   triggerFormsEvent
,   getFormsEvent

) where


import Base
import Graphics.Forms.Parameters

import Graphics.UI.Gtk
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
import qualified Text.PrettyPrint as PP (Doc)
import qualified Text.ParserCombinators.Parsec as P (CharParser)

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

-- * A description for fields, that can be edited with a GUI
--
data FieldDescription alpha =  Field {
        fdParameters      ::  Parameters
    ,   fdFieldPrinter    ::  alpha -> PP.Doc
    ,   fdFieldParser     ::  alpha -> P.CharParser () alpha
    ,   fdFieldEditor     ::  alpha -> StateM (Widget, Injector alpha , alpha -> Extractor alpha , GEvent)
    ,   fdApplicator      ::  alpha -> alpha -> StateM ()}
    | VertBox Parameters [FieldDescription alpha] -- ^ Vertical Box
    | HoriBox Parameters [FieldDescription alpha] -- ^ Horizontal Box
    | TabbedBox [(String,FieldDescription alpha)]   -- ^ Notebook

data GenFieldDescription = forall alpha . Typeable alpha => GenF (FieldDescription alpha) alpha
data GenValue = forall alpha . Typeable alpha => GenV alpha

-- -----------------------------------------------
-- * Events the gui frame triggers
--

-- | The events of the forms plugin
data FormsEvent =
    RegisterPrefs [(String,GenFieldDescription)]
        -- ^ Callback to register perferences
    | PrefsChanged
        -- ^ The preferences have changed
    | NeedRestart
        -- ^ The application needs to be restarted to apply the changed preferences
        deriving Typeable

data FormsEventSel = FormsEventSel
    deriving (Eq, Ord, Show, Typeable)

instance Selector FormsEventSel where
    type ValueType FormsEventSel = PEvent FormsEvent

instance EventSelector FormsEventSel where
    type BaseType FormsEventSel = FormsEvent


triggerFormsEvent :: FormsEvent -> StateM (FormsEvent)
triggerFormsEvent = triggerEvent FormsEventSel

getFormsEvent :: StateM (PEvent FormsEvent)
getFormsEvent = getEvent FormsEventSel

-------------------------------------
-- * Defining the state
--

type SectionName = String

data PrefsDescrState = PrefsDescrState
    deriving (Eq, Ord, Show, Typeable)

instance Selector PrefsDescrState where
    type ValueType PrefsDescrState = [(String,GenFieldDescription)]

