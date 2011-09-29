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
,   castFD
,   toGenFieldDescr
,   toFieldEditor

,   SectionName
,   PrefsDescrState(..)

,   FormsEvent(..)
,   FormsEventSel(..)
,   triggerFormsEvent
,   getFormsEvent

,   setCurrentPrefsPath
,   getCurrentPrefsPath
,   registerCurrentPrefsPath

) where


import Base
import Graphics.Forms.Parameters

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk
import Control.Monad
import Data.Typeable (Typeable(..), Typeable1, cast, Typeable)
import qualified Text.PrettyPrint as PP (Doc)
import qualified Text.ParserCombinators.Parsec as P (CharParser)
import Debug.Trace (trace)
import Data.Maybe (fromJust)

pluginNameForms :: [Char]
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

data GenSelection = forall alpha . Typeable alpha => GenSelection alpha

allGUIEvents, genericGUIEvents :: [GUIEventSelector]
genericGUIEvents = [FocusOut,FocusIn,ButtonPressed,KeyPressed]
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
    deriving (Typeable)

-- | A type neutral FieldDescription with a type neutral value attached
data GenFieldDescription = forall alpha . (Typeable alpha, Eq alpha) => GenF (FieldDescription alpha) alpha

-- | A type neutral value of a field description
data GenValue = forall alpha . (Typeable alpha, Eq alpha) => GenV alpha
    deriving Typeable

instance Eq GenValue where
    (GenV a) == (GenV b) = if typeOf a == typeOf b then
                    fromJust (cast a) == b
                    else False

toGenFieldDescr :: (Typeable alpha, Eq alpha) => FieldDescription alpha ->
    FieldDescription GenValue
toGenFieldDescr (VertBox paras fdl) = VertBox paras (map toGenFieldDescr fdl)
toGenFieldDescr (HoriBox paras fdl) = HoriBox paras (map toGenFieldDescr fdl)
toGenFieldDescr (TabbedBox list)    = TabbedBox
        (map (\(s,fd) -> (s,toGenFieldDescr fd)) list)
toGenFieldDescr (Field paras fdFieldPrinter fdFieldParser fdFieldEditor fdApplicator)
        = trace (show paras) $ Field
            paras
            (\ (GenV a) ->
                let a' = myCast "Basics>>toGenFieldDescr:1 " a
                in fdFieldPrinter a')
            (\ (GenV a) ->
                let a' =  myCast "Basics>>toGenFieldDescr:2 " a
                in liftM GenV (fdFieldParser a'))
            (\ (GenV a) ->
                let a' = myCast "Basics>>toGenFieldDescr:3 " a
                in liftM toFieldEditor (fdFieldEditor a'))
            (\ (GenV a) (GenV b) ->
                let (a', b') = (myCast "Basics>>toGenFieldDescr:4 " a,
                                myCast "Basics>>toGenFieldDescr:5 " b)
                in fdApplicator a' b')

-- | A cast from a type neutral FieldDescription with a type neutral value
-- to a typed field description with a typed value
castFD :: (Typeable alpha, Typeable1 FieldDescription, Typeable GenValue) =>
    FieldDescription GenValue -> FieldDescription alpha
castFD fdGen = myCast "Basics>>castFD" fdGen


toFieldEditor :: (Typeable alpha, Eq alpha) => (Widget,
                     Injector alpha,
                     alpha -> Extractor alpha,
                     GEvent)
                -> (Widget,
                     Injector GenValue,
                     GenValue -> Extractor GenValue,
                     GEvent)
toFieldEditor (widget, inj, ext, gevent) =
    (widget,
        (\ (GenV a) -> let a' = myCast "Basics>>toFieldEditor:1" a
                       in inj a'),
        (\ (GenV a) -> let a' = myCast "Basics>>toFieldEditor:2" a
                       in liftM genMaybe (ext a')),
        gevent)
  where
    genMaybe :: (Typeable alpha, Eq alpha) => Maybe alpha -> Maybe GenValue
    genMaybe Nothing  = Nothing
    genMaybe (Just a) = Just (GenV a)

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

triggerFormsEvent :: FormsEvent -> StateM (FormsEvent)
triggerFormsEvent = triggerEvent FormsEventSel

getFormsEvent :: StateM (PEvent FormsEvent)
getFormsEvent = getEvent FormsEventSel

-------------------------------------
-- * Defining the state
--


type SectionName = String

-- | The description and current value of preferences
data PrefsDescrState = PrefsDescrState
    deriving (Eq, Ord, Show, Typeable)

-- | The description and current value of preferences
instance Selector PrefsDescrState where
    type ValueType PrefsDescrState = [(SectionName,GenFieldDescription)]

-- | The file path for preferences
data PrefsPathSel = PrefsPathSel
    deriving (Eq,Ord,Show,Typeable)

-- | The file path for preferences
instance Selector PrefsPathSel where
    type ValueType PrefsPathSel = FilePath

setCurrentPrefsPath :: FilePath -> StateM ()
setCurrentPrefsPath =  setState PrefsPathSel

getCurrentPrefsPath :: StateM FilePath
getCurrentPrefsPath = getState PrefsPathSel

registerCurrentPrefsPath :: FilePath -> StateM (Maybe String)
registerCurrentPrefsPath = registerState PrefsPathSel
