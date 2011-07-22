{-# Language MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, RankNTypes,
    ExistentialQuantification, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances,
    FlexibleInstances, TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Forms.GUIEvent
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for handling gui events by our own event system
--
-----------------------------------------------------------------------------------

module Graphics.Forms.GUIEvent (

    getStandardRegFunction
,   GtkRegFunc
,   GtkHandler
,   Connection(..)
,   Connections
,   GtkRegMap(..)
,   GuiHandlerStateSel(..)
,   GtkEventsStateSel(..)

,   activateGUIEvent
,   activateGUIEvent'
,   makeGUIEvent
,   registerGUIEvent
,   triggerGUIEvent
,   propagateGUIEvent
,   retriggerAsChanged
,   dummyGUIEvent
) where

import Base
import Graphics.Pane
import Graphics.Forms.Basics
       (GUIEventSelector(..), GUIEvent(..), GEvent,
        pluginNameForms)



import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk
import Data.Unique
import Data.IORef
import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map as Map  (delete,insert,lookup,empty)
import Data.Maybe (isJust,fromJust)
import Control.Arrow (first)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))


-- ------------------------------------------------------------
-- * Implementation of GUI event system
-- ------------------------------------------------------------

--  | A type for handling an IO event
--  Returning True: The event has been handles
--  Returning False: Handling should proceed
type GtkHandler = Gtk.Event -> IO Bool

--
-- | A type for a function to register a gtk event
-- |
type GtkRegFunc = forall o . GObjectClass o => o -> GtkHandler -> IO (Connection)

--
-- | The widgets are the real event sources.
-- The GtkRegFunc is the function used to register the event.
-- The connectIds are set, when the event is activated, and
-- can be used to deactivate the event.
-- The last map is used to unregister propagated events properly
--
type GUIEventReg =  ([Connection],Map Unique [(Unique,GUIEvent)])

--
-- | The event state regarding to gtk
--
newtype GtkRegMap =  GtkRegMap (Map EvtID (Map GUIEventSelector GUIEventReg))

data GuiHandlerStateSel = GuiHandlerStateSel
    deriving (Eq, Ord, Show, Typeable)

instance Selector GuiHandlerStateSel where
    type ValueType GuiHandlerStateSel = Handlers GUIEvent


-- | All gui events share the same map
guiEventFactory :: EventFactory GUIEvent (Handlers GUIEvent)
guiEventFactory = EventFactory {
        efGetHandlers = getState GuiHandlerStateSel,
        efSetHandlers = setState GuiHandlerStateSel}

data GtkEventsStateSel = GtkEventsStateSel
    deriving (Eq, Ord, Show, Typeable)

instance Selector GtkEventsStateSel where
    type ValueType GtkEventsStateSel = GtkRegMap

getGtkHandlers :: StateM GtkRegMap
getGtkHandlers = getState GtkEventsStateSel

setGtkHandlers :: GtkRegMap -> StateM ()
setGtkHandlers = setState GtkEventsStateSel

withGtkHandlers :: (GtkRegMap -> GtkRegMap) -> StateM ()
withGtkHandlers = withState GtkEventsStateSel

--
-- | Constructs a new event. The plugin name has to be unique!
--
makeGUIEvent :: StateM (PEvent GUIEvent)
makeGUIEvent = do
    let ef           =  guiEventFactory
    ev <- mkEvent (undefined :: GUIEventSelector) ef
    withState GuiHandlerStateSel (\ (Handlers handlerMap :: Handlers GUIEvent) ->
        case Map.lookup (evtID ev) handlerMap of
                Just [] -> error "Events>>makeGUIEvent: Event already known"
                Nothing -> Handlers (Map.insert (evtID ev) [] handlerMap))
    withGtkHandlers (addEventToGtkHandlers ev)
    return ev
  where
    addEventToGtkHandlers ev (GtkRegMap map) =
        case (evtID ev) `Map.lookup` map of
            Nothing -> GtkRegMap $ Map.insert (evtID ev) (Map.empty) map
            Just x -> error "Events>>makeEvent: Unique not unique"

--
-- | Registers an event handler for this event
--
registerGUIEvent :: GEvent -> [GUIEventSelector] -> Handler GUIEvent -> StateM (HandlerID)
registerGUIEvent event selectors handler = registerEvent event guiEventHandler
  where
    guiEventHandler evt@GUIEvent{geSelector = sel}
        | elem sel selectors    = handler evt
        | otherwise             = return evt

propagateGUIEvent :: GEvent -> [GEvent] -> [GUIEventSelector] -> StateM ()
propagateGUIEvent to fromList selectors =
    mapM_ (\ from -> registerGUIEvent from selectors (\e -> (evtTrigger to) e)) fromList

-- TODO: unregister GUI events

triggerGUIEvent :: GEvent -> GUIEvent -> StateM GUIEvent
triggerGUIEvent event eventValue = (evtTrigger event) eventValue

dummyGUIEvent :: GUIEvent
dummyGUIEvent = GUIEvent{
    geSelector = Dummy,
    geGtkEvent = Gtk.Event True,
    geText = "",
    geMbSelection = Nothing,
    geGtkReturn = True}

--
-- | First register handlers, then activate the GUI event
activateGUIEvent
  :: (GObjectClass o) =>
     o
     -> PEvent GUIEvent
     -> GUIEventSelector
     -> StateM ()
activateGUIEvent widget event eventSel = do
    activateGUIEvent' widget event (getStandardRegFunction eventSel) eventSel


activateGUIEvent'
  :: (GObjectClass o) =>
     o
     -> PEvent GUIEvent
     -> (o -> GtkHandler -> IO Connection)
     -> GUIEventSelector
     -> StateM ()
activateGUIEvent' widget event registerFunc eventSel = do
    cid <- reifyState $ \ stateR -> registerFunc widget (\ e -> do
        Handlers handlerMap <- reflectState (getState GuiHandlerStateSel) stateR
        case Map.lookup (evtID event) handlerMap of
                Nothing -> error "Events>>activateGUIEvent: Unknown event"
                Just [] -> return False
                Just handlers -> do
                    name <- if (widget `isA` gTypeWidget)
                                then widgetGetName (castToWidget widget)
                                else return ""
                    eventList <- mapM (\f -> let ev = GUIEvent
                                                    {geSelector    = eventSel,
                                                     geGtkEvent    = e,
                                                     geText        = "",
                                                     geMbSelection = Nothing,
                                                     geGtkReturn   = False}
                                            in reflectState (f ev) stateR)
                                                    (map snd handlers)
                    let boolList = map geGtkReturn eventList
                    return (foldr (&&) True boolList))
    withGtkHandlers (\ (GtkRegMap gtkHandlers) ->
        case Map.lookup (evtID event) gtkHandlers of
                Nothing -> error "Events>>activateGUIEvent: Unknown event"
                Just gtkMap ->
                    GtkRegMap $ Map.insert  (evtID event) (newGtkMap gtkMap cid) gtkHandlers)
  where
    newGtkMap gtkMap cid =
        case Map.lookup eventSel gtkMap of
            Nothing ->  Map.insert eventSel ([cid],Map.empty) gtkMap
            Just (cids,prop) -> Map.insert eventSel (cid:cids,prop) gtkMap

--
-- | A convinence method for not repeating this over and over again
--
getStandardRegFunction :: GUIEventSelector -> GtkRegFunc
getStandardRegFunction FocusOut         =   \w h -> (castToWidget w) `onFocusOut` h
getStandardRegFunction FocusIn          =   \w h -> (castToWidget w) `onFocusIn` h
getStandardRegFunction ButtonPressed    =   \w h -> (castToWidget w) `onButtonPress` h
getStandardRegFunction KeyPressed       =   \w h -> (castToWidget w) `afterKeyRelease` h
getStandardRegFunction Clicked          =   \w h -> liftM castCID $ (castToButton w) `onClicked`
                                                                    (h (Gtk.Event True) >> return ())
getStandardRegFunction _    =   error "Basic>>getStandardRegFunction: no original GUI event"

retriggerAsChanged :: GEvent -> [GUIEventSelector] -> StateM ()
retriggerAsChanged e selList =
    retriggerEvent e (\ ge@GUIEvent{geSelector = sel} ->
        if elem sel selList
            then Just ge{geSelector = MayHaveChanged}
            else Nothing )
