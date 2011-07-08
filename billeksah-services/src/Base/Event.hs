{-# LANGUAGE RankNTypes, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances,
    FlexibleInstances, ExistentialQuantification, FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  Base.Event
-- Copyright   :  Juergen "jutaro" Nicklisch-Franken
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :
-- Portability :
--
-- | Simple (imperative) event implementation
--  For concrete usage an event factory has to be constructed.
-----------------------------------------------------------------------------

module Base.Event (
-- * High level interface
    makeEvent,
    getEvent,
    registerEvent,
    triggerEvent,
    unionEvent,
    filterEvent,
    propagateEvent,
    retriggerEvent,

-- * Types
    Handler,
    Handlers(..),
    EvtID,
    HandlerID,
    PEvent(..),
    EventFactory(..),

-- * Low level interface
    stdEventFactory,
    mkEvent,
    GenEvent(..),
    newEventID
) where

import Base.State
import Base.Selector

import Data.Unique (newUnique, Unique)
import Data.Map (Map)
import Control.Monad.IO.Class
import qualified Data.Map as Map (empty, insert, lookup)
import Control.Monad (foldM)
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Typeable (typeRepKey, Typeable(..), cast, Typeable)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

--   ---------------------------------
--  Types
--

--
-- | A handler is a callback function
--
type Handler event = event -> StateM event

type EvtID = Unique
type HandlerID = Unique

--
-- | An event has a unique id, can be triggered, and callbacks can be registered
-- and unregistered
--
data PEvent event = PEvent {
    evtRegister    :: Handler event  -> HandlerID -> StateM (),
    evtUnregister  :: HandlerID -> StateM (),
    evtTrigger     :: event -> StateM event,
    evtID          :: EvtID}
        deriving Typeable

--
-- | A boxed type for events
--
data GenEvent =  forall event. Typeable event => GenEvent event
    deriving Typeable

--
-- | The implementation type
--
newtype Handlers event = Handlers (Map EvtID [(HandlerID, event  -> StateM event)])
    deriving Typeable

--
-- | The provider of low level state mechanism
--
data EventFactory event handlers = EventFactory {
    efGetHandlers :: StateM handlers,
    efSetHandlers :: handlers  -> StateM ()}

--   ---------------------------------
--  High level interface
--

--
-- | Constructs a new event. The plugin name has to be unique!
--
makeEvent :: (Selector alpha, Typeable beta) => alpha -> StateM (PEvent beta)
makeEvent selector = do
    ideRef           <- liftIO $ newIORef (Handlers Map.empty)
    let ef           =  stdEventFactory ideRef
    ev <- mkEvent ef
    persistEvent selector ev
    return ev

--
-- | Get the event from a plugin name (The type has to fit, otherwise
--  an error will be thrown).
--
getEvent :: (Selector alpha, Typeable beta) => alpha -> StateM (PEvent beta)
getEvent sel = do
    (GenEvent e) <- getGEvent sel
    case cast e of
        Just v -> return v
        Nothing -> error ("PluginTypes>>getEvent: Can't cast event " ++ show sel)

--
-- | Registers an event handler for this event
--
registerEvent :: PEvent alpha -> Handler alpha -> StateM (HandlerID)
registerEvent event handler = do
    newEvtID         <- liftIO $ newUnique
    (evtRegister event) handler newEvtID
    return newEvtID

--
-- | Triggers the event with the provided value
--
triggerEvent :: (Selector alpha, Typeable beta) => alpha  -> beta -> StateM beta
triggerEvent sel e = getEvent sel >>= \ event -> (evtTrigger event) e

--
-- | Merge two event streams of the same type
--
unionEvent :: Typeable event => PEvent event  -> PEvent event  -> StateM (PEvent event )
unionEvent e1 e2 = do
    newEvtID <- newEventID
    return $ PEvent {
        evtRegister    = \ handler newHdlIdID -> do
            (evtRegister e1) handler newHdlIdID
            (evtRegister e2) handler newHdlIdID,
        evtUnregister  = \ hdlID -> do
            (evtUnregister e1) hdlID
            (evtUnregister e2) hdlID,
        evtTrigger     = \ event -> do
            (evtTrigger e1) event
            (evtTrigger e2) event,
        evtID          = newEvtID}

--
-- | Allow all events that fulfill the predicate, discard the rest. Think of it as
--
filterEvent :: Typeable e =>  (e -> Bool) -> PEvent e -> StateM (PEvent e)
filterEvent filterFunc event = do
    newEvtID <- newEventID
    return $ PEvent {
        evtRegister    = \ handler newHdlId -> do
            (evtRegister event) (newHandler handler) newHdlId,
        evtUnregister  = evtUnregister event,
        evtTrigger     = evtTrigger event,
        evtID          = newEvtID}
  where
    newHandler handler = \ event -> do
        if filterFunc event
            then handler event
            else return event

--
-- | Propagate event ...
--
propagateEvent :: Typeable e =>  PEvent e -> [PEvent e] -> StateM ()
propagateEvent to fromList =
    mapM_ (\ from -> registerEvent from (\e -> (evtTrigger to) e)) fromList

retriggerEvent :: Typeable e =>  PEvent e -> (e -> Maybe e) -> StateM ()
retriggerEvent event trans =
    registerEvent event (\e -> case trans e of
                                    Nothing -> return e
                                    Just ne -> (evtTrigger event) ne) >> return ()

--   ---------------------------------
--  Low level implementation
--

stdEventFactory :: Typeable event => IORef (Handlers event) -> EventFactory event (Handlers event)
stdEventFactory handlersRef = EventFactory {
        efGetHandlers = liftIO $ readIORef handlersRef,
        efSetHandlers = \ nh -> liftIO $ writeIORef handlersRef nh}

newEventID = liftIO $ newUnique

--
-- | Make an PEvent in the IO Monad
--
mkEvent :: (Typeable event) =>
                EventFactory event (Handlers event) -> StateM (PEvent event)
mkEvent ef@EventFactory{efGetHandlers = getHandlers, efSetHandlers = setHandlers} = do
    newEvtID <- newEventID
    return $ PEvent {
        evtRegister     = \ handler newUni -> do
            Handlers handlerMap  <-  getHandlers
            let newHandlers =   case newEvtID `Map.lookup` handlerMap of
                                    Nothing -> Map.insert newEvtID [(newUni,handler)] handlerMap
                                    Just l  -> Map.insert newEvtID ((newUni,handler):l) handlerMap
            setHandlers (Handlers newHandlers),
        evtUnregister   = \ unique -> do
            Handlers handlerMap  <-  getHandlers
            let newHandlers =   case newEvtID `Map.lookup` handlerMap of
                                    Nothing -> handlerMap
                                    Just l -> let newList = filter (\ (mu,_) -> mu /= unique) l
                                              in  Map.insert newEvtID newList handlerMap
            setHandlers (Handlers newHandlers)
            return (),
        evtTrigger      = \ event -> do
            Handlers handlerMap  <-  getHandlers
            case newEvtID `Map.lookup` handlerMap of
                Nothing     ->  return event
                Just l      ->  foldM (\ e (_,ah) -> ah e) event (reverse l),
        evtID           = newEvtID}

getGEvent :: Selector alpha => alpha -> StateM GenEvent
getGEvent key = getState key

pluginEventPrefix = "billeksah-main.event"

persistEvent :: (Selector alpha, Typeable beta) =>  alpha -> PEvent beta -> StateM ()
persistEvent key event =
     registerState  key (GenEvent event) >> return ()



