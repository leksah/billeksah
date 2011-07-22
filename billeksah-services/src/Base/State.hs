{-# Language ExistentialQuantification, ScopedTypeVariables, StandaloneDeriving,
    DeriveDataTypeable, FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  Base.State
-- Copyright   :  Juergen "jutaro" Nicklisch-Franken
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Base.State (
-- * High level interface
    registerState,
    setState,
    getState,
    hasState,
    withState,
    runState,

-- * Convenience functions
    reifyState,
    reflectState,
    runInIO,
    catchState,
    forkState,

-- * Types
    TheState,
    StateRef,
    StateM,
    StateAction,
    GenState(..),

-- * Low level interface
    registerState',
) where

import Base.Selector

import Data.IORef
       (atomicModifyIORef, readIORef, writeIORef, newIORef, IORef)
import Control.Monad.Reader (ReaderT(..))
import qualified Data.Map as Map (insert, lookup, empty)
import Data.Map (Map)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (liftM)
import Data.Maybe (fromJust, isJust)
import Control.Exception (catch, Exception)
import Control.Concurrent (forkIO)
import Prelude hiding (catch)
import Unsafe.Coerce (unsafeCoerce)

--
-- | A state is a Map from a selector to something
--
type State a = Map GenSelector a

--
-- | The special types getting lost here, but can be recovered by attaching runtime
-- type information via Typeable
--
data GenState = forall alpha . GenState alpha

--
-- | The concrete state is build from this
--
type TheState = State GenState

--
-- | A mutable reference to the IDE state
--
type StateRef = IORef TheState

--
-- | The IDE Monad
--
type StateM = ReaderT StateRef IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
--   which does not return a value
--
type StateAction = StateM ()

--
-- | Initial call for the state service,
--  everything has to happen now inside this monad
--
runState :: StateM a -> IO a
runState f = do
    ref :: StateRef <- newIORef Map.empty
    reflectState f ref

--
-- | Lift an IO action to an IDE action
--
reifyState :: (StateRef -> IO a) -> StateM a
reifyState = ReaderT

runInIO :: forall alpha beta. (beta -> StateM alpha) -> StateM (beta -> IO alpha)
runInIO f          =   reifyState (\ideRef -> return (\v -> reflectState (f v) ideRef))

--
-- | Do an IDE action in the IO monad
--
reflectState :: StateM a -> StateRef -> IO a
reflectState c ideR = runReaderT c ideR

catchState :: Exception e => StateM a -> (e -> StateM a) -> StateM a
catchState block handler = reifyState (\ideR -> catch (reflectState block ideR) (handler' ideR))
  where
    handler' ideR = (\ f e -> reflectState (f e) ideR) handler

forkState :: StateAction  -> StateAction
forkState block  = reifyState (\ideR -> forkIO  (reflectState block ideR) >> return ())


-- ---------------------------------------------------------------------
-- * Methods for accesing the IDE State
--

--
-- | Read an attribute of the contents
--
readStateM :: (TheState -> beta) -> StateM beta
readStateM f = do
    e <- ask
    liftIO $ liftM f (readIORef e)

--
-- | Modify the contents, without returning a value
--
modifyStateM_ :: (TheState -> TheState) -> StateM ()
modifyStateM_ f = let f' a  = (f a,()) in do
    e <- ask
    liftIO (atomicModifyIORef e f')

--
-- | Variation on modifyIDE_ that lets you return a value
--
modifyStateM :: (TheState -> (TheState,beta)) -> StateM beta
modifyStateM f = do
    e <- ask
    liftIO (atomicModifyIORef e f)

--
-- | Call a function in the IO Monad with the state and return the result
--
withStateM :: (TheState -> IO alpha) -> StateM alpha
withStateM f = do
    e <- ask
    liftIO $ f =<< readIORef e

--
-- | Gets the state
--
getStateM :: StateM(TheState)
getStateM = do
    e <- ask
    st <- liftIO $ readIORef e
    return st

--
-- | Registers a key and sets the value
--
registerState :: Selector alpha => alpha -> ValueType alpha -> StateM (Maybe String)
registerState key value = do
    hasIt <- hasState key
    if hasIt then return $ Just $ "State>>registerState: " ++
                                    "State already registered " ++ show key
             else modifyStateM_ (\st -> Map.insert (GS key) (GenState value) st) >> return Nothing

--
-- | Registers a key
--
registerState' :: Selector alpha => alpha -> GenState -> StateM (Maybe String)
registerState' key value = do
    hasIt <- hasState key
    if hasIt then return $ Just $ "State>>registerState: " ++
                                    "State already registered " ++ show key
             else modifyStateM_ (\st -> Map.insert (GS key) value st) >> return Nothing

--
-- | Set a value for a key
--
setState ::  Selector alpha => alpha  -> ValueType alpha -> StateM ()
setState key value = modifyStateM_ (\ st -> case GS key `Map.lookup` st of
                                            Nothing -> error $ "State>>setState: " ++
                                                            "State not registered " ++ show key
                                            Just _ -> Map.insert (GS key) (GenState value) st)

--
-- | Get a value for a key
--
getState :: Selector alpha => alpha  -> StateM (ValueType alpha)
getState key = readStateM (\st -> case (GS key) `Map.lookup` st of
                                Nothing -> error $ "State>>getState: " ++
                                    "State not registered " ++ show key
                                Just (GenState v) -> unsafeCoerce v)

--
-- | Is this state registered?
--
hasState :: Selector alpha => alpha -> StateM Bool
hasState sel = readStateM (\st -> case GS sel `Map.lookup` st of
                                    Nothing -> False
                                    Just _  -> True)

--
-- | Do the function with the state and set the result as new value
--
withState :: Selector alpha => alpha -> (ValueType alpha -> ValueType alpha) -> StateM ()
withState key f =
    modifyStateM_
        (\ st -> case (GS key) `Map.lookup` st of
                    Nothing -> error $ "State>>withState: " ++
                                    "State not registered " ++ show key
                    Just (GenState v) ->
                            let nv = f (unsafeCoerce v)
                            in Map.insert (GS key) (GenState nv) st)



