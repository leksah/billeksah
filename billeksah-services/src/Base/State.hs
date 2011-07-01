{-# Language ExistentialQuantification, ScopedTypeVariables #-}

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

--
import Data.Typeable (cast, Typeable)
import Data.IORef
       (atomicModifyIORef, readIORef, writeIORef, newIORef, IORef)
import Control.Monad.Reader (ReaderT(..))
import qualified Data.Map as Map (insert, lookup, empty)
import Data.Map (Map)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (liftM)
import Data.Maybe (isJust)
import Control.Exception (catch, Exception)
import Control.Concurrent (forkIO)
import Prelude hiding (catch)

-- | A unique selector is just a string
--
type Selector = String

--
-- | A state is a Map from a selector to something
--
type State a = Map Selector a

--
-- | The special types getting lost here, but can be recovered by attaching runtime
-- type information via Typeable
--
data GenState = forall alpha . Typeable alpha =>  GenState alpha

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

catchState :: Exception e => StateM a -> (e -> IO a) -> StateM a
catchState block handler = reifyState (\ideR -> catch (reflectState block ideR) handler)

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
registerState :: Typeable alpha =>  Selector -> alpha -> StateM (Maybe String)
registerState key value = do
    hasIt <- hasState key
    if hasIt then return $ Just $ "State>>registerState: " ++
                                    "State already registered " ++ key
             else modifyStateM_ (\st -> Map.insert key (GenState value) st) >> return Nothing

--
-- | Registers a key
--
registerState' :: Selector -> GenState -> StateM (Maybe String)
registerState' key value = do
    hasIt <- hasState key
    if hasIt then return $ Just $ "State>>registerState: " ++
                                    "State already registered " ++ key
             else modifyStateM_ (\st -> Map.insert key value st) >> return Nothing

--
-- | Set a value for a key
--
setState :: Typeable alpha => Selector -> alpha -> StateM ()
setState key value = modifyStateM_ (\ st -> case key `Map.lookup` st of
                                            Nothing -> error $ "State>>setState: " ++
                                                            "State not registered " ++ key
                                            Just _ -> Map.insert key (GenState value) st)

--
-- | Get a value for a key
--
getState :: Typeable alpha => Selector -> StateM alpha
getState key = readStateM (\st -> case key `Map.lookup` st of
                                Nothing -> error $ "State>>getState: " ++
                                    "State not registered " ++ key
                                Just (GenState v) -> case cast v of
                                                        Just s -> s
                                                        Nothing -> error "State>>getState: Cast error")

--
-- | Is this state registered?
--
hasState :: Selector -> StateM Bool
hasState sel = readStateM (\st -> case sel `Map.lookup` st of
                                    Nothing -> False
                                    Just _  -> True)

--
-- | Do the function with the state and set the result as new value
--
withState :: Typeable alpha => Selector -> (alpha -> alpha) -> StateM ()
withState key f =
    modifyStateM_
        (\ st -> case key `Map.lookup` st of
                    Nothing -> error $ "State>>withState: " ++
                                    "State not registered " ++ key
                    Just (GenState v) ->
                            let nv = case cast v of
                                        Nothing -> error "State>>withState: Cast error"
                                        Just x -> f x
                            in Map.insert key (GenState nv) st)



