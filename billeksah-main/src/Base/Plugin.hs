{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, RankNTypes #-}

-----------------------------------------------------------------------------
--
-- Module      :  Base.Plugin
-- Copyright   :  Juergen "jutaro" Nicklisch-Franken
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :
-- Portability :
--
-- | Simple plugin system
--
--
-----------------------------------------------------------------------------

module Base.Plugin (
    Plugin(..),
    VersionBounds,
    Prerequisite,
    PluginName,
    PluginInterface(..),
    PluginConfig(..),
    BaseEvent,
    BaseEventValue(..),
    runAll,

    pseudoPluginName,
    triggerBaseEvent,
    registerBaseEvent,
    getBaseEvent
) where

import Base.Event
import Base.PluginTypes
import Base.State

import Data.Version (showVersion, Version(..))
import Data.IORef (writeIORef, readIORef, newIORef)
import qualified Data.Map as Map (insert, empty, lookup)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Unique (newUnique)
import System.Plugins (loadPackageFunction, load)
import System.Plugins.Load (LoadStatus(..))
import Control.Monad (foldM, when)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Debug.Trace (trace)
import Data.Typeable (Typeable)



-- | This has to be called from main
runAll :: FilePath -> PluginConfig -> [Plugin] -> BaseEvent -> StateM ()
runAll pluginPath config loadList baseEvent = trace ("runAll: " ++ show (map getPluginName loadList))
    $ do

    -- load and pre-init the plugins
    iniRes <- loadPlugins baseEvent loadList
    -- post-init the plugins
    mapM_ (\(GenInterface interface) -> initInterface baseEvent interface) (catMaybes iniRes)
    -- trigger the startup event
    (evtTrigger baseEvent) StartUp -- Here the application shall take control
    return ()

type Error = String

initInterface :: BaseEvent -> PluginInterface event -> StateM()
initInterface baseEvent interface = (piInit2 interface) baseEvent (piEvent interface)

-- | Return type for the loading of plugins
--  If the load fails, Nothing is returned

-- | Load all the plugins,
loadPlugins
  :: BaseEvent
     -> [Plugin]
     -> StateM [Maybe GenInterface]
loadPlugins baseEvent loadList = mapM (loadPlugin baseEvent) loadList

-- | Load one Plugin and call init1
loadPlugin
  :: BaseEvent
     -> Plugin
     -> StateM
          (Maybe GenInterface)
loadPlugin baseEvent plug@Plugin{plInterface =funcName,plModule = moduleName}  =
    trace ("loadPlugin " ++ getPluginName plug) $ do
        mv <- liftIO $ loadPackageFunction (getPluginName plug) moduleName funcName
        -- TODO checkVersion
        case mv of
            Nothing ->  trace "failure" $ (evtTrigger baseEvent) (BaseError
                            ("Cant load " ++ show (getPluginName plug)))
                                >> return Nothing
            (Just interface) ->   trace "success" $do
                    -- use it for error reporting
                    (piInit1 interface) baseEvent (piEvent interface)
                    return (Just (GenInterface interface))

type Warning = String





