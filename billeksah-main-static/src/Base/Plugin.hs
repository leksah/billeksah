{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, RankNTypes, ExistentialQuantification,
    DeriveDataTypeable #-}

-----------------------------------------------------------------------------
--
-- Module      :  Base.Plugin
-- Copyright   :  Juergen Nicklisch-Franken
-- License     :  LGPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :  portabel
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

    triggerBaseEvent,
    registerBaseEvent,
    getBaseEvent
) where

import Base

import Data.Version (showVersion, Version(..))
import Data.IORef (writeIORef, readIORef, newIORef)
import qualified Data.Map as Map (insert, empty, lookup)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Unique (newUnique)
import Control.Monad (foldM, when)
import Data.Maybe (isJust, catMaybes)
import Data.Map (Map)
import Data.Typeable (Typeable)



-- | This has to be called from main
runAll :: FilePath -> PluginConfig -> [Plugin] -> BaseEvent ->
            Map String GenInterfaceM -> StateM ()
runAll pluginPath config loadList baseEvent initMap = do

    message Debug ("runAll: " ++ show (map getPluginName loadList))

    -- load and pre-init the plugins
    iniRes <- loadPlugins baseEvent loadList initMap
    -- post-init the plugins
    mapM_ (\(GenInterface interface) -> initInterface baseEvent interface)
            (catMaybes iniRes)
    -- trigger the startup event
    (evtTrigger baseEvent) StartUp -- Here the application shall take control
    return ()

type Error = String

initInterface :: BaseEvent -> PluginInterface event -> StateM()
initInterface baseEvent interface = (piInit2 interface) baseEvent (piEvent interface)


-- | Load all the plugins,
loadPlugins
  :: BaseEvent
     -> [Plugin]
     -> Map String GenInterfaceM
     -> StateM [Maybe GenInterface]
loadPlugins baseEvent loadList initMap = mapM (loadPlugin baseEvent initMap) loadList

-- | Load one Plugin and call init1
loadPlugin
  :: BaseEvent
     -> Map String GenInterfaceM
     -> Plugin
     -> StateM (Maybe GenInterface )
loadPlugin baseEvent initMap plug@Plugin{plInterface =funcName,plModule = moduleName}  = do
    message Info ("loadPlugin " ++ getPluginName plug)
    let genInterface = Map.lookup (plName plug) initMap
    -- TODO checkVersion
    case genInterface of
        Nothing ->  do
            message Error ("Cant load " ++ show (getPluginName plug))
            return Nothing
        Just (GenInterfaceM getInterface) ->  do
                -- use it for error reporting
                interface <- getInterface
                (piInit1 interface) baseEvent (piEvent interface)
                return (Just (GenInterface interface))

type Warning = String






