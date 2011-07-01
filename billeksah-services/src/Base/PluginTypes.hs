{-# Language DeriveDataTypeable, ExistentialQuantification, RankNTypes, FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  Base.PluginTypes
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

module Base.PluginTypes where

import Base.Event
import Base.State

import Data.Version (showVersion, Version(..))
import Data.Typeable (cast, Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef)
import qualified Data.Map as Map (empty)

-- | The type of event this base component can trigger

type BaseEvent = PEvent BaseEventValue

data BaseEventValue = StartUp | BaseError String
    deriving Typeable

data PluginInterface event =
    PluginInterface {
        piInit1   :: BaseEvent -> PEvent event -> StateM (),
        piInit2   :: BaseEvent -> PEvent event -> StateM (),
        piEvent   :: PEvent event,
        piName    :: String,
        piVersion :: Version}
        deriving Typeable

data GenInterfaceM = forall alpha . GenInterfaceM (StateM (PluginInterface alpha))
    deriving Typeable

data GenInterface = forall event . GenInterface (PluginInterface event)

-- | A PluginConfig is a configuration for an app, which is composed of plugins
data PluginConfig  = PluginConfig {
    cfName          :: String,   -- ^ name of the config
    cfVersion       :: Version,  -- ^ version of the config
    cfPlugins       :: [Prerequisite], -- ^ plugins to load
    cfChoices       :: [Prerequisite], -- ^ other plugins
    cfSynopsis      :: String} -- ^ comment for this configuration
    deriving (Eq,Ord,Show)

type PluginName    = String

-- | A plugin has
--  * a unique name
--  * a version
--  * a list of prerequisites
--  * maybe a comment
--  * a module and function name for getting a PluginConfig

data Plugin        = Plugin {
    plName          :: PluginName,
    plVersion       :: Version,
    plPrerequisites :: [Prerequisite],
    plChoices       :: [Prerequisite],
    plModule        :: String,
    plInterface     :: String,
    plSynopsis      :: String}
    deriving (Eq,Ord,Show)

-- | Get the name of the plugin, which includes its version
getPluginName :: Plugin -> String
getPluginName Plugin{plName = name, plVersion = version} = name ++ "-" ++ showVersion version

-- | An upper and a lower bound for versions, which may be omitted
type VersionBounds = (Maybe Version, Maybe Version)

-- | A prerequisite is a pair of a plugin and version bounds
type Prerequisite  = (PluginName, VersionBounds)

type LoadList      = [Plugin]

pseudoPluginName = "billeksah-main"

triggerBaseEvent :: BaseEventValue -> StateM(BaseEventValue)
triggerBaseEvent = triggerEvent pseudoPluginName

getBaseEvent :: StateM (BaseEvent)
getBaseEvent = getEvent pseudoPluginName

configPathSelector = "billeksah-main.currentConfigPath"

registerBaseEvent :: Handler BaseEventValue -> StateM HandlerID
registerBaseEvent handler = getBaseEvent >>= \e -> registerEvent e handler
setCurrentConfigPath :: FilePath -> StateM ()
setCurrentConfigPath =  setState configPathSelector

getCurrentConfigPath :: StateM FilePath
getCurrentConfigPath = getState configPathSelector

registerCurrentConfigPath :: FilePath -> StateM (Maybe String)
registerCurrentConfigPath = registerState configPathSelector
