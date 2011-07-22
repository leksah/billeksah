{-# Language DeriveDataTypeable, ExistentialQuantification, RankNTypes, FlexibleContexts,
    TypeFamilies #-}

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
import Base.Selector

import Data.Version (showVersion, Version(..))
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef)
import qualified Data.Map as Map (empty)
import Control.Monad (when)

-- | The type of event this base component can trigger

data MessageLevel = Debug | Info | Warning | Error
    deriving(Eq,Ord,Show,Read)

type BaseEvent = PEvent BaseEventValue

data BaseEventValue = StartUp | BaseLog MessageLevel String

data PluginInterface event =
    PluginInterface {
        piInit1   :: BaseEvent -> PEvent event -> StateM (),
        piInit2   :: BaseEvent -> PEvent event -> StateM (),
        piEvent   :: PEvent event,
        piName    :: String,
        piVersion :: Version}

data GenInterfaceM = forall alpha . GenInterfaceM (StateM (PluginInterface alpha))

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
    deriving (Eq,Ord,Show, Read)

-- | Get the name of the plugin, which includes its version
getPluginName :: Plugin -> String
getPluginName Plugin{plName = name, plVersion = version} = name ++ "-" ++ showVersion version

-- | An upper and a lower bound for versions, which may be omitted
type VersionBounds = (Maybe Version, Maybe Version)

-- | A prerequisite is a pair of a plugin and version bounds
type Prerequisite  = (PluginName, VersionBounds)

type LoadList      = [Plugin]

data BaseEventSel = BaseEventSel
    deriving (Eq,Ord,Show,Typeable)

instance Selector BaseEventSel where
    type ValueType BaseEventSel = PEvent BaseEventValue

instance EventSelector BaseEventSel where
    type BaseType BaseEventSel = BaseEventValue

-- | ConfigPathSel | MessageLevelSel

triggerBaseEvent :: BaseEventValue -> StateM(BaseEventValue)
triggerBaseEvent = triggerEvent BaseEventSel

getBaseEvent :: StateM (BaseEvent)
getBaseEvent = getEvent BaseEventSel

registerBaseEvent :: Handler BaseEventValue -> StateM HandlerID
registerBaseEvent handler = getBaseEvent >>= \e -> registerEvent e handler

data ConfigPathSel = ConfigPathSel
    deriving (Eq,Ord,Show,Typeable)

instance Selector ConfigPathSel where
    type ValueType ConfigPathSel = FilePath

setCurrentConfigPath :: FilePath -> StateM ()
setCurrentConfigPath =  setState ConfigPathSel

getCurrentConfigPath :: StateM FilePath
getCurrentConfigPath = getState ConfigPathSel

registerCurrentConfigPath :: FilePath -> StateM (Maybe String)
registerCurrentConfigPath = registerState ConfigPathSel

data MessageLevelSel = MessageLevelSel
    deriving (Eq,Ord,Show,Read,Typeable)

instance Selector MessageLevelSel where
    type ValueType MessageLevelSel = MessageLevel

setMessageLevel :: MessageLevel -> StateM ()
setMessageLevel =  setState MessageLevelSel

getMessageLevel :: StateM MessageLevel
getMessageLevel = getState MessageLevelSel

registerMessageLevel :: MessageLevel -> StateM (Maybe String)
registerMessageLevel = registerState MessageLevelSel

--
-- | Outputs a message. Triggers the BaseEvent BaseLog, which is in this
-- package handled to ~BaseLog level str -> liftIO $ putStrLn (show level ++ " " ++ str)~
message :: MessageLevel -> String -> StateM ()
message level str = do
    fromLevel <- getMessageLevel
    when (level >= fromLevel) $ do
        triggerBaseEvent (BaseLog level str) >> return ()


messageR :: MessageLevel -> String -> StateRef -> IO ()
messageR level str = reflectState (message level str)

