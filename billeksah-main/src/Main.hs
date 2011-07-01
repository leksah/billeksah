{-# LANGUAGE EmptyDataDecls #-}

-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | This starts up the plug in shell, and next the app
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Base.Plugin
import Base.Config
import Base.PluginTypes
import Base.Event

import System.Console.GetOpt
       (OptDescr, usageInfo, ArgOrder(..), getOpt)
import System.Environment (getArgs)
import System.FilePath (dropFileName)
import Data.IORef (newIORef)
import qualified Data.Map as Map (empty)
import Base.State (runState)
import Control.Monad.IO.Class (MonadIO(..))

data Flag

header :: String
header = "Usage: billeksah-base [OPTION...] pluginConfig"

options :: [OptDescr Flag]
options = []

getOpts :: [String] -> IO ([Flag], [String])
getOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options

main = do
    args             <- getArgs
    (o,files)        <- getOpts args
    let pluginCPath  =  case files of
                            i:_ -> i
                            _   -> error "Required arg missing: pluginConfig"
        pluginPath   =  dropFileName pluginCPath
    config       <- loadPluginConfig pluginCPath
    runState (do
        baseEvent    <- makeEvent pseudoPluginName
        registerEvent baseEvent (\ e ->
            case e of
                BaseError str -> liftIO $ putStrLn ("billeksah-base error: " ++ str) >> return e
                otherwise     -> return e)
        res <- registerCurrentConfigPath pluginCPath
        case res of
            Nothing -> return ()
            Just str -> triggerBaseEvent (BaseError str) >> return ()

        --  loadListFromConfig first collect all needed plugins, and then
        --  sort the list, so that for every plugin, its prerequisites precede them in the list
        loadList         <- loadListFromConfig baseEvent pluginPath config

        runAll pluginPath config loadList baseEvent)

