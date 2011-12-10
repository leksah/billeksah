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
import Base

import System.Console.GetOpt
       (ArgDescr(..), OptDescr(..), usageInfo, ArgOrder(..),
        getOpt)
import System.Environment (getArgs)
import System.FilePath (dropFileName)
import Data.IORef (newIORef)
import qualified Data.Map as Map (empty)
import Base
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (catMaybes)
import Control.Monad (when)
import Data.Version (showVersion)
import Paths_billeksah_main (version)
import System.Exit (exitSuccess)
import Data.Foldable (find)
import System.IO (stdout, hFlush)

data Flag =    Help
             | Version
             | Verbosity String
       deriving (Show,Eq)

options :: [OptDescr Flag]

options =   [
             Option ['e'] ["verbosity"] (ReqArg Verbosity "Verbosity")
                "One of Debug, Info, Warning, Error"

         ,   Option ['v'] ["version"] (NoArg Version)
                "Show the version number of plugin system"
         ,   Option ['h'] ["help"] (NoArg Help)
                "Display command line options"]


header :: String
header = "Usage: billeksah-base [OPTION...] pluginConfig"


getOpts :: [String] -> IO ([Flag], [String])
getOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options

main = do
    args             <- getArgs
    (o,files)        <- getOpts args
    let verbosity'   =  case find (\x -> case x of
                                            Verbosity s -> True
                                            _           -> False) o of
                            Nothing            -> Info
                            Just (Verbosity v) -> case maybeRead v of
                                                    Just v' -> v'
                                                    Nothing -> error $
                                                        "Main>>main: unknown verbosity "
                                                             ++ v
    when (elem Version o) $ do
        putStrLn $ "billeksah-base, version " ++ showVersion version
        exitSuccess
    when (elem Help o) $ do
        putStrLn $ "billeksah-base plugin system, " ++ usageInfo header options
        exitSuccess

    let pluginCPath  =  case files of
                            i:_ -> i
                            _   -> error "Required arg missing: pluginConfig"
        pluginPath   =  dropFileName pluginCPath
    config       <- loadPluginConfig pluginCPath
    runState (do
        registerMessageLevel verbosity'
        baseEvent    <- makeEvent BaseEventSel
        registerEvent' baseEvent (\ e ->
            case e of
                BaseLog level str -> do
                    liftIO $ putStrLn (show level ++ " " ++ str)
                    liftIO $ hFlush stdout
                otherwise     -> return ())
        res <- registerCurrentConfigPath pluginCPath
        case res of
            Nothing -> return ()
            Just str -> message Error str

        --  loadListFromConfig first collect all needed plugins, and then
        --  sort the list, so that for every plugin, its prerequisites precede them in the list
        loadList         <- loadListFromConfig baseEvent pluginPath config

        runAll pluginPath config loadList baseEvent)

