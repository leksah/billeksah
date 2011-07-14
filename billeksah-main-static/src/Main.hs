{-# LANGUAGE EmptyDataDecls, ExistentialQuantification #-}

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

import Base
import Graphics.Pane
import Graphics.Forms
import Leksah (leksahPluginInterface)
import Panes.PluginConfig (pluginPanePluginInterface)

import Base.Plugin


import System.Console.GetOpt
       (OptDescr, usageInfo, ArgOrder(..), getOpt)
import System.Environment (getArgs)
import System.FilePath (dropFileName)
import Data.IORef (newIORef)
import qualified Data.Map as Map (fromList, empty)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map (Map)
import Data.Typeable (Typeable)
import Leksah.Dummy (dummyPluginInterface)


data Flag

header :: String
header = "Usage: billeksah-main-static [OPTION...] pluginConfig"

options :: [OptDescr Flag]
options = []


pluginTable :: Map String GenInterfaceM
pluginTable = Map.fromList [
    ("billeksah-forms", GenInterfaceM formsPluginInterface),
    ("billeksah-pane", GenInterfaceM panePluginInterface),
    ("leksah-main", GenInterfaceM leksahPluginInterface),
    ("leksah-plugin-pane", GenInterfaceM pluginPanePluginInterface),
    ("leksah-dummy", GenInterfaceM dummyPluginInterface)]

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
        baseEvent    <- makeEvent MainEventSel
        registerEvent'  baseEvent (\ e -> case e of
                                            BaseError str -> liftIO $ putStrLn ("billeksah-base error: " ++ str)
                                            otherwise -> return ())
        res <- registerCurrentConfigPath pluginCPath
        case res of
            Nothing -> return ()
            Just str -> triggerBaseEvent (BaseError str) >> return ()

        --  loadListFromConfig first collect all needed plugins, and then
        --  sort the list, so that for every plugin, its prerequisites precede them in the list
        loadList         <- loadListFromConfig baseEvent pluginPath config

        runAll pluginPath config loadList baseEvent pluginTable)

