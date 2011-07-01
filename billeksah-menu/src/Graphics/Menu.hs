{-# Language DeriveDataTypeable, StandaloneDeriving #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Menu
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Menu services
--
-----------------------------------------------------------------------------

module Graphics.Menu where

import Base

import Data.Version (Version(..))
import Debug.Trace (trace)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk

-- -----------------------------------------------------------
-- * It's a plugin
--

pluginName = "billeksah-menu"

menuBarStateName = pluginName ++ "." ++ "menubar"

menuPluginInterface :: StateM (PluginInterface MenuEvent)
menuPluginInterface = do
    fe <- makeEvent pluginName
    return $ PluginInterface {
         piInit1   = frameInit1,
         piInit2   = frameInit2,
         piEvent   = fe,
         piName    = pluginName,
         piVersion = Version [1,0,0][]}

deriving instance Typeable MenuBar

frameInit1 :: BaseEvent -> PEvent MenuEvent -> StateM ()
frameInit1 baseEvent myEvent = trace ("init1 " ++ pluginName) $ do
    emptyMenuBar <- liftIO menuBarNew
    registerState menuBarStateName emptyMenuBar
    return ()

frameInit2 :: BaseEvent -> PEvent MenuEvent -> StateM ()
frameInit2 baseEvent myEvent = trace ("init2 " ++ pluginName) $ do
    return ()

data MenuEvent = MenuChanged
    deriving Typeable

triggerMenuEvent :: MenuEvent -> StateM(MenuEvent)
triggerMenuEvent          = triggerEvent pluginName

getMenuEvent :: StateM (PEvent MenuEvent)
getMenuEvent              = getEvent pluginName

registerMenuEvent :: Handler MenuEvent -> StateM HandlerID
registerMenuEvent handler = getMenuEvent >>= \e -> registerEvent e handler

--------------------------------------------------------------
-- | getMenuBar

getMenuBar :: StateM MenuBar
getMenuBar = getState menuBarStateName

setMenuBar :: MenuBar -> StateM ()
setMenuBar menuBar = setState menuBarStateName menuBar

data Position =
    First
    | Last
    | Before [String] (Maybe Position)
    | After [String] (Maybe Position)

addSubMenu :: MenuItem -> Position -> StateM ()
addSubMenu item pos = do
    menuBar <- getMenuBar
    liftIO $  do
        case pos of
            First -> menuShellPrepend menuBar item
            Last  -> menuShellAppend menuBar item
            Before this otherwise -> do
                idx <- getMenuIndex menuBar this
                menuShellInsert  menuBar item idx

getMenuIndex :: MenuBar -> [String] -> IO (Maybe [Int])
getMenuIndex = undefined

