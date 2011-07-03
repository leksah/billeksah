{-# Language DeriveDataTypeable, StandaloneDeriving, ExistentialQuantification,
    RankNTypes, ScopedTypeVariables #-}
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

module Graphics.Menu (
--    buildMenu
    initActions,
    initMenu
)where

import Base
import Graphics.FrameTypes
import Graphics.Frame
import Graphics.Panes (PaneDirection(..))


import Data.Version (Version(..))
import Debug.Trace (trace)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk
import Control.Monad (foldM)



--------------------------------------------------------------
-- * MenuBar state


{-
getMenuBar :: StateM MenuBar
getMenuBar = getState menuBarStateName

setMenuBar :: MenuBar -> StateM ()
setMenuBar menuBar = setState menuBarStateName menuBar
-}

--
-- | The Actions that can be performed on frames
--
frameActions :: [ActionDescr]
frameActions =
    [AD "Quit" "_Quit" Nothing (Just "gtk-quit")
        quit [] ActionNormal
    ,AD "View" "_View" Nothing Nothing (return ()) [] ActionNormal
    ,AD "ViewMoveLeft" "Move _Left" Nothing Nothing
        (viewMove LeftP) [] ActionNormal
    ,AD "ViewMoveRight" "Move _Right" Nothing Nothing
        (viewMove RightP) [] ActionNormal
    ,AD "ViewMoveUp" "Move _Up" Nothing Nothing
        (viewMove TopP) [] ActionNormal
    ,AD "ViewMoveDown" "Move _Down" Nothing Nothing
        (viewMove BottomP) [] ActionNormal
    ,AD "ViewSplitHorizontal" "Split H_orizontal" Nothing Nothing
        viewSplitHorizontal [] ActionNormal
    ,AD "ViewSplitVertical" "Split _Vertical" Nothing Nothing
        viewSplitVertical [] ActionNormal
    ,AD "ViewCollapse" "_Collapse" Nothing Nothing
        viewCollapse [] ActionNormal
    ,AD "ViewNest" "_Group" Nothing Nothing
        viewNewGroup [] ActionNormal
--    ,AD "ViewDetach" "_Detach" Nothing Nothing
--        viewDetach [] ActionNormal

    ,AD "ViewTabsLeft" "Tabs Left" Nothing Nothing
        (viewTabsPos PosLeft) [] ActionNormal
    ,AD "ViewTabsRight" "Tabs Right" Nothing Nothing
        (viewTabsPos PosRight) [] ActionNormal
    ,AD "ViewTabsUp" "Tabs Up" Nothing Nothing
        (viewTabsPos PosTop) [] ActionNormal
    ,AD "ViewTabsDown" "Tabs Down" Nothing Nothing
        (viewTabsPos PosBottom) [] ActionNormal
    ,AD "ViewSwitchTabs" "Tabs On/Off" Nothing Nothing
        viewSwitchTabs [] ActionNormal

    ,AD "ViewClosePane" "Close pane" Nothing (Just "gtk-close")
        viewClosePane [] ActionNormal

--    ,AD "ToggleToolbar" "Toggle Toolbar" Nothing Nothing
--        toggleToolbar [] False TODO

        ]

initActions :: StateM ()
initActions = do
    RegisterActions l <- triggerFrameEvent (RegisterActions frameActions)
    actionGroup <- liftIO $ actionGroupNew "global"
    mapM_ (buildAction actionGroup) l
    uiManager <- getUiManagerSt
    liftIO $ uiManagerInsertActionGroup uiManager actionGroup 1

  where
    buildAction actionGroup actionDescr =
        let (acc,accString) = case adAccelerators actionDescr of
                                   [] ->  (Just "","=" ++ adName actionDescr)
                                   (ha:r) -> (Just ha, ha ++ "=" ++ adName actionDescr)
        in case adActionType actionDescr of
            ActionNormal -> reifyState $ \ stateR -> do
                act <- actionNew (adName actionDescr)
                    (adLabel actionDescr) (adSynopsis actionDescr) (adStockID actionDescr)
                onActionActivate act (reflectState (adAction actionDescr) stateR)
                actionGroupAddActionWithAccel actionGroup act acc
            ActionToggle -> reifyState $ \ stateR -> do
                act <- toggleActionNew (adName actionDescr)
                    (adLabel actionDescr) (adSynopsis actionDescr) (adStockID actionDescr)
                on act actionToggled (reflectState (adAction actionDescr) stateR)
                actionGroupAddActionWithAccel actionGroup act acc
            ActionSelect (a :: String) ->
                undefined -- TODO

initMenu :: StateM (Maybe MenuBar)
initMenu = do
    initialMenu' <- liftIO $ initialMenu
    mb           <- liftIO $ menuBarNew
    event        <- triggerFrameEvent (RegisterMenu initialMenu')
    case event of
        RegisterMenu [] -> return Nothing
        RegisterMenu l -> mapM_ (buildMenu mb) l >> return (Just mb)
        otherwise -> error "Graphics.Menu>>initMenu: impossible"
  where
    buildMenu mb (mi,pos) = liftIO $ menuShellAppend mb mi

initialMenu :: IO [(MenuItem,Position)]
initialMenu = do
    fileMenu <- menuNew
    quitItem <- menuItemNewWithLabel "Quit"
    menuShellAppend fileMenu quitItem
    fileItem <- menuItemNewWithLabel "File"
    menuItemSetSubmenu fileItem fileMenu
    return [(fileItem,Last)]

getMenuIndex :: MenuBar -> [String] -> IO (Maybe [Int])
getMenuIndex = undefined

