{-# Language CPP, TypeFamilies, RankNTypes #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Pane
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- |
--
-----------------------------------------------------------------------------------

module Graphics.Pane (
    startupFrame,
    module Graphics.Frame,
    module Graphics.FrameTypes,
    module Graphics.Panes,
    setSensitivity,
    panePluginInterface
) where

import Base
import Graphics.FrameTypes
import Graphics.Frame
import Graphics.Panes
import Graphics.Menu

import Graphics.UI.Gtk
import Debug.Trace (trace)
import Data.Version (Version(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent (yield)

-- -----------------------------------------------------------
-- * It's a plugin
--

panePluginInterface :: StateM (PluginInterface FrameEvent)
panePluginInterface = do
    fe <- makeEvent panePluginName
    return $ PluginInterface {
         piInit1   = frameInit1,
         piInit2   = frameInit2,
         piEvent   = fe,
         piName    = panePluginName,
         piVersion = Version [1,0,0][]}

frameInit1 :: BaseEvent -> PEvent FrameEvent -> StateM ()
frameInit1 baseEvent myEvent = trace ("init1 " ++ panePluginName) $ do
    res <- registerActionState initialActionState
    case res of
        Nothing -> return ()
        Just s -> (evtTrigger baseEvent) (BaseError s) >> return ()
    return ()

frameInit2 :: BaseEvent -> PEvent FrameEvent -> StateM ()
frameInit2 baseEvent myEvent = trace ("init2 " ++ panePluginName) $ do
    uiManager <- liftIO $ do
        unsafeInitGUIForThreadedRTS
        uiManagerNew
    liftIO $ initGtkRc
    res <- registerFrameState (initialFrameState uiManager)
    case res of
        Nothing -> return ()
        Just s -> (evtTrigger baseEvent) (BaseError s) >> return ()
    registerFrameEvent (\ e -> case e of
                            ActivatePane _ -> setSensitivity [(PaneActiveSens, True)] >> return e
                            otherwise -> return e)
    registerFrameEvent (\ e -> case e of
                            DeactivatePane _ -> setSensitivity [(PaneActiveSens, False)] >> return e
                            otherwise -> return e)
    return ()


--
-- | The Actions that can be performed on frames
--
frameActions :: [ActionDescr]
frameActions =
    [AD "File" "_File" Nothing Nothing (return ()) Nothing ActionSubmenu (MPFirst []) TPNo []
    ,AD "Quit" "_Quit" Nothing (Just "gtk-quit")
        quit Nothing ActionNormal (MPFirst ["File"]) TPNo []
    ,AD "View" "_View" Nothing Nothing (return ()) Nothing ActionSubmenu (MPLast [] False) TPNo []
    ,AD "ViewMoveLeft" "Move _Left" Nothing Nothing
        (viewMove LeftP) (Just "<alt><shift>Left") ActionNormal (MPFirst ["View"]) TPNo [GS PaneActiveSens]
    ,AD "ViewMoveRight" "Move _Right" Nothing Nothing
        (viewMove RightP) (Just "<alt><shift>Right") ActionNormal (MPAppend False) TPNo [GS PaneActiveSens]
    ,AD "ViewMoveUp" "Move _Up" Nothing Nothing
        (viewMove TopP) (Just "<alt><shift>Up") ActionNormal (MPAppend False) TPNo [GS PaneActiveSens]
    ,AD "ViewMoveDown" "Move _Down" Nothing Nothing
        (viewMove BottomP) (Just "<alt><shift>Down") ActionNormal (MPAppend False) TPNo [GS PaneActiveSens]
    ,AD "ViewSplitHorizontal" "Split H_orizontal" Nothing Nothing
        viewSplitHorizontal (Just "<ctrl>2") ActionNormal (MPAppend True) TPNo [GS PaneActiveSens]
    ,AD "ViewSplitVertical" "Split _Vertical" Nothing Nothing
        viewSplitVertical (Just "<ctrl>3") ActionNormal (MPAppend False) TPNo [GS PaneActiveSens]
    ,AD "ViewCollapse" "_Collapse" Nothing Nothing
        viewCollapse (Just "<ctrl>1") ActionNormal (MPAppend False) TPNo [GS PaneActiveSens]
    ,AD "ViewNest" "_Group" Nothing Nothing
        viewNewGroup Nothing ActionNormal (MPAppend False) TPNo [GS PaneActiveSens]
--    ,AD "ViewDetach" "_Detach" Nothing Nothing
--        viewDetach [] ActionNormal

    ,AD "ViewTabsLeft" "Tabs Left" Nothing Nothing
        (viewTabsPos PosLeft) (Just "<alt><shift><ctrl>Left") ActionNormal (MPAppend True) TPNo
            [GS PaneActiveSens]
    ,AD "ViewTabsRight" "Tabs Right" Nothing Nothing
        (viewTabsPos PosRight) (Just "<alt><shift><ctrl>Right") ActionNormal (MPAppend False) TPNo
            [GS PaneActiveSens]
    ,AD "ViewTabsUp" "Tabs Up" Nothing Nothing
        (viewTabsPos PosTop) (Just "<alt><shift><ctrl>Up") ActionNormal (MPAppend False) TPNo
            [GS PaneActiveSens]
    ,AD "ViewTabsDown" "Tabs Down" Nothing Nothing
        (viewTabsPos PosBottom) (Just "<alt><shift><ctrl>Down") ActionNormal (MPAppend False) TPNo
            [GS PaneActiveSens]
    ,AD "ViewSwitchTabs" "Tabs On/Off" Nothing Nothing
        viewSwitchTabs (Just "<shift><ctrl>t") ActionNormal (MPAppend False) TPNo
            [GS PaneActiveSens]

    ,AD "ViewClosePane" "Close pane" Nothing (Just "gtk-close")
        viewClosePane (Just "<ctrl><shift>q") ActionNormal (MPAppend True) TPFirst
            [GS PaneActiveSens]

--    ,AD "ToggleToolbar" "Toggle Toolbar" Nothing Nothing
--        toggleToolbar ["<ctrl>t"] False TODO

        ]

--
-- | Opens up the main window, with menu, toolbar, accelerators
--
startupFrame :: String -> (Window -> VBox -> Notebook -> StateAction) -> StateAction
startupFrame windowName beforeMainGUI = trace "startupFrame*" $ do
    --    osxApp <- OSX.applicationNew
    uiManager <- getUiManagerSt
    RegisterActions l <- triggerFrameEvent (RegisterActions frameActions)
    (menuBar,toolbar)   <- initActions uiManager l
    setSensitivity [(PaneActiveSens, False)]
    reifyState $ \ stateR -> do

        win         <-  trace "1" $ windowNew
        accGroup <- uiManagerGetAccelGroup uiManager
        windowAddAccelGroup win accGroup
        trace "2" $ widgetSetName win windowName
        reflectState (setWindowsSt [win]) stateR

        vb <- vBoxNew False 1  -- Top-level vbox
        widgetSetName vb "topBox"
        trace "before initAction" $ containerAdd win vb

        boxPackStart vb menuBar PackNatural 0

        toolbarSetIconSize toolbar IconSizeButton
        toolbarSetStyle toolbar ToolbarBothHoriz
        widgetSetSizeRequest toolbar 700 (-1)
        boxPackStart vb toolbar PackNatural 0

        nb <- reflectState (newNotebook []) stateR
        afterSwitchPage nb (\i -> reflectState (handleNotebookSwitch nb i) stateR)
        widgetSetName nb "root"
        win `onDelete` (\ _ -> do reflectState quit stateR; return True)
        boxPackStart vb nb PackGrow 0

        reflectState (beforeMainGUI win vb nb) stateR
        widgetShowAll win
        timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- maybe switch back to

        mainGUI
        return ()

--
-- | Set gtk style - call that function once at initialization
--
initGtkRc :: IO ()
#if MIN_VERSION_gtk(0,11,0)
initGtkRc = rcParseString ("style \"leksah-close-button-style\"\n" ++
    "{\n" ++
    "  GtkWidget::focus-padding = 0\n" ++
    "  GtkWidget::focus-line-width = 0\n" ++
    "  xthickness = 0\n" ++
    "  ythickness = 0\n" ++
    "}\n" ++
    "widget \"*.leksah-close-button\" style \"leksah-close-button-style\"")
#else
initGtkRc = return ()
#endif
