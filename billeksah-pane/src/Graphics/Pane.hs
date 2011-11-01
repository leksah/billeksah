{-# Language CPP, TypeFamilies, RankNTypes, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Pane
-- Copyright   :  Juergen Nicklisch-Franken
-- License     :  LGPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :  portabel
--
-- |
--
-----------------------------------------------------------------------------------

module Graphics.Pane (
    startupFrame,
    setSensitivity,
    panePluginInterface,

    module Graphics.Frame,
    module Graphics.FrameTypes,
    module Graphics.Panes,
    module Graphics.Session

) where

import Base
import Graphics.FrameTypes
import Graphics.Frame
import Graphics.Panes
import Graphics.Menu
import Graphics.Session
import Graphics.Statusbar

import Graphics.UI.Gtk
import Data.Version (Version(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent (yield)
import Control.Monad (when)
import Data.Typeable (Typeable)

-- -----------------------------------------------------------
-- * It's a plugin
--

panePluginInterface :: StateM (PluginInterface FrameEvent)
panePluginInterface = do
    fe <- makeEvent FrameEventSel
    return $ PluginInterface {
         piInit1   = frameInit1,
         piInit2   = frameInit2,
         piEvent   = fe,
         piName    = panePluginName,
         piVersion = Version [1,0,0][]}

frameInit1 :: BaseEvent -> EventChannel FrameEvent -> StateM ()
frameInit1 baseEvent myEvent = do
    message Debug ("init1 " ++ panePluginName)
    res <- registerActionState initialActionState
    case res of
        Nothing -> return ()
        Just s ->  message Error s
    return ()

frameInit2 :: BaseEvent -> EventChannel FrameEvent -> StateM ()
frameInit2 baseEvent myEvent = do
    message Debug ("init2 " ++ panePluginName)
    uiManager <- reifyState (\stateR -> do
        res <- unsafeInitGUIForThreadedRTS
--        res <- initGUI
        messageR Debug ("initGUI " ++ show res) stateR
        uiManagerNew)
    liftIO $ initGtkRc
    res <- registerFrameState (initialFrameState uiManager)
    case res of
        Nothing -> return ()
        Just s ->  message Error s
    getFrameEvent >>= \e -> registerEvent' e
        (\s -> case s of
            ActivatePane pn  -> do  setSensitivity [(PaneActiveSens, True)]
                                    setStatusText "SBActivePane" pn
            DeactivatePane _ -> do  setSensitivity [(PaneActiveSens, False)]
                                    setStatusText "SBActivePane" ""
            _ -> return ())
    return ()

data PaneActiveSens = PaneActiveSens
    deriving (Eq, Ord, Show, Typeable)

instance Selector PaneActiveSens where
    type ValueType PaneActiveSens = Bool


--
-- | The Actions that can be performed on frames
--
frameActions :: [ActionDescr]
frameActions =
    [AD "File" "_File" Nothing Nothing (return ()) Nothing ActionSubmenu
            (Just $ MPFirst []) Nothing []
    ,AD "Quit" "_Quit" Nothing (Just "gtk-quit") quit Nothing ActionNormal
            (Just $ MPFirst ["File"]) Nothing []
    ,AD "View" "_View" Nothing Nothing (return ()) Nothing ActionSubmenu
            (Just $ MPLast [] False) Nothing []
    ,AD "ViewMoveLeft" "Move _Left" Nothing Nothing
            (viewMove LeftP) (Just "<alt><shift>Left") ActionNormal
            (Just $ MPFirst ["View"]) Nothing [GS PaneActiveSens]
    ,AD "ViewMoveRight" "Move _Right" Nothing Nothing
            (viewMove RightP) (Just "<alt><shift>Right") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewMoveUp" "Move _Up" Nothing Nothing
            (viewMove TopP) (Just "<alt><shift>Up") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewMoveDown" "Move _Down" Nothing Nothing
            (viewMove BottomP) (Just "<alt><shift>Down") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewSplitHorizontal" "Split H_orizontal" Nothing Nothing
            viewSplitHorizontal (Just "<ctrl>2") ActionNormal
            (Just $ MPAppend True) Nothing [GS PaneActiveSens]
    ,AD "ViewSplitVertical" "Split _Vertical" Nothing Nothing
            viewSplitVertical (Just "<ctrl>3") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewCollapse" "_Collapse" Nothing Nothing
            viewCollapse (Just "<ctrl>1") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewNest" "_Group" Nothing Nothing
            viewNewGroup Nothing ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewDetach" "_Detach" Nothing Nothing
            viewDetachInstrumented Nothing ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewTabsLeft" "Tabs Left" Nothing Nothing
            (viewTabsPos PosLeft) (Just "<alt><shift><ctrl>Left") ActionNormal
            (Just $ MPAppend True) Nothing[GS PaneActiveSens]
    ,AD "ViewTabsRight" "Tabs Right" Nothing Nothing
            (viewTabsPos PosRight) (Just "<alt><shift><ctrl>Right") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewTabsUp" "Tabs Up" Nothing Nothing
        (viewTabsPos PosTop) (Just "<alt><shift><ctrl>Up") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewTabsDown" "Tabs Down" Nothing Nothing
        (viewTabsPos PosBottom) (Just "<alt><shift><ctrl>Down") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]
    ,AD "ViewSwitchTabs" "Tabs On/Off" Nothing Nothing
        viewSwitchTabs (Just "<shift><ctrl>t") ActionNormal
            (Just $ MPAppend False) Nothing [GS PaneActiveSens]

    ,AD "ViewClosePane" "Close pane" Nothing (Just "gtk-close")
        viewClosePane (Just "<ctrl><shift>q") ActionNormal
            (Just $ MPAppend True) (Just TPFirst) [GS PaneActiveSens]

    ,AD "ToolbarVisible" "Toolbar visible" Nothing Nothing
        toggleToolbar (Just "<alt><shift>t") ActionToggle (Just $ MPAppend True) Nothing []
        ]

frameCompartments = [
    TextCompDescr "SBActions" False 150 PackGrow CPLast,
    TextCompDescr "SBActivePane" False 150 PackGrow CPLast]

--
-- | Opens up the main window, with menu, toolbar, accelerators
--
startupFrame :: String -> (Window -> VBox -> Notebook -> StateAction) -> StateAction
startupFrame windowName beforeMainGUI = do
    message Debug "startupFrame"
    --    osxApp <- OSX.applicationNew
    uiManager <- getUiManagerSt
    RegisterActions allActions       <- triggerFrameEvent (RegisterActions frameActions)
    RegisterPane allPanes            <- triggerFrameEvent (RegisterPane [])
    setPaneTypes allPanes
    RegisterSessionExt sessionExt    <- triggerFrameEvent (RegisterSessionExt [])
    setSessionExt sessionExt
    (menuBar,toolbar)   <- initActions uiManager allActions
    setSensitivity [(PaneActiveSens, False)]
    tbv <- toolbarVisible
    setToolbar (Just toolbar)
    showToolbar True
    RegisterStatusbarComp allCompartments <- triggerFrameEvent (RegisterStatusbarComp frameCompartments)
    statusbar <- buildStatusbar allCompartments
    reifyState $ \ stateR -> do

        win         <-  windowNew
        accGroup <- uiManagerGetAccelGroup uiManager
        windowAddAccelGroup win accGroup
        widgetSetName win windowName
        reflectState (setWindowsSt [win]) stateR

        vb <- vBoxNew False 1  -- Top-level vbox
        widgetSetName vb "topBox"
        containerAdd win vb

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

        boxPackEnd vb statusbar PackNatural 0

        widgetShowAll win
        timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- maybe switch back to
        reflectState (beforeMainGUI win vb nb) stateR

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

viewDetachInstrumented :: StateM ()
viewDetachInstrumented = do
    mbPair <- viewDetach
    case mbPair of
        Nothing     -> return ()
        Just (win,wid) -> do
            instrumentSecWindow win
            liftIO $ widgetShowAll win

-- TODO Menu, Toolbar, Key accelerators, Statusbar ??? for other windows
instrumentSecWindow :: Window -> StateM ()
instrumentSecWindow win = return ()

