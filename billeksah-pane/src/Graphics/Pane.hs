{-# Language CPP #-}
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
    module Graphics.Panes,
    module Graphics.Menu,
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
    fe <- makeEvent pluginName
    return $ PluginInterface {
         piInit1   = frameInit1,
         piInit2   = frameInit2,
         piEvent   = fe,
         piName    = pluginName,
         piVersion = Version [1,0,0][]}

frameInit1 :: BaseEvent -> PEvent FrameEvent -> StateM ()
frameInit1 baseEvent myEvent = trace ("init1 " ++ pluginName) $ do
    return ()

frameInit2 :: BaseEvent -> PEvent FrameEvent -> StateM ()
frameInit2 baseEvent myEvent = trace ("init2 " ++ pluginName) $ do
    uiManager <- liftIO $ do
        unsafeInitGUIForThreadedRTS
        uiManagerNew
    liftIO $ initGtkRc
    res <- registerFrameState (initialFrameState uiManager)
    case res of
        Nothing -> return ()
        Just s -> (evtTrigger baseEvent) (BaseError s) >> return ()
    case res of
        Nothing -> return ()
        Just s -> (evtTrigger baseEvent) (BaseError s) >> return ()

startupFrame :: String -> (Window -> VBox -> Notebook -> StateAction) -> StateAction
startupFrame windowName beforeMainGUI = trace "startupFrame*" $ do
    --    osxApp <- OSX.applicationNew
    reifyState $ \ stateR -> do
        win         <-  trace "1" $ windowNew
        trace "2" $ widgetSetName win windowName
        reflectState (setWindowsSt [win]) stateR

        vb <- vBoxNew False 1  -- Top-level vbox
        widgetSetName vb "topBox"
        trace "before initAction" $ containerAdd win vb


        reflectState initActions stateR
        mbMenuBar <-         trace "after initAction" $ reflectState initMenu stateR
        trace "after initMenu" $
            case mbMenuBar of
                Nothing -> return ()
                Just mb -> boxPackStart vb mb PackNatural 0

        nb          <-  reflectState (newNotebook []) stateR
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
