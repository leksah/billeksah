{-# Language ScopedTypeVariables, TypeFamilies, DeriveDataTypeable,
    ExistentialQuantification #-}

-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Panes.Preferences
-- Copyright   :  (c) Juergen Nicklisch-Franken
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
--
-- | Module for visually editing generic preferences
--
-----------------------------------------------------------------------------------

module Graphics.Panes.Preferences (

    openPreferencesPane
,   PreferencesPane

) where

import Base
import Graphics.Pane
import Graphics.Forms.Basics
import Graphics.Forms.Build
import Graphics.Forms.FormPane
import Graphics.Forms.GUIEvent

import Graphics.UI.Gtk
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (readIORef, writeIORef, newIORef)
import Graphics.UI.Gtk.General.Enums
       (WindowPosition(..), Packing(..), PolicyType(..))
import Graphics.UI.Gtk.Windows.MessageDialog
       (ButtonsType(..), MessageType(..))
import System.Glib.Attributes (AttrOp(..), set)
import Control.Monad (when)
import Debug.Trace (trace)

openPreferencesPane ::  StateM ()
openPreferencesPane = do
    message Debug "Open preferences pane"
    mbPane :: Maybe PreferencesPane <- getOrBuildDisplay (Left []) True
    case mbPane of
        Nothing -> return ()
        Just p ->  {-- registerRefresh p  >> --} return ()
                    -- Handle changes to preferences, while the pane is open.
    return ()

--registerRefresh pane = undefined
--getPluginPaneEvent >>= (\e -> registerEvent' e handler)
--  where
--    handler PluginDescrChanged =  do
--        mbV <-  pcpExt pane
--        case mbV of
--            Nothing -> return ()
--            Just v -> do
--                currentConfigPath   <- getCurrentConfigPath
--                prerequisites       <- liftIO $ getPrereqChoices
--                                            currentConfigPath
--                let pluginConfig'   =  v{cfChoices = prerequisites
--                                                     ++ cfPlugins v}
--                (pcpInj pane) pluginConfig'
--    handler otherwise          =  return ()

-- ----------------------------------------------
-- * It's a pane
--

data PreferencesPane = PreferencesPane {
    prpTopW             :: VBox,
    prpInj              :: Injector [GenValue],
    prpExt              :: Extractor [GenValue]
} deriving Typeable


instance PaneInterface PreferencesPane where
    data PaneState PreferencesPane =  PrefPaneState
            deriving(Read,Show)

    getTopWidget    =  \ p   -> castToWidget (prpTopW p)
    primPaneName    =  \ dp  -> "PreferencesPane"
    paneType        =  \ _   -> "**PreferencesPane"
    saveState       =  \ s   -> return $ Just PrefPaneState
    recoverState    =  \ pp ps -> do
        nb      <-  getNotebook pp
        mbP     <-  buildPane pp nb builder
        return mbP
    builder         =  buildPreferencesPane

instance Pane PreferencesPane



-- ----------------------------------------------
-- * Building the pane in standard form
--
buildPreferencesPane :: PanePath -> Notebook -> Window
                            -> StateM (Maybe PreferencesPane, Connections)
buildPreferencesPane = \ pp nb window -> trace "1" $ do

    allPrefs <-  getState PrefsDescrState
    let descrs = map (\ (s,GenF fdg v) -> (s,GenFG (toFieldDescriptionG fdg) v))
                     allPrefs
    trace "5" $ (buildGenericFormsPane descrs formPaneDescr) pp nb window
  where
    formPaneDescr :: FormPaneDescr [GenValue] PreferencesPane =
        FormPaneDescr {
            fpGetPane      = \ top inj ext -> PreferencesPane top inj ext,
            fpSaveAction   = \ v -> do
                currentConfigPath <- getCurrentConfigPath
--                liftIO $ writePluginConfig currentConfigPath v
--                triggerPluginPane PluginConfigChanged
                return (),
            fpHasChanged   = \ v1 v2 -> v1 == v2,
            fpGuiHandlers  = handlers,
            fpExtraButtons = []}
    handlers = []

