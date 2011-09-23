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
,   PreferencesPane(..)


) where

import Base
import Graphics.Pane
import Graphics.Forms.Basics
import Graphics.Forms.Build
import Graphics.Forms.FormPane

import Graphics.UI.Gtk
import Data.Typeable (Typeable)
import Base.Preferences (savePrefs, setPrefs)

openPreferencesPane ::  StateM ()
openPreferencesPane = do
    message Debug "Open preferences pane"
    _mbPane :: Maybe PreferencesPane <- getOrBuildDisplay (Left []) True
    return ()

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
    primPaneName    =  \ _dp  -> "PreferencesPane"
    paneType        =  \ _   -> "**PreferencesPane"
    saveState       =  \ _s   -> return $ Just PrefPaneState
    recoverState    =  \ pp _ps -> do
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
buildPreferencesPane = \ pp nb window -> do

    allPrefs <-  getState PrefsDescrState
    let descrs = map (\ (s,GenF fdg v) -> (s,GenFG (toFieldDescriptionG fdg) v))
                     allPrefs
    buildGenericFormsPane descrs (formPaneDescr (map fst descrs)) pp nb window
  where
    formPaneDescr categories =
        FormPaneDescr {
            fpGetPane      = \ top inj ext -> PreferencesPane top inj ext,
            fpSaveAction   = \ v -> do
                mapM (\ (str,GenV val) -> setPrefs str val) (zip categories v)
                currentPrefsPath <- getCurrentPrefsPath
                savePrefs currentPrefsPath,
            fpHasChanged   = \ v1 v2 -> v1 == v2,
            fpGuiHandlers  = [],
            fpExtraButtons = []}
