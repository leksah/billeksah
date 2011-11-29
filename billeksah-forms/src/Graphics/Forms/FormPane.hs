{-# Language RankNTypes #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Forms.FormPane
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | A standard builder for a pane with a form
--
-----------------------------------------------------------------------------------

module Graphics.Forms.FormPane (
    FormPaneDescr(..),
    buildFormsPane,
    buildGenericFormsPane,
    SimpleFormPaneDescr(..),
    buildSimpleFormsPane,
) where

import Base

import Graphics.Forms.Build
       (GenFieldDescriptionG(..), GenFieldDescriptionG,
        buildGenericEditor, FieldDescriptionG, buildEditor)
import Graphics.Forms.GUIEvent
       (registerGUIEvent, dummyGUIEvent, triggerGUIEvent)
import Graphics.Forms.Basics
       (GEvent, GenValue(..), Extractor, Injector, GUIEvent(..),
        GUIEventSelector(..))
import Graphics.Pane

import Graphics.UI.Gtk
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (when)


-- | A description for a forms pane, which is targeted as an editor,
-- with save and cancel buttons

data FormPaneDescr alpha beta  =  FormPaneDescr {
    fpGetPane      :: Pane beta  => VBox -> Injector alpha -> Extractor alpha -> GEvent -> beta , -- ^ Construct the pane data type
    fpSaveAction   :: alpha -> StateM (),                                   -- ^ Called when the save button is hit
    fpHasChanged   :: alpha -> alpha -> Bool,                                   -- ^ Judge if this qualify as a change
    fpGuiHandlers  :: [([GUIEventSelector],Handler GUIEvent)],          -- ^ Handle GUI Events triggered
    fpExtraButtons :: [(String,StateM ())]}                             -- ^ Add extra buttons with handlers

--
-- | Returns a builder for a pane
-- Requires a forms description, an initial value and a FormPaneDescr
buildFormsPane :: Pane beta  => FieldDescriptionG alpha  ->  alpha  -> FormPaneDescr alpha beta
                        -> (PanePath -> Notebook -> Window -> StateM (Maybe beta, Connections))
buildFormsPane descr val formDescr =
    buildFormsPanePrim descr (buildEditor descr val) formDescr val

--
-- | Returns a builder for a pane
-- Requires a forms description, an initial value and a FormPaneDescr
buildGenericFormsPane :: Pane beta  => [(String,GenFieldDescriptionG)]  -> FormPaneDescr [GenValue] beta
    -> (PanePath -> Notebook -> Window -> StateM (Maybe beta, Connections))
buildGenericFormsPane descr formDescr =
    buildFormsPanePrim descr (buildGenericEditor descr) formDescr
        (map (\ (_s,GenFG _ v) -> GenV v) descr)

-- | A description for a forms pane, which has no standard buttons and no change detection

data SimpleFormPaneDescr alpha beta  =  SimpleFormPaneDescr {
    sfpGetPane      :: Pane beta  => ScrolledWindow -> Injector alpha -> Extractor alpha -> GEvent -> beta}
            -- ^ Construct the pane data type

--
-- | Returns a builder for a pane
-- Requires a forms description, an initial value and a FormPaneDescr
buildSimpleFormsPane :: Pane beta  => FieldDescriptionG alpha  ->  alpha  -> SimpleFormPaneDescr alpha beta
                        -> (PanePath -> Notebook -> Window -> StateM (Maybe beta, Connections))
buildSimpleFormsPane descr val formDescr =
    buildSimpleFormsPanePrim descr (buildEditor descr val) formDescr val


buildFormsPanePrim
  :: (WidgetClass child, Pane a) =>
     t
     -> StateM
          (child,
           Injector alpha,
           alpha -> Extractor alpha,
           GEvent)
     -> FormPaneDescr alpha a
     -> alpha
     -> t1
     -> t2
     -> Window
     -> StateM (Maybe a, [ConnectId Widget])
buildFormsPanePrim _descr editor formDescr val = \ _panePath _notebook window -> do
    reifyState (\ stateR -> do
        ------------------------------------------
        -- Plugin editor
        lastSaved           <- liftIO $ newIORef val
        (widget,inj,ext,notifier) <- reflectState editor stateR
        sw <- scrolledWindowNew Nothing Nothing
        scrolledWindowAddWithViewport sw widget
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

        -- Buttons
        bb                  <-  hButtonBoxNew
        saveB               <-  buttonNewFromStock "gtk-save"
        revertB             <-  buttonNewFromStock "gtk-revert-to-saved"
        closeB              <-  buttonNewFromStock "gtk-close"

        extraButtons        <-  mapM buttonNewFromStock (map fst (fpExtraButtons formDescr))

        widgetSetSensitive saveB False
        widgetSetSensitive revertB False

        mapM (boxPackStartDefaults bb) extraButtons

        boxPackStartDefaults bb revertB
        boxPackStartDefaults bb saveB
        boxPackStartDefaults bb closeB

        validationLabel <- labelNew Nothing

        --Frame
        vb                  <-  vBoxNew False 0
        boxPackStart vb sw PackGrow 7
        boxPackStart vb validationLabel PackNatural 0
        boxPackEnd vb bb PackNatural 7

        let inj2 = do
            (\b -> do
                    liftIO $ writeIORef lastSaved b
                    inj b)

        let pane  = (fpGetPane formDescr) vb inj2 (ext val) notifier

        --Events
        cid1 <- saveB `onClicked` (do
            mbV <- reflectState (ext val) stateR
            case mbV of
                Nothing -> return ()
                Just v -> do
                    reflectState (do
                        (fpSaveAction formDescr) v
                        liftIO $ writeIORef lastSaved v
                        triggerGUIEvent notifier dummyGUIEvent{geSelector= MayHaveChanged}) stateR
                    return ())

        cid2 <- revertB `onClicked` (do
            old <- readIORef lastSaved
            reflectState (inj old) stateR)

        cid3 <- closeB `onClicked` do
            (hasChanged',_) <- reflectState (hasChanged ext lastSaved) stateR
            if not hasChanged'
                then reflectState (closePane pane >> return ()) stateR
                else do
                    md <- messageDialogNew (Just window) []
                        MessageQuestion
                        ButtonsYesNo
                        "Unsaved changes. Close anyway?"
                    set md [ windowWindowPosition := WinPosCenterOnParent ]
                    resp <- dialogRun md
                    widgetDestroy md
                    case resp of
                        ResponseYes ->   do
                            reflectState (closePane pane >> return ()) stateR
                        _  ->   return ()

        cids <- mapM (\ (button,handler) -> button `onClicked` (reflectState handler stateR))
            (zip extraButtons (map snd (fpExtraButtons formDescr)))

        reflectState (do
            registerGUIEvent notifier [MayHaveChanged] (\ e -> do
                (hasChanged',canExtract) <-  hasChanged ext lastSaved
                when canExtract $ liftIO $ labelSetMarkup validationLabel ""
                setChanged pane hasChanged'
                liftIO $ widgetSetSensitive saveB hasChanged'
                liftIO $ widgetSetSensitive revertB hasChanged'
                return (e{geGtkReturn=False}))

            registerGUIEvent notifier [ValidationError] (\e -> do
                liftIO $ labelSetMarkup validationLabel
                    ("<span foreground=\"red\" size=\"large\">" ++
                        "The following fields have invalid values: "
                        ++ geText e ++ "</span>")
                return e)

            mapM_ (\ (selectors,handler) -> registerGUIEvent notifier selectors handler)
                (fpGuiHandlers formDescr)) stateR

        return (Just pane, map castCID ([cid1, cid2, cid3] ++ cids)))
  where
    hasChanged ext lastSavedRef = do
        old <- liftIO $ readIORef lastSavedRef
        mbP <- ext old
        return $ case mbP of
                    Nothing -> (False,False)
                    Just p -> (not ((fpHasChanged formDescr) p old), True)


buildSimpleFormsPanePrim
  :: (WidgetClass child, Pane a) =>
     t
     -> StateM
          (child,
           Injector alpha,
           alpha -> Extractor alpha,
           GEvent)
     -> SimpleFormPaneDescr alpha a
     -> alpha
     -> t1
     -> t2
     -> Window
     -> StateM (Maybe a, [ConnectId Widget])
buildSimpleFormsPanePrim _descr editor formDescr val = \ _panePath _notebook _ -> do
    reifyState (\ stateR -> do
        ------------------------------------------
        (widget,inj,ext,notifier) <- reflectState editor stateR
        sw <- scrolledWindowNew Nothing Nothing
        scrolledWindowAddWithViewport sw widget
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

        --Frame

        let pane  = (sfpGetPane formDescr) sw inj (ext val) notifier

        return (Just pane,[]))
  where
