-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Composite
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for making composite editors
--
-----------------------------------------------------------------------------------

module Graphics.Forms.Composite (
    maybeEditor
,   disableEditor
,   pairEditor
,   tupel3Editor
,   splitEditor
,   eitherOrEditor
,   multisetEditor
,   selectionEditor
,   ColumnDescr(..)

,   filesEditor
,   stringsEditor

,   versionEditor
--,   versionRangeEditor
--,   dependencyEditor
--,   dependenciesEditor
) where

import Graphics.Forms.Parameters
import Graphics.Forms.Basics
import Graphics.Forms.Build
import Graphics.Forms.Simple
import Graphics.Forms.GUIEvent
import Graphics.Forms.Default
import Base.State
import Base.MyMissing (forceJust)
import Graphics.Pane (Direction(..), castCID)



import Control.Monad.IO.Class(liftIO)
import Graphics.UI.Gtk
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.List (nubBy, sortBy, nub, sort, elemIndex)
import Distribution.Simple
    (PackageName(..))
import Distribution.Text (simpleParse, display)
import Data.Version (Version(..))
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk (Event(..))
import Data.Typeable (Typeable)


--
-- | An editor which composes two subeditors
--
pairEditor :: (Editor alpha, Parameters) -> (Editor beta, Parameters) -> Editor (alpha,beta)
pairEditor (fstEd,fstPara) (sndEd,sndPara) parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    noti1   <- makeGUIEvent
    noti2   <- makeGUIEvent
    propagateGUIEvent notifier [noti1,noti2] allGUIEvents
    fst@(fstFrame,inj1,_ext1) <- fstEd fstPara noti1
    snd@(sndFrame,inj2,_ext2) <- sndEd sndPara noti2
    mkEditor
        (\widget (v1,v2) -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- case getPara "Direction" parameters of
                        ParaDir Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        ParaDir Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                        _ -> error "Composite>>pairEditor"
                    boxPackStart box fstFrame PackGrow 0
                    boxPackStart box sndFrame PackGrow 0
                    containerAdd widget box
                    reflectState (do
                        inj1 v1
                        inj2 v2) stateR
                    writeIORef coreRef (Just (fst,snd))
                Just ((_,inj1,_),(_,inj2,_)) -> do
                    reflectState (do
                        inj1 v1
                        inj2 v2) stateR)
        (do
            core <- liftIO $ readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    if isJust r1 && isJust r2
                        then return (Just (fromJust r1,fromJust r2))
                        else return Nothing)
        parameters
        notifier

tupel3Editor :: (Editor alpha, Parameters)
    -> (Editor beta, Parameters)
    -> (Editor gamma, Parameters)
    -> Editor (alpha,beta,gamma)
tupel3Editor p1 p2 p3 parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    noti1   <- makeGUIEvent
    noti2   <- makeGUIEvent
    noti3   <- makeGUIEvent
    propagateGUIEvent notifier [noti1,noti2,noti3] (Clicked : allGUIEvents)
    r1@(frame1,inj1,_ext1) <- (fst p1) (snd p1) noti1
    r2@(frame2,inj2,_ext2) <- (fst p2) (snd p2) noti2
    r3@(frame3,inj3,_ext3) <- (fst p3) (snd p3) noti3
    mkEditor
        (\widget (v1,v2,v3) -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- case getPara "Direction" parameters of
                        ParaDir Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        ParaDir Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                        _ -> error "Composite>>tupel3Editor"
                    boxPackStart box frame1 PackGrow 0
                    boxPackStart box frame2 PackGrow 0
                    boxPackStart box frame3 PackGrow 0
                    containerAdd widget box
                    reflectState (do
                        inj1 v1
                        inj2 v2
                        inj3 v3) stateR
                    writeIORef coreRef (Just (r1,r2,r3))
                Just ((_,inj1,_),(_,inj2,_),(_,inj3,_)) -> do
                    reflectState (do
                        inj1 v1
                        inj2 v2
                        inj3 v3) stateR)
        (do core <- liftIO $ readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2),(_,_,ext3)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    r3 <- ext3
                    if isJust r1 && isJust r2 && isJust r3
                        then return (Just (fromJust r1,fromJust r2, fromJust r3))
                        else return Nothing)
        parameters
        notifier

--
-- | Like a pair editor, but with a moveable split
--
splitEditor :: (Editor alpha, Parameters) -> (Editor beta, Parameters) -> Editor (alpha,beta)
splitEditor (fstEd,fstPara) (sndEd,sndPara) parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    noti1   <- makeGUIEvent
    noti2   <- makeGUIEvent
    propagateGUIEvent notifier [noti1,noti2] allGUIEvents
    fst@(fstFrame,inj1,_ext1) <- fstEd fstPara noti1
    snd@(sndFrame,inj2,_ext2) <- sndEd sndPara noti2
    mkEditor
        (\widget (v1,v2) -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    paned <- case getPara "Direction" parameters of
                        ParaDir Horizontal  -> do  h <- vPanedNew
                                                   return (castToPaned h)
                        ParaDir Vertical    -> do  v <- hPanedNew
                                                   return (castToPaned v)
                        _ -> error "Composite>>splitEditor"
                    panedPack1 paned fstFrame True True
                    panedPack2 paned sndFrame True True
                    containerAdd widget paned
                    reflectState (do
                        inj1 v1
                        inj2 v2) stateR
                    writeIORef coreRef (Just (fst,snd))
                Just ((_,inj1,_),(_,inj2,_)) -> do
                    reflectState (do
                        inj1 v1
                        inj2 v2) stateR)
        (do
            core <- liftIO $ readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    if isJust r1 && isJust r2
                        then return (Just (fromJust r1,fromJust r2))
                        else return Nothing)
        parameters
        notifier

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- or deselected (if the positive Argument is False)
--
maybeEditor :: Default beta => (Editor beta, Parameters) -> Bool -> String -> Editor (Maybe beta)
maybeEditor (childEdit, childParams) positive boolLabel parameters notifier = do
    coreRef      <- liftIO $ newIORef Nothing
    childRef     <- liftIO $ newIORef Nothing
    notifierBool <- makeGUIEvent
    cNoti        <- makeGUIEvent
    mkEditor
        (\widget mbVal -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- case getPara "Direction" parameters of
                        ParaDir Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        ParaDir Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                        _ -> error "Composite>>maybeEditor"
                    be@(boolFrame,inj1,_ext1)  <- reflectState (boolEditor
                            (("Name",ParaString boolLabel) <<< defaultParams)
                        notifierBool) stateR
                    boxPackStart box boolFrame PackNatural 0
                    containerAdd widget box
                    reflectState (do
                        registerGUIEvent notifierBool [Clicked]
                            (onClickedHandler widget coreRef childRef cNoti)
                        propagateGUIEvent notifier [notifierBool] [MayHaveChanged]
                        case mbVal of
                            Nothing  -> inj1 (not positive)
                            Just val -> reifyState $ \ stateR -> do
                                (childWidget,inj2,_ext2) <- reflectState(
                                    getChildEditor childRef childEdit childParams cNoti) stateR
                                boxPackEnd box childWidget PackGrow 0
                                widgetShowAll childWidget
                                reflectState (do
                                    inj1 positive
                                    inj2 val) stateR) stateR
                    writeIORef coreRef (Just (be,box))
                Just ((_boolFrame,inj1,_extt),box) -> do
                    hasChild <- hasChildEditor childRef
                    case mbVal of
                        Nothing ->
                            if hasChild
                                then do
                                    (childWidget,_,_) <- reflectState(
                                        getChildEditor childRef childEdit childParams cNoti)
                                            stateR
                                    reflectState (inj1 (not positive)) stateR
                                    widgetHideAll childWidget
                                else reflectState (inj1 (not positive)) stateR
                        Just val ->
                            if hasChild
                                then do
                                    reflectState (inj1 positive) stateR
                                    (childWidget,inj2,_) <- reflectState (
                                        getChildEditor childRef childEdit childParams cNoti)
                                            stateR
                                    widgetShowAll childWidget
                                    reflectState (inj2 val) stateR
                                else do
                                    reflectState (inj1 positive) stateR
                                    (childWidget,inj2,_) <- reflectState (
                                        getChildEditor childRef childEdit childParams cNoti)
                                            stateR
                                    boxPackEnd box childWidget PackGrow 0
                                    widgetShowAll childWidget
                                    reflectState (inj2 val) stateR)
        (reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> return Nothing
                Just ((_boolFrame,_inj1,ext1),_) -> do
                    bool <- reflectState ext1 stateR
                    case bool of
                        Nothing -> return Nothing
                        Just bv | bv == positive -> do
                            (_,_,ext2) <- reflectState(
                                getChildEditor childRef childEdit childParams cNoti) stateR
                            value <- reflectState ext2 stateR
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (Just value))
                        _ -> return (Just Nothing))
        parameters
        notifier
    where
    onClickedHandler _widget coreRef childRef cNoti event = do
        core <- liftIO $ readIORef coreRef
        case core of
            Nothing  -> error "Impossible"
            Just ((_boolFrame,_inj1,ext1),vBox) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                        if bool /= positive
                            then do
                                hasChild <- hasChildEditor childRef
                                when hasChild $ do
                                    (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                    liftIO $ widgetHideAll childWidget
                            else do
                                _hasChild <- hasChildEditor childRef
                                (childWidget,inj2,_ext2) <- getChildEditor childRef childEdit childParams cNoti
                                children <- liftIO $ containerGetChildren vBox
                                unless (elem childWidget children) $
                                    liftIO $ boxPackEnd vBox childWidget PackNatural 0
                                inj2 getDefault
                                liftIO $ widgetShowAll childWidget
                    Nothing -> return ()
                return (event {geGtkReturn=True})
    getChildEditor childRef childEditor childParams cNoti =  reifyState $ \ stateR -> do
        mb <- readIORef childRef
        case mb of
            Just editor -> return editor
            Nothing -> do
                let _val = childEditor
                editor@(_,_,_) <- reflectState (childEditor childParams cNoti) stateR
                reflectState (propagateGUIEvent notifier [cNoti] allGUIEvents) stateR
                writeIORef childRef (Just editor)
                return editor
    hasChildEditor childRef =  liftIO $ do
        mb <- readIORef childRef
        return (isJust mb)


--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- or grayed out (if the positive Argument is False)
--
disableEditor :: Default beta => (Editor beta, Parameters) -> Bool -> String -> Editor (Bool,beta)
disableEditor (childEdit, childParams) positive boolLabel parameters notifier = do
    coreRef      <- liftIO $ newIORef Nothing
    childRef     <- liftIO $ newIORef Nothing
    notifierBool <- makeGUIEvent
    cNoti        <- makeGUIEvent
    mkEditor
        (\widget mbVal -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- case getPara "Direction" parameters of
                        ParaDir Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        ParaDir Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                        _ -> error "Composite>>disableEditor"
                    be@(boolFrame,inj1,_ext1) <- reflectState (boolEditor
                        (("Name",ParaString boolLabel) <<< defaultParams)
                        notifierBool) stateR
                    boxPackStart box boolFrame PackNatural 0
                    containerAdd widget box
                    reflectState (do
                        registerGUIEvent notifierBool [Clicked]
                            (onClickedHandler widget coreRef childRef cNoti)
                        propagateGUIEvent notifier [notifierBool] [MayHaveChanged]) stateR
                    case mbVal of
                        (False,val) -> do
                            (childWidget,inj2,_ext2) <- reflectState (
                                getChildEditor childRef childEdit childParams cNoti) stateR
                            boxPackEnd box childWidget PackGrow 0
                            widgetShowAll childWidget
                            reflectState(do
                                inj1 ( not positive)
                                inj2 val) stateR
                            widgetSetSensitive childWidget False
                        (True,val) -> do
                            (childWidget,inj2,_ext2) <- reflectState (
                                getChildEditor childRef childEdit childParams cNoti) stateR
                            boxPackEnd box childWidget PackGrow 0
                            widgetShowAll childWidget
                            reflectState(do
                                inj1 positive
                                inj2 val) stateR
                            widgetSetSensitive childWidget True
                    writeIORef coreRef (Just (be,box))
                Just ((_boolFrame,inj1,_extt),box) -> do
                    hasChild <- hasChildEditor childRef
                    case mbVal of
                        (False,_val) ->
                            if hasChild
                                then do
                                    (childWidget,_,_) <- reflectState (
                                        getChildEditor childRef childEdit childParams cNoti)
                                            stateR
                                    reflectState (inj1 (not positive)) stateR
                                    widgetSetSensitive childWidget False
                                else reflectState (inj1 (not positive)) stateR
                        (True,val) ->
                            if hasChild
                                then do
                                    reflectState (inj1 positive) stateR
                                    (childWidget,inj2,_) <- reflectState (
                                        getChildEditor childRef childEdit childParams cNoti)
                                            stateR
                                    reflectState (inj2 val) stateR
                                    widgetSetSensitive childWidget True
                                else do
                                    reflectState (inj1 positive) stateR
                                    (childWidget,inj2,_) <- reflectState (
                                        getChildEditor childRef childEdit childParams cNoti)
                                            stateR
                                    boxPackEnd box childWidget PackGrow 0
                                    widgetSetSensitive childWidget True
                                    reflectState (inj2 val) stateR)
        (do
            core <- liftIO $ readIORef coreRef
            case core of
                Nothing  -> return Nothing
                Just ((_boolFrame,_inj1,ext1),_) -> do
                    bool <- ext1
                    case bool of
                        Nothing -> return Nothing
                        Just bv | bv == positive -> do
                            (_,_,ext2) <- getChildEditor childRef childEdit childParams cNoti
                            value <- ext2
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (True, value))
                        _ -> do
                            (_,_,ext2) <- getChildEditor childRef childEdit childParams cNoti
                            value <- ext2
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (False, value)))
        parameters
        notifier
    where
    onClickedHandler _widget coreRef childRef cNoti event = do
        core <- liftIO $ readIORef coreRef
        case core of
            Nothing  -> error "Impossible"
            Just ((_boolFrame,_inj1,ext1),vBox) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                        if bool /= positive
                            then do

                                hasChild <- hasChildEditor childRef
                                when hasChild $ do
                                    (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                    liftIO $ widgetSetSensitive childWidget False
                            else do
                                hasChild <- hasChildEditor childRef
                                if hasChild
                                    then do
                                        (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                        liftIO $ widgetSetSensitive childWidget True
                                    else do
                                        (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                        liftIO $ boxPackEnd vBox childWidget PackNatural 0
                                        inj2 getDefault
                                        liftIO $ widgetSetSensitive childWidget True
                    Nothing -> return ()
                return (event {geGtkReturn=True})
    getChildEditor childRef childEditor childParams cNoti =  do
        mb <- liftIO $ readIORef childRef
        case mb of
            Just editor -> return editor
            Nothing -> do
                let _val = childEditor
                editor@(_,_,_) <- childEditor childParams cNoti
                propagateGUIEvent notifier [cNoti] allGUIEvents
                liftIO $ writeIORef childRef (Just editor)
                return editor
    hasChildEditor childRef =  liftIO $ do
        mb <- readIORef childRef
        return (isJust mb)

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- or deselected (if the positive Argument is False)
eitherOrEditor :: (Default alpha, Default beta) => (Editor alpha, Parameters) ->
                        (Editor beta, Parameters) -> String -> Editor (Either alpha beta)
eitherOrEditor (leftEditor,leftParams) (rightEditor,rightParams)
            _label2 parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    noti1 <- makeGUIEvent
    noti2 <- makeGUIEvent
    noti3 <- makeGUIEvent
    propagateGUIEvent notifier [noti1,noti2,noti3] allGUIEvents
    be@(boolFrame,inj1,_ext1) <- boolEditor2  (getParaS "Name" rightParams) leftParams noti1
    le@(leftFrame,inj2,_ext2) <- leftEditor (("Name", ParaString "") <<< leftParams) noti2
    re@(rightFrame,inj3,_ext3) <- rightEditor (("Name", ParaString "") <<<rightParams) noti3
    mkEditor
        (\widget v -> do
            core <- liftIO $ readIORef coreRef
            case core of
                Nothing  -> do
                    registerGUIEvent noti1 [Clicked] (onClickedHandler widget coreRef)
                    box <- case getPara "Direction" parameters of
                        ParaDir Horizontal -> do
                            b <- liftIO $ hBoxNew False 1
                            return (castToBox b)
                        ParaDir Vertical -> do
                            b <- liftIO $ vBoxNew False 1
                            return (castToBox b)
                        _ -> error "Composite>>eitherOrEditor"
                    liftIO $ boxPackStart box boolFrame PackNatural 0
                    liftIO $ containerAdd widget box
                    case v of
                        Left vl -> do
                          liftIO $ boxPackStart box leftFrame PackNatural 0
                          inj2 vl
                          inj3 getDefault
                          inj1 True
                        Right vr  -> do
                          liftIO $ boxPackStart box rightFrame PackNatural 0
                          inj3 vr
                          inj2 getDefault
                          inj1 False
                    liftIO $ writeIORef coreRef (Just (be,le,re,box))
                Just ((_,inj1,_),(leftFrame,inj2,_),(rightFrame,inj3,_),box) ->
                    case v of
                            Left vl -> do
                              liftIO $ containerRemove box rightFrame
                              liftIO $ boxPackStart box leftFrame PackNatural 0
                              inj2 vl
                              inj3 getDefault
                              inj1 True
                            Right vr  -> do
                              liftIO $ containerRemove box leftFrame
                              liftIO $ boxPackStart box rightFrame PackNatural 0
                              inj3 vr
                              inj2 getDefault
                              inj1 False)
        (do core <- liftIO $ readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2),(_,_,ext3),_) -> do
                    mbbool <- ext1
                    case mbbool of
                        Nothing -> return Nothing
                        Just True   ->  do
                            value <- ext2
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (Left value))
                        Just False -> do
                            value <- ext3
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (Right value)))
        (("Name",ParaString "") <<< parameters)
        notifier
    where
    onClickedHandler _widget coreRef event =  do
        core <- liftIO $ readIORef coreRef
        case core of
            Nothing  -> error "Impossible"
            Just ((_,_,ext1),(leftFrame,_,_),(rightFrame,_,_),box) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                            if bool then liftIO $ do
                              containerRemove box rightFrame
                              boxPackStart box leftFrame PackNatural 0
                              widgetShowAll box
                            else liftIO $ do
                              containerRemove box leftFrame
                              boxPackStart box rightFrame PackNatural 0
                              widgetShowAll box
                    Nothing -> return ()
                return event{geGtkReturn=True}

--
-- | An editor for a selection from some given elements
selectionEditor :: (Show alpha, Typeable alpha, Default alpha, Eq alpha) => ColumnDescr alpha
    -> Maybe (alpha -> alpha -> Ordering) -- ^ The 'mbSort' arg, a sort function if desired
    -> Maybe (alpha -> alpha -> Bool) -- ^ the test to ommit double insertions
    -> Maybe ([alpha] -> StateM())
    -> Editor ([alpha],[alpha])
selectionEditor (ColumnDescr showHeaders columnsDD) mbSort mbTest mbDeleteHandler parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget (selected,choices) -> do
            core <- liftIO $ readIORef coreRef
            case core of
                Nothing  -> do
                    (box,buttonBox) <- case getPara "Direction" parameters of
                        ParaDir Horizontal -> do
                            b  <- liftIO $ hBoxNew False 1
                            bb <- liftIO $ vButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                        ParaDir Vertical -> do
                            b  <- liftIO $ vBoxNew False 1
                            bb <- liftIO $ hButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                        _ -> error "Composite>>selectionEditor"
                    addButton   <- liftIO $ buttonNewFromStock "gtk-add"
                    removeButton <- liftIO $ buttonNewFromStock "gtk-remove"
                    deleteButton <- liftIO $ buttonNewFromStock "gtk-delete"

                    liftIO $ containerAdd buttonBox addButton
                    when (isJust mbDeleteHandler) $ liftIO $ containerAdd buttonBox deleteButton
                    liftIO $ containerAdd buttonBox removeButton

                    -- Two list stores
                    listStoreUnselected   <-  liftIO $ listStoreNew ([]:: [alpha])
                    listStoreSelected     <-  liftIO $ listStoreNew ([]:: [alpha])

                    activateGUIEvent' listStoreUnselected notifier
                        (\ w h -> do
                            res     <-  after (castToTreeModel w) rowInserted (\ _ _ ->
                                h (Gtk.Event True) >> return ())
                            return (castCID res)) MayHaveChanged
                    activateGUIEvent' listStoreUnselected notifier
                        (\ w h -> do
                            res     <-  after (castToTreeModel w) rowDeleted (\ _ ->
                                h (Gtk.Event True) >> return ())
                            return (castCID res)) MayHaveChanged
                    activateGUIEvent' listStoreSelected notifier
                        (\ w h -> do
                            res     <-  after (castToTreeModel w) rowInserted (\ _ _ ->
                                h (Gtk.Event True) >> return ())
                            return (castCID res)) MayHaveChanged
                    activateGUIEvent' listStoreSelected notifier
                        (\ w h -> do
                            res     <-  after (castToTreeModel w) rowDeleted (\ _ ->
                                h (Gtk.Event True) >> return ())
                            return (castCID res)) MayHaveChanged


                    reifyState $ \ stateR -> do
                    -- Two tree views
                        treeViewSelected  <-  treeViewNewWithModel listStoreSelected
                        let ParaSize minSize =  getPara "MinSize" parameters

                        uncurry (widgetSetSizeRequest treeViewSelected) minSize
                        sw1          <-  scrolledWindowNew Nothing Nothing
                        containerAdd sw1 treeViewSelected
                        scrolledWindowSetPolicy sw1 PolicyAutomatic PolicyAutomatic
                        sel1         <-  treeViewGetSelection treeViewSelected
                        treeSelectionSetMode sel1 SelectionMultiple
                        mapM_ (\(str,func,mbFunc2) -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col str
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn treeViewSelected col
                            renderer <- cellRendererTextNew
                            case mbFunc2 of
                                Nothing -> return ()
                                Just func -> do
                                    set renderer [cellTextEditable := True]
                                    on renderer edited
                                        (applyFunc listStoreSelected func stateR)
                                    return ()
                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStoreSelected func
                                ) columnsDD
                        treeViewSetHeadersVisible treeViewSelected showHeaders


                        treeViewUnselected  <-  liftIO $ treeViewNewWithModel listStoreUnselected
                        let ParaSize minSize =   getPara "MinSize" parameters
                        uncurry (widgetSetSizeRequest treeViewUnselected) minSize
                        sw2          <-  scrolledWindowNew Nothing Nothing
                        containerAdd sw2 treeViewUnselected
                        scrolledWindowSetPolicy sw2 PolicyAutomatic PolicyAutomatic
                        sel2         <-  treeViewGetSelection treeViewUnselected
                        treeSelectionSetMode sel2 SelectionMultiple
                        mapM_ (\(str,func, _mbFunc2) -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col str
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn treeViewUnselected col
                            renderer <- cellRendererTextNew
                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStoreUnselected func
                                ) columnsDD
                        treeViewSetHeadersVisible treeViewSelected showHeaders

                        boxPackStart box sw1 PackGrow 0
                        boxPackStart box buttonBox PackNatural 0
                        boxPackStart box sw2 PackNatural 0
                        containerAdd widget box

                        fill selected choices listStoreSelected listStoreUnselected
                        reflectState (do
                            activateGUIEvent treeViewSelected notifier ButtonPressed
                            activateGUIEvent treeViewUnselected notifier ButtonPressed) stateR

                        removeButton `onClicked` do
                            treePaths <- treeSelectionGetSelectedRows sel1
                            selected <- listStoreToList listStoreSelected
                            unselected <- listStoreToList listStoreUnselected
                            mapM_ (\(i:_) -> listStoreRemove listStoreSelected i) treePaths
                            newSelected <- listStoreToList listStoreSelected
                            fill newSelected (selected ++ unselected)
                                listStoreSelected listStoreUnselected
                        addButton `onClicked` do
                            treePaths <- treeSelectionGetSelectedRows sel2
                            selected <- listStoreToList listStoreSelected
                            unselected <- listStoreToList listStoreUnselected
                            addSelected <- mapM (\ (i:_) ->
                                listStoreGetValue listStoreUnselected i) treePaths
                            newSelected <- listStoreToList listStoreSelected
                            fill (newSelected ++ addSelected) (selected ++ unselected)
                                    listStoreSelected listStoreUnselected
                        case mbDeleteHandler of
                            Just handler -> do
                                deleteButton `onClicked` do
                                    treePaths   <- treeSelectionGetSelectedRows sel2
                                    _unselected  <- listStoreToList listStoreUnselected
                                    toDelete    <- mapM (\(i:_) -> listStoreGetValue
                                                                listStoreUnselected i)
                                                        treePaths
                                    reflectState (handler toDelete) stateR
                                return ()
                            Nothing -> return ()

                        doubleClickToSelected treeViewSelected sel1 listStoreSelected stateR
                        doubleClickToSelected treeViewUnselected sel2 listStoreUnselected stateR
                        writeIORef coreRef (Just (listStoreSelected,listStoreUnselected))
                Just (listStoreSelected,listStoreUnselected) -> liftIO $ do
                    fill selected choices listStoreSelected listStoreUnselected)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (listStoreSelected,listStoreUnselected) -> do
                    v1 <- listStoreToList listStoreSelected
                    v2 <- listStoreToList listStoreUnselected
                    return (Just (v1,v2)))
        (("MinSize",ParaSize (-1,-1)) <<< parameters)
        notifier
  where
    applyFunc listStoreSelected func stateR (p:_) string  =  do
        row <- listStoreGetValue listStoreSelected p
        listStoreSetValue listStoreSelected p (func row string)
        reflectState (triggerGUIEvent notifier
            dummyGUIEvent {geSelector = MayHaveChanged}) stateR
        return ()
    applyFunc  _ _ _ [] _ =  error "Composite>>selectionEditor"
    fill selected choices listStoreSelected listStoreUnselected =
        let selected' = case mbTest of
                            Nothing -> nub selected
                            Just f  -> nubBy f selected
            all       = selected ++ choices
            all'      = case mbTest of
                            Nothing -> nub all
                            Just f -> nubBy f all
            unselected' =  case mbTest of
                                Nothing -> [e | e <- all' , not (elem e selected')]
                                Just f -> [e | e <- all' ,
                                            null [ g | g <- selected', f g e]]
        in do
            listStoreClear listStoreSelected
            mapM_ (listStoreAppend listStoreSelected)
                    (case mbSort of
                        Nothing -> selected'
                        Just sortF -> sortBy sortF selected')
                --Fill box 2
            listStoreClear listStoreUnselected
            mapM_ (listStoreAppend listStoreUnselected)
                (case mbSort of
                    Nothing -> unselected'
                    Just sortF -> sortBy sortF unselected')
    doubleClickToSelected treeView treeViewSelection listStore stateR =
        liftIO $ treeView `onButtonPress`
            (\ gtkEvent ->
                case gtkEvent of
                    Gtk.Button{Gtk.eventClick = eventClick}
                        | eventClick == DoubleClick -> do
                            rows <- treeSelectionGetSelectedRows treeViewSelection
                            mbVal <- case rows of
                                        ([i]:_) -> liftM (Just . GenSelection)
                                            (listStoreGetValue listStore i)
                                        _ -> return Nothing

                            reflectState (triggerGUIEvent notifier (dummyGUIEvent {
                                geSelector = Selection,
                                geGtkEvent = gtkEvent,
                                geMbSelection = mbVal,
                                geGtkReturn = True})) stateR
                            return False
                    _ -> return False)



-- a trivial example: (ColumnDescr False [("",(\row -> [cellText := show row]))])
-- and a nontrivial:
--  [("Package",\(Dependency str _) -> [cellText := str])
--  ,("Version",\(Dependency _ vers) -> [cellText := showVersionRange vers])])
data ColumnDescr row = ColumnDescr Bool [(String,(row -> [AttrOp CellRendererText]),
                                                    Maybe (row -> String -> row))]

--
-- | An editor with a subeditor, of which a list of items can be selected
multisetEditor :: (Show alpha, Default alpha, Eq alpha) => ColumnDescr alpha
    -> (Editor alpha, Parameters)
    -> Maybe ([alpha] -> [alpha]) -- ^ The 'mbSort' arg, a sort function if desired
    -> Maybe (alpha -> alpha -> Bool) -- ^ The 'mbReplace' arg, a function which is a criteria for removing an
                              --   old entry when adding a new value
    -> Editor [alpha]
multisetEditor (ColumnDescr showHeaders columnsDD) (singleEditor, sParams) mbSort mbReplace
        parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    cnoti   <- makeGUIEvent
    mkEditor
        (\widget vs -> do
            core <- liftIO $ readIORef coreRef
            case core of
                Nothing  -> do
                    (box,buttonBox) <- case getPara "Direction" parameters of
                        ParaDir Horizontal -> do
                            b  <- liftIO $ hBoxNew False 1
                            bb <- liftIO $ vButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                        ParaDir Vertical -> do
                            b  <- liftIO $ vBoxNew False 1
                            bb <- liftIO $ hButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                    (frameS,injS,extS) <- singleEditor sParams cnoti
                    propagateGUIEvent notifier [cnoti] allGUIEvents
                    addButton   <- liftIO $ buttonNewWithLabel "Add"
                    removeButton <- liftIO $ buttonNewWithLabel "Remove"
                    liftIO $ containerAdd buttonBox addButton
                    liftIO $ containerAdd buttonBox removeButton
                    listStore   <-  liftIO $ listStoreNew ([]:: [alpha])
                    activateGUIEvent' listStore notifier
                        (\ w h -> do
                            res     <-  after (castToTreeModel w) rowInserted (\ _ _ ->
                                h (Gtk.Event True) >> return ())
                            return (castCID res)) MayHaveChanged
                    activateGUIEvent' listStore notifier
                        (\ w h -> do
                            res     <-  after (castToTreeModel w) rowDeleted (\ _ ->
                                h (Gtk.Event True) >> return ())
                            return (castCID res)) MayHaveChanged
                    treeView        <-  liftIO $ treeViewNewWithModel listStore
                    let ParaSize minSize =   getPara "MinSize" parameters
                    reifyState $ \ stateR -> do
                        uncurry (widgetSetSizeRequest treeView) minSize
                        sw          <-  scrolledWindowNew Nothing Nothing
                        containerAdd sw treeView
                        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
                        sel         <-  treeViewGetSelection treeView
                        treeSelectionSetMode sel SelectionSingle
                        mapM_ (\(str,func,_mbFunc2) -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col str
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn treeView col
                            renderer <- cellRendererTextNew

                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStore func
                            ) columnsDD
                        treeViewSetHeadersVisible treeView showHeaders
                        sel  `onSelectionChanged` selectionHandler sel listStore
                            (\ a -> reflectState (injS a) stateR)
                        boxPackStart box sw PackGrow 0
                        boxPackStart box buttonBox PackNatural 0
                        boxPackStart box frameS PackNatural 0
                        reflectState (activateGUIEvent (castToWidget treeView) notifier FocusOut)
                            stateR
                        containerAdd widget box
                        listStoreClear listStore
                        mapM_ (listStoreAppend listStore)
                            (case mbSort of
                                Nothing -> vs
                                Just sortF -> sortF vs)
                        addButton `onClicked` do
                            mbv <- reflectState extS stateR
                            case mbv of
                                Just v -> do
                                    case mbReplace of
                                        Nothing         -> return ()
                                        Just replaceF   -> do
                                             cont <- listStoreToList listStore
                                             mapM_ (listStoreRemove listStore)
                                                $ map fst
                                                    $ filter (\(_,e) -> replaceF v e)
                                                        $ zip [0..] cont
                                    case mbSort of
                                        Nothing    -> do
                                            listStoreAppend listStore v
                                            return ()
                                        Just sortF -> do
                                            cont <- listStoreToList listStore
                                            listStoreClear listStore
                                            mapM_ (listStoreAppend listStore) (sortF (v:cont))
                                    cont <- listStoreToList listStore
                                    case elemIndex v cont of
                                        Just idx -> do
                                            treeSelectionSelectPath sel [idx]
                                            mbCol <- treeViewGetColumn treeView 0
                                            case mbCol of
                                                Nothing  -> return ()
                                                Just col -> treeViewScrollToCell treeView [idx] col Nothing
                                        Nothing -> return ()
                                Nothing -> return ()
                        removeButton `onClicked` do
                            mbi <- treeSelectionGetSelected sel
                            case mbi of
                                Nothing -> return ()
                                Just iter -> do
                                    [i] <- treeModelGetPath listStore iter
                                    listStoreRemove listStore i
                        writeIORef coreRef (Just listStore)
                        reflectState (injS getDefault) stateR
                Just listStore -> liftIO $ do
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore)
                        (case mbSort of
                            Nothing -> vs
                            Just sortF -> sortF vs))
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listStore -> do
                    v <- listStoreToList listStore
                    return (Just v))
        (("MinSize",ParaSize (-1,-1)) <<< parameters)
        notifier
    where
--    selectionHandler :: TreeSelection -> ListStore a -> Injector a -> IO ()
    selectionHandler sel listStore inj = do
        ts <- treeSelectionGetSelected sel
        case ts of
            Nothing -> return ()
            Just iter -> do
                [i] <- treeModelGetPath listStore iter
                v <- listStoreGetValue listStore i
                inj v
                return ()


filesEditor :: Maybe FilePath -> FileChooserAction -> String -> Editor [FilePath]
filesEditor fp act label p =
    multisetEditor
        (ColumnDescr False [("",(\row -> [cellText := row]),Nothing)])
        (fileEditor fp act label, defaultParams)
        (Just sort)
        (Just (==))
        (("Shadow", ParaShadow ShadowIn) <<<
            (("Direction", ParaDir Vertical) <<< p))

stringsEditor :: (String -> Bool) -> Bool -> Editor [String]
stringsEditor validation trimBlanks p =
    multisetEditor
        (ColumnDescr False [("",(\row -> [cellText := row]),Nothing)])
        (stringEditor validation trimBlanks, defaultParams)
        (Just sort)
        (Just (==))
        (("Shadow", ParaShadow ShadowIn) <<< p)

--dependencyEditor :: [PackageIdentifier] -> Editor Dependency
--dependencyEditor packages para noti = do
--    (wid,inj,ext) <- pairEditor
--        ((eitherOrEditor (comboSelectionEditor ((sort . nub) (map (display . pkgName) packages)) id
--            , ("Name", ParaString "Select") <<< defaultParams)
--            (stringEditor (const True) True, ("Name", ParaString "Enter") <<< defaultParams)
--            "Select from list?"), ("Name", ParaString  "Name") <<< defaultParams)
--        (versionRangeEditor,("Name", ParaString "Version") <<< defaultParams)
--        (("Direction", ParaDir Vertical) <<< para)
--        noti
--    let pinj (Dependency pn@(PackageName s) v) = if elem s (map (display . pkgName) packages)
--                                                    then inj (Left s,v)
--                                                    else inj (Right s,v)
--    let pext = do
--        mbp <- ext
--        case mbp of
--            Nothing -> return Nothing
--            Just (Left "",v) -> return Nothing
--            Just (Left s,v) -> return (Just $ Dependency (PackageName s) v)
--            Just (Right "",v) -> return Nothing
--            Just (Right s,v) -> return (Just $ Dependency (PackageName s) v)
--    return (wid,pinj,pext)
--
--dependenciesEditor :: [PackageIdentifier] -> Editor [Dependency]
--dependenciesEditor packages p noti =
--    multisetEditor
--        (ColumnDescr True [("Package",\(Dependency (PackageName str) _) ->
--                                [cellText := str],Nothing)
--                           ,("Version",\(Dependency _ vers) ->
--                                    [cellText := display vers], Nothing)])
--        (dependencyEditor packages,
--            ("OuterAlignment",ParaAlign (0.0, 0.5, 1.0, 1.0)) <<<
--                (("InnerAlignment", ParaAlign (0.0, 0.5, 1.0, 1.0)) <<< defaultParams))
--        (Just (sortBy (\ (Dependency p1 _) (Dependency p2 _) -> compare p1 p2)))
--        (Just (\ (Dependency p1 _) (Dependency p2 _) -> p1 == p2))
--        (("Shadow", ParaShadow ShadowIn) <<<
--            ("OuterAlignment",ParaAlign (0.0, 0.5, 1.0, 1.0)) <<<
--                ("InnerAlignment", ParaAlign (0.0, 0.5, 1.0, 1.0)) <<<
--                    ("Direction",ParaDir Vertical) <<<
--                       ("VPack",ParaPack PackGrow) <<< p)
--        noti
--
--versionRangeEditor :: Editor VersionRange
--versionRangeEditor para noti = do
--    (wid,inj,ext) <-
--        maybeEditor
--            ((eitherOrEditor
--                (pairEditor
--                    (comboSelectionEditor v1 show, defaultParams)
--                    (versionEditor, ("Name",ParaString "Enter Version") <<< defaultParams),
--                        (("Direction",ParaDir Vertical) <<<
--                            ("Name",ParaString "Simple") <<<
--                            ("OuterAlignment",ParaAlign  (0.0, 0.0, 0.0, 0.0)) <<<
--                            ("OuterPadding", ParaPadding (0, 0, 0, 0)) <<<
--                            ("InnerAlignment", ParaAlign  (0.0, 0.0, 0.0, 0.0)) <<<
--                            ("InnerPadding", ParaPadding   (0, 0, 0, 0)) <<< defaultParams))
--                (tupel3Editor
--                    (comboSelectionEditor v2 show, defaultParams)
--                    (versionRangeEditor, ("Shadow", ParaShadow ShadowIn) <<< defaultParams)
--                    (versionRangeEditor, ("Shadow", ParaShadow ShadowIn) <<< defaultParams),
--                        ("Name", ParaString "Complex") <<<
--                        ("Direction", ParaDir Vertical) <<<
--                        ("OuterAlignment", ParaAlign (0.0, 0.0, 0.0, 0.0)) <<<
--                        ("OuterPadding", ParaPadding (0, 0, 0, 0)) <<<
--                        ("InnerAlignment", ParaAlign (0.0, 0.0, 0.0, 0.0)) <<<
--                        ("InnerPadding", ParaPadding (0, 0, 0, 0)) <<<defaultParams)
--                        "Select version range"), defaultParams)
--            False "Any Version"
--            (("Direction", ParaDir Vertical) <<< para)
--            noti
--    let vrinj AnyVersion                =   inj Nothing
--        vrinj (ThisVersion v)           =   inj (Just (Left (ThisVersionS,v)))
--        vrinj (LaterVersion v)          =   inj (Just (Left (LaterVersionS,v)))
--        vrinj (EarlierVersion v)        =   inj (Just (Left (EarlierVersionS,v)))
--        vrinj (UnionVersionRanges (ThisVersion v1) (LaterVersion v2)) | v1 == v2
--                                        =  inj (Just (Left (ThisOrLaterVersionS,v1)))
--        vrinj (UnionVersionRanges (LaterVersion v1) (ThisVersion v2)) | v1 == v2
--                                        =  inj (Just (Left (ThisOrLaterVersionS,v1)))
--        vrinj (UnionVersionRanges (ThisVersion v1) (EarlierVersion v2)) | v1 == v2
--                                        =  inj (Just (Left (ThisOrEarlierVersionS,v1)))
--        vrinj (UnionVersionRanges (EarlierVersion v1) (ThisVersion v2)) | v1 == v2
--                                        =  inj (Just (Left (ThisOrEarlierVersionS,v1)))
--        vrinj (UnionVersionRanges v1 v2)=  inj (Just (Right (UnionVersionRangesS,v1,v2)))
--        vrinj (IntersectVersionRanges v1 v2)
--                                        =    inj (Just (Right (IntersectVersionRangesS,v1,v2)))
--    let vrext = do  mvr <- ext
--                    case mvr of
--                        Nothing -> return (Just AnyVersion)
--                        Just Nothing -> return (Just AnyVersion)
--                        Just (Just (Left (ThisVersionS,v)))     -> return (Just (ThisVersion v))
--                        Just (Just (Left (LaterVersionS,v)))    -> return (Just (LaterVersion v))
--                        Just (Just (Left (EarlierVersionS,v)))   -> return (Just (EarlierVersion v))
--
--                        Just (Just (Left (ThisOrLaterVersionS,v)))   -> return (Just (orLaterVersion  v))
--                        Just (Just (Left (ThisOrEarlierVersionS,v)))   -> return (Just (orEarlierVersion  v))
--                        Just (Just (Right (UnionVersionRangesS,v1,v2)))
--                                                        -> return (Just (UnionVersionRanges v1 v2))
--                        Just (Just (Right (IntersectVersionRangesS,v1,v2)))
--                                                        -> return (Just (IntersectVersionRanges v1 v2))
--    return (wid,vrinj,vrext)
--        where
--            v1 = [ThisVersionS,LaterVersionS,ThisOrLaterVersionS,EarlierVersionS,ThisOrEarlierVersionS]
--            v2 = [UnionVersionRangesS,IntersectVersionRangesS]
--
--data Version1 = ThisVersionS | LaterVersionS | ThisOrLaterVersionS | EarlierVersionS | ThisOrEarlierVersionS
--    deriving (Eq)
--instance Show Version1 where
--    show ThisVersionS   =  "This Version"
--    show LaterVersionS  =  "Later Version"
--    show ThisOrLaterVersionS = "This or later Version"
--    show EarlierVersionS =  "Earlier Version"
--    show ThisOrEarlierVersionS = "This or earlier Version"
--
--data Version2 = UnionVersionRangesS | IntersectVersionRangesS
--    deriving (Eq)
--instance Show Version2 where
--    show UnionVersionRangesS =  "Union Version Ranges"
--    show IntersectVersionRangesS =  "Intersect Version Ranges"

versionEditor :: Editor Version
versionEditor para noti = do
    (wid,inj,ext) <- stringEditor (\s -> not (null s)) True para noti
    let pinj v = inj (display v)
    let pext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> return (simpleParse s)
    return (wid, pinj, pext)

--instance Default Version1
--    where getDefault = ThisVersionS
--
--instance Default Version2
--    where getDefault = UnionVersionRangesS

instance Default Version
    where getDefault = forceJust (simpleParse "0") "PackageEditor>>default version"

--instance Default VersionRange
--    where getDefault = AnyVersion

--instance Default Dependency
--    where getDefault = Dependency getDefault getDefault

instance Default PackageName
    where getDefault = PackageName getDefault






