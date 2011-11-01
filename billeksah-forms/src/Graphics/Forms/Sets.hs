{-# Language ExistentialQuantification #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Forms.Sets
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for editors of sets of objects
--
-----------------------------------------------------------------------------------

module Graphics.Forms.Sets (
    ColumnsDescr(..),
    ColumnDescr(..),
    tableEditor,
    multisetEditor,
    selectionEditor,
    filesEditor,
    stringsEditor
) where

import Base
import Graphics.Panes
import Graphics.Pane (castCID)
import Graphics.Forms.Default (Default(..), Default)
import Graphics.Forms.Basics
import Graphics.Forms.Build (mkEditor)
import Graphics.Forms.Parameters
import Graphics.Forms.GUIEvent
import Graphics.Forms.Simple (stringEditor, fileEditor)


import Graphics.UI.Gtk
import System.Glib.Attributes (AttrOp)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (writeIORef, readIORef, newIORef)
import Data.Typeable (Typeable)
import Control.Monad (liftM, when)
import Data.Maybe (isJust)
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk (Event(..))
import Data.List (sort, elemIndex, sortBy, nubBy, nub)
import qualified Graphics.UI.Gtk as Gtk (Button)

-- | A Column decsription describes the colums for a table editor.
-- This is an example:
--        (ColumnsDescr True [
--            ColumnDescr{
--                tcdLabel = "Lower",
--                tcdRenderer = cellRendererTextNew,
--                tcdRenderFunc = \ (_,(lower,_)) -> [cellText := showMbVersion lower],
-- The tricky part is the edit function:
--                tcdMbEditFunc = Just (\ renderer listStore -> do
--                                        on renderer edited (\ (p:_) str ->  do
--                                            row@(pn,(lower,upper)) <- listStoreGetValue listStore p
--                                            let newRow = case parse boundParser "" str of
--                                                            Left _  ->  row
--                                                            Right v ->  (pn,(v,upper))
--                                            listStoreSetValue listStore p newRow)
--                                        return ())}])

data ColumnsDescr alpha = ColumnsDescr {
    tcsdShowHeaders :: Bool,
    -- ^ describes, if headers should be shown
    tcsdColums :: [ColumnDescr alpha]}
    -- ^ describes the columns

data ColumnDescr alpha = forall beta . CellRendererClass beta => ColumnDescr {
    tcdLabel      :: String,
    -- ^ the column header
    tcdRenderer   :: IO beta,
    -- ^ construct the renderer used for this column
    tcdRenderFunc :: alpha -> [AttrOp beta],
    -- ^ the function to render the contest
    tcdMbEditFunc :: Maybe (beta -> ListStore alpha -> IO ())}
    -- ^ maybe a function, to edit the contest

--
-- | An editor for a list of given elements in the form of a table
--
tableEditor :: (Show alpha, Eq alpha) => ColumnsDescr alpha -> Editor [alpha]
tableEditor ColumnsDescr {tcsdShowHeaders = showHeaders, tcsdColums = columns} parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget table -> do
            core <- liftIO $ readIORef coreRef
            case core of
                Nothing  -> do
                    -- list store
                    listStore   <-  liftIO $ listStoreNew ([]:: [alpha])

                    reifyState $ \ stateR -> do
                    -- tree view
                        myTreeView  <-  treeViewNewWithModel listStore
                        let ParaSize minSize =  getPara "MinSize" parameters

                        uncurry (widgetSetSizeRequest myTreeView) minSize
                        sw1          <-  scrolledWindowNew Nothing Nothing
                        containerAdd sw1 myTreeView
                        scrolledWindowSetPolicy sw1 PolicyAutomatic PolicyAutomatic
                        mapM_ (\ tcd@ColumnDescr{tcdRenderer = rend, tcdMbEditFunc = mbEdit,
                                tcdRenderFunc = renderFunc} -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col (tcdLabel tcd)
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn myTreeView col
                            renderer <- rend
                            case mbEdit of
                                Nothing -> return ()
                                Just func -> func renderer listStore
                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStore renderFunc
                                ) columns
                        treeViewSetHeadersVisible myTreeView showHeaders

                        containerAdd widget sw1

                        fill table listStore
                        writeIORef coreRef (Just listStore)
                Just listStore -> liftIO $ do
                    fill table listStore)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listStore -> do
                    v1 <- listStoreToList listStore
                    return (Just v1))
        (("MinSize",ParaSize (-1,-1)) <<< parameters)
        notifier
  where
    fill table listStore = do
        listStoreClear listStore
        mapM_ (listStoreAppend listStore) table


--
-- | An editor for a selection from some given elements
selectionEditor :: (Show alpha, Typeable alpha, Default alpha, Eq alpha) => ColumnsDescr alpha
    -> Maybe (alpha -> alpha -> Ordering) -- ^ The 'mbSort' arg, a sort function if desired
    -> Maybe (alpha -> alpha -> Bool) -- ^ the test to ommit double insertions
    -> Maybe ([alpha] -> StateM()) -- ^ a handler for elements, which gets deleted
    -> Editor ([alpha],[alpha])
selectionEditor ColumnsDescr{tcsdShowHeaders = showHeaders, tcsdColums = columns} mbSort mbTest
                    mbDeleteHandler parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\ widget (selected,choices) -> do
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
                        mapM_ (\ ColumnDescr{tcdLabel = label, tcdRenderer = rend,
                                                tcdRenderFunc = func, tcdMbEditFunc = mbFunc} -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col label
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn treeViewSelected col
                            renderer <- rend
                            case mbFunc of
                                Nothing -> return ()
                                Just efunc -> do
                                    efunc renderer listStoreSelected
                                    return ()
                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStoreSelected func
                                ) columns
                        treeViewSetHeadersVisible treeViewSelected showHeaders


                        treeViewUnselected  <-  liftIO $ treeViewNewWithModel listStoreUnselected
                        let ParaSize minSize =   getPara "MinSize" parameters
                        uncurry (widgetSetSizeRequest treeViewUnselected) minSize
                        sw2          <-  scrolledWindowNew Nothing Nothing
                        containerAdd sw2 treeViewUnselected
                        scrolledWindowSetPolicy sw2 PolicyAutomatic PolicyAutomatic
                        sel2         <-  treeViewGetSelection treeViewUnselected
                        treeSelectionSetMode sel2 SelectionMultiple
                        mapM_ (\ColumnDescr{tcdLabel = label, tcdRenderer = rend,
                                                tcdRenderFunc = func} -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col label
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn treeViewUnselected col
                            renderer <- rend
                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStoreUnselected func
                                ) columns
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
                                        otherwise -> return Nothing

                            reflectState (triggerGUIEvent notifier (dummyGUIEvent {
                                geSelector = Selection,
                                geGtkEvent = gtkEvent,
                                geMbSelection = mbVal,
                                geGtkReturn = True})) stateR
                            return False
                    otherwise -> return False)

--
-- | An editor with a subeditor, of which a list of items can be selected
multisetEditor :: (Show alpha, Default alpha, Eq alpha) => ColumnsDescr alpha
    -> (Editor alpha, Parameters)
    -> Maybe ([alpha] -> [alpha]) -- ^ The 'mbSort' arg, a sort function if desired
    -> Maybe (alpha -> alpha -> Bool) -- ^ The 'mbReplace' arg, a function which is a criteria for removing an
                              --   old entry when adding a new value
    -> Editor [alpha]
multisetEditor ColumnsDescr{tcsdShowHeaders = showHeaders, tcsdColums = columns}
                    (singleEditor, sParams) mbSort mbReplace parameters notifier = do
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
                        mapM_ (\ColumnDescr{tcdLabel = label, tcdRenderer = rend,
                                                tcdRenderFunc = func} -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col label
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn treeView col
                            renderer <- rend

                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStore func
                            ) columns
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
        (ColumnsDescr False [
            ColumnDescr{
                tcdLabel = "",
                tcdRenderer = cellRendererTextNew,
                tcdRenderFunc = \row -> [cellText := row],
                tcdMbEditFunc = Nothing}])
        (fileEditor fp act label, defaultParams)
        (Just sort)
        (Just (==))
        (("Shadow", ParaShadow ShadowIn) <<<
            (("Direction", ParaDir Vertical) <<< p))

stringsEditor :: (String -> Bool) -> Bool -> Editor [String]
stringsEditor validation trimBlanks p =
    multisetEditor
        (ColumnsDescr False [
            ColumnDescr{
                tcdLabel = "",
                tcdRenderer = cellRendererTextNew,
                tcdRenderFunc = \row -> [cellText := row],
                tcdMbEditFunc = Nothing}])
        (stringEditor validation trimBlanks, defaultParams)
        (Just sort)
        (Just (==))
        (("Shadow", ParaShadow ShadowIn) <<< p)

