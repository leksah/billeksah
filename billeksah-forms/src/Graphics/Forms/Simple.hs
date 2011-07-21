{-# Language ScopedTypeVariables, CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Forms.Simple
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for making simple editors
--
-----------------------------------------------------------------------------------

module Graphics.Forms.Simple (
    noEditor
,   boolEditor
,   boolEditor2
,   enumEditor
,   clickEditor
,   stringEditor
,   multilineStringEditor
,   intEditor
,   genericEditor
,   fontEditor
,   colorEditor
,   comboSelectionEditor
,   staticListEditor
,   staticListMultiEditor
,   multiselectionEditor
,   fileEditor
,   otherEditor
,   imageEditor

) where


import Base
import Graphics.Forms.Basics
import Graphics.Forms.Parameters
import Graphics.Forms.Build
import Graphics.Forms.GUIEvent
import Graphics.Panes (Direction(..))

import Graphics.UI.Gtk hiding (eventKeyName, eventModifier)
import qualified Graphics.UI.Gtk as Gtk
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.List
import Data.Maybe
import System.FilePath.Posix
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk (Event(..))
import Unsafe.Coerce (unsafeCoerce)
import Control.Exception (SomeException)


-- ------------------------------------------------------------
-- * Simple Editors
-- ------------------------------------------------------------

instance ContainerClass Widget
instance BinClass Widget
instance ButtonClass Widget

--
-- | An invisible editor without any effect
--
noEditor :: alpha -> Editor alpha
noEditor proto parameters notifier =
    mkEditor
        (\ widget _ -> return ())
        (return (Just proto))
        parameters
        notifier

--
-- | Editor for a boolean value in the form of a check button
--
boolEditor :: Editor Bool
boolEditor parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget bool -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- checkButtonNewWithLabel (getParaS "Name" parameters)
                    widgetSetName button (getParaS "Name" parameters)
                    containerAdd widget button
                    toggleButtonSetActive button bool
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget button) notifier)
                            (Clicked: genericGUIEvents)
                        retriggerAsChanged notifier [Clicked]) stateR
                    writeIORef coreRef (Just button)
                Just button -> toggleButtonSetActive button bool)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just button -> do
                    r <- toggleButtonGetActive button
                    return (Just r))
        (("Name",ParaString "") <<< parameters)
        notifier

--
-- | Editor for a boolean value in the form of two radio buttons
----
boolEditor2 :: String -> Editor Bool
boolEditor2 label2 parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget bool -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- vBoxNew True 2
                    radio1 <- radioButtonNewWithLabel (getParaS "Name" parameters)
                    radio2 <- radioButtonNewWithLabelFromWidget radio1 label2
                    boxPackStart box radio1 PackGrow 2
                    boxPackStart box radio2 PackGrow 2
                    widgetSetName radio1 $ getParaS "Name" parameters ++ ".1"
                    widgetSetName radio2 $ getParaS "Name" parameters ++ ".2"
                    containerAdd widget box
                    if bool
                        then toggleButtonSetActive radio1 True
                        else toggleButtonSetActive radio2 True
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget radio1) notifier)
                                        (Clicked:genericGUIEvents)
                        mapM_ (activateGUIEvent (castToWidget radio2) notifier)
                                        (Clicked:genericGUIEvents)
                        retriggerAsChanged notifier [Clicked]) stateR
                    writeIORef coreRef (Just (radio1,radio2))
                Just (radio1,radio2) ->
                    if bool
                        then toggleButtonSetActive radio1 True
                        else toggleButtonSetActive radio2 True)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (radio1,radio2) -> do
                    r <- toggleButtonGetActive radio1
                    return (Just r))
        (("Name", ParaString "") <<< parameters)
        notifier

--
-- | Editor for an enum value in the form of n radio buttons
----
enumEditor :: forall alpha . (Show alpha, Enum alpha, Bounded alpha)  => [String] -> Editor alpha
enumEditor labels parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    let vals :: [alpha] =  allOf
    mkEditor
        (\widget enumValue -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- vBoxNew True 2
                    let label0 = if length labels > 0 then labels !! 0 else show (vals !! 0)
                    button0 <- radioButtonNewWithLabel label0
                    buttons <- mapM (\ v -> do
                        let n = fromEnum v
                        let label = if length labels > n then labels !! n else show v
                        radio <- if n == 0
                                    then return button0
                                    else radioButtonNewWithLabelFromWidget button0 label
                        boxPackStart box radio PackGrow 2
                        widgetSetName radio (label ++ show n)
                        return radio) vals
                    containerAdd widget box
                    reflectState (do
                        mapM_
                            (\e ->
                                (mapM_
                                    (\b -> activateGUIEvent (castToWidget b) notifier e)
                             buttons)) (Clicked:genericGUIEvents)
                        retriggerAsChanged notifier [Clicked]) stateR
                    mapM_ (\(b,n) -> toggleButtonSetActive b (n == fromEnum enumValue))
                                (zip buttons [0..length buttons - 1])
                    writeIORef coreRef (Just buttons)
                Just buttons -> do
                    mapM_ (\(b,n) -> toggleButtonSetActive b (n == fromEnum enumValue))
                                (zip buttons [0..length buttons - 1]))
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just buttons -> do
                    boolArray <- mapM toggleButtonGetActive buttons
                    let mbInd =  findIndex (== True) boolArray
                    let res = case mbInd of
                                Nothing -> Nothing
                                Just i -> Just (vals !! i)
                    return res)
        (("Name", ParaString "") <<< parameters)
        notifier

-- | An Editor for nothing (which may report a click) in the form of a button
--
clickEditor :: Bool -> Editor ()
clickEditor canDefault parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget bool -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- case getPara "StockId" parameters of
                        ParaString "" ->   buttonNewWithLabel (getParaS "Name" parameters)
                        ParaString st ->   buttonNewFromStock st
                    widgetSetName button (getParaS "Name" parameters)
                    containerAdd widget button
                    reflectState(
                        activateGUIEvent (castToWidget button) notifier Clicked) stateR
                    writeIORef coreRef (Just button)
                    when canDefault $ do
                        set button [widgetCanDefault := True]
                        widgetGrabDefault button
                Just button -> return ())
        (return (Just ()))
        (("Name",ParaString "") <<< parameters)
        notifier

-- | An Editor to display an image
--
imageEditor :: Editor StockId
imageEditor parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget stockId -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    image <- imageNewFromStock stockId IconSizeLargeToolbar
                    widgetSetName image (getParaS "Name" parameters)
                    containerAdd widget image
                    writeIORef coreRef (Just (image,stockId))
                Just (image,stockId2) -> imageSetFromStock image stockId IconSizeLargeToolbar)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,stockId3) -> return (Just stockId3))
        parameters
        notifier

--
-- | Editor for a string in the form of a text entry
--
stringEditor :: (String -> Bool) -> Bool -> Editor String
stringEditor validation trimBlanks parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget string -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    entry   <-  entryNew
                    widgetSetName entry (getParaS "Name" parameters)
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget entry) notifier)
                            genericGUIEvents
                        retriggerAsChanged notifier [KeyPressed]) stateR
                    containerAdd widget entry
                    entrySetText entry (if trimBlanks then trim string else string)
                    writeIORef coreRef (Just entry)
                Just entry -> entrySetText entry (if trimBlanks then trim string else string))
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just entry -> do
                    r <- entryGetText entry
                    if validation r
                        then return (Just (if trimBlanks then trim r else r))
                        else return Nothing)
        parameters
        notifier

--
-- | Editor for a multiline string in the form of a multiline text entry
--
multilineStringEditor :: Editor String
multilineStringEditor parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget string -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    aTextView       <-  textViewNew
                    widgetSetName aTextView (getParaS "Name" parameters)
                    aScrolledWindow <-  scrolledWindowNew Nothing Nothing
                    scrolledWindowSetPolicy aScrolledWindow PolicyAutomatic PolicyAutomatic
                    containerAdd aScrolledWindow aTextView
                    containerAdd widget aScrolledWindow
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget aTextView) notifier)
                                    genericGUIEvents
                        retriggerAsChanged notifier [KeyPressed]) stateR
                    buffer          <-  textViewGetBuffer aTextView
                    textBufferSetText buffer string
                    writeIORef coreRef (Just (aScrolledWindow,aTextView))
                Just (aScrolledWindow,aTextView) -> do
                    buffer          <-  textViewGetBuffer aTextView
                    textBufferSetText buffer string)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (aScrolledWindow, aTextView) -> do
                    buffer          <-  textViewGetBuffer aTextView
                    start           <-  textBufferGetStartIter buffer
                    end             <-  textBufferGetEndIter buffer
                    r               <-  textBufferGetText buffer start end False
                    return (Just r))
        (("HPack",ParaPack PackGrow) <<<("VPack",ParaPack PackGrow) <<< parameters)
        notifier

--
-- | Editor for an integer in the form of a spin entry
--
intEditor :: (Double,Double,Double) -> Editor Int
intEditor (min, max, step) parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget v -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    spin <- spinButtonNewWithRange min max step
                    widgetSetName spin  (getParaS "Name" parameters)
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget spin) notifier)
                            (genericGUIEvents)
                        activateGUIEvent' (castToWidget spin) notifier
                            (\ w h -> do
                                res     <-  afterValueSpinned (castToSpinButton w) (do
                                    h (Gtk.Event True)
                                    return ())
                                return (unsafeCoerce res))
                                MayHaveChanged) stateR
                    containerAdd widget spin
                    spinButtonSetValue spin (fromIntegral v)
                    writeIORef coreRef (Just spin)
                Just spin -> spinButtonSetValue spin (fromIntegral v))
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just spin -> do
                    newNum <- spinButtonGetValue spin
                    return (Just (truncate newNum)))
        parameters
        notifier

--
-- | Editor for for any value which is an instance of Read and Show in the form of a
-- | text entry
genericEditor :: (Show beta, Read beta) => Editor beta
genericEditor parameters notifier = do
    (wid,inj,ext) <- stringEditor (const True) True parameters notifier
    let ginj = inj . show
    let gext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> catchState (liftM Just (liftIO $ readIO s))
                            (\ (e :: SomeException) -> do
                                message Error ("Generic editor no parse for " ++ s ++ " " ++ show e)
                                return Nothing)
    return (wid,ginj,gext)

--
-- | Editor for no value, it only emtis a clicked event and has the form of a check button
--
buttonEditor :: Editor ()
buttonEditor parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget _ -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel  (getParaS "Name" parameters)
                    widgetSetName button  (getParaS "Name" parameters)
                    containerAdd widget button
                    reflectState (
                        mapM_ (activateGUIEvent (castToWidget button) notifier )
                            (Clicked:genericGUIEvents)) stateR
                    writeIORef coreRef (Just button)
                Just button -> return ())
        (return (Just ()))
        parameters
        notifier

--
-- | Editor for the selection of some element from a static list of elements in the
-- | form of a combo box

comboSelectionEditor :: Eq beta => [beta] -> (beta -> String) -> Editor beta
comboSelectionEditor list showF parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget obj -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    combo <- comboBoxNewText
                    mapM_ (\o -> comboBoxAppendText combo (showF o)) list
                    widgetSetName combo (getParaS "Name" parameters)
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget combo) notifier )
                            genericGUIEvents
                        activateGUIEvent' (castToWidget combo) notifier
                            (\ w h -> do
                                res     <-  on (castToComboBox w) changed (do
                                    h (Gtk.Event True)
                                    return ())
                                return (unsafeCoerce res)) MayHaveChanged) stateR
                    comboBoxSetActive combo 1
                    containerAdd widget combo
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> comboBoxSetActive combo i
                        Nothing -> return ()
                    writeIORef coreRef (Just combo)
                Just combo -> do
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> comboBoxSetActive combo i
                        Nothing -> return ())
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just combo -> do
                    ind <- comboBoxGetActive combo
                    case ind of
                        (-1)   -> return Nothing
                        otherwise  -> return (Just (list !! ind)))
        parameters
        notifier

--
-- | Editor for the selection of some elements from a list of elements in the
-- | form of a list box
multiselectionEditor :: (Show beta, Eq beta) => Editor [beta]
multiselectionEditor parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget objs -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    listStore   <- listStoreNew ([]:: [alpha])
                    listView    <- treeViewNewWithModel listStore
                    widgetSetName listView (getParaS "Name" parameters)
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget listView) notifier)
                            genericGUIEvents
                        retriggerAsChanged notifier [KeyPressed,ButtonPressed]) stateR
                    sel         <- treeViewGetSelection listView
                    treeSelectionSetMode sel SelectionMultiple
                    renderer    <- cellRendererTextNew
                    col         <- treeViewColumnNew
                    treeViewAppendColumn listView col
                    cellLayoutPackStart col renderer True
                    cellLayoutSetAttributes col renderer listStore
                        $ \row -> [ cellText := show row ]
                    treeViewSetHeadersVisible listView False
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) objs
                    containerAdd widget listView
                    treeSelectionUnselectAll sel
                    --let inds = catMaybes $map (\obj -> elemIndex obj list) objs
                    --mapM_ (\i -> treeSelectionSelectPath sel [i]) inds
                    writeIORef coreRef (Just (listView,listStore))
                Just (listView,listStore) -> do
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) objs)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (listView,listStore) -> do
                    sel         <- treeViewGetSelection listView
                    treePath    <- treeSelectionGetSelectedRows sel
                    values      <- mapM (\[i] -> listStoreGetValue listStore i) treePath
                    return (Just values))
        parameters
        notifier

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box with toggle elements
staticListMultiEditor :: (Eq beta) => [beta] -> (beta -> String) -> Editor [beta]
staticListMultiEditor list showF parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget objs -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    listStore <- listStoreNew ([]:: [(Bool,beta)])
                    listView <- treeViewNewWithModel listStore
                    widgetSetName listView (getParaS "Name" parameters)
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget listView) notifier)
                             genericGUIEvents
                        retriggerAsChanged notifier [KeyPressed,ButtonPressed]) stateR
                    sel <- treeViewGetSelection listView
                    treeSelectionSetMode sel SelectionSingle
                    rendererToggle <- cellRendererToggleNew
                    set rendererToggle [cellToggleActivatable := True]
                    rendererText <- cellRendererTextNew
                    col1 <- treeViewColumnNew
                    treeViewAppendColumn listView col1
                    cellLayoutPackStart col1 rendererToggle True
                    cellLayoutSetAttributes col1 rendererToggle listStore
                        $ \row -> [ cellToggleActive := fst row]
                    col2 <- treeViewColumnNew
                    treeViewAppendColumn listView col2
                    cellLayoutPackStart col2 rendererText True
                    cellLayoutSetAttributes col2 rendererText listStore
                        $ \row -> [ cellText := showF (snd row)]
                    treeViewSetHeadersVisible listView False
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) $ map (\e -> (elem e objs,e)) list
                    let ParaSize minSize =   getPara "MinSize" parameters
                    uncurry (widgetSetSizeRequest listView) minSize
                    sw          <-  scrolledWindowNew Nothing Nothing
                    containerAdd sw listView
                    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
                    containerAdd widget sw
                      -- update the model when the toggle buttons are activated
                    on rendererToggle cellToggled $ \pathStr -> do
                        let (i:_) = stringToTreePath pathStr
                        val <- listStoreGetValue listStore i
                        listStoreSetValue listStore i (not (fst val),snd val)
                    listView `onKeyPress` (\event -> do
                        let Key { eventKeyName = name, eventModifier = modifier, eventKeyChar = char } = event
                        case (name, modifier, char) of
                            ("Return", _, _) -> do
                                sel <- treeViewGetSelection listView
                                rows <- treeSelectionGetSelectedRows sel
                                mapM_ (\ (i:_) -> do
                                    val <- listStoreGetValue listStore i
                                    listStoreSetValue listStore i (not (fst val),snd val)) rows
                                return True
                            _ -> return False)
                    writeIORef coreRef (Just (listView,listStore))
                Just (listView,listStore) -> do
                    let model = map (\e -> (elem e objs,e)) list
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) $ map (\e -> (elem e objs,e)) list)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (listView,listStore) -> do
                    model <- listStoreToList listStore
                    return (Just (map snd $ filter (\e -> fst e) model)))
        parameters
        notifier

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box

staticListEditor :: (Eq beta) => [beta] -> (beta -> String) -> Editor beta
staticListEditor list showF parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget obj -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    listStore <- listStoreNew ([]:: [alpha])
                    listView <- treeViewNewWithModel listStore
                    widgetSetName listView (getParaS "Name" parameters)
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget listView) notifier)
                                genericGUIEvents
                        retriggerAsChanged notifier [KeyPressed,ButtonPressed]) stateR
                    sel <- treeViewGetSelection listView
                    treeSelectionSetMode sel
                        (case getPara "MultiSel" parameters of
                            ParaBool True  -> SelectionMultiple
                            ParaBool False -> SelectionSingle)
                    renderer <- cellRendererTextNew
                    col <- treeViewColumnNew
                    treeViewAppendColumn listView col
                    cellLayoutPackStart col renderer True
                    cellLayoutSetAttributes col renderer listStore
                        $ \row -> [ cellText := showF row ]
                    treeViewSetHeadersVisible listView False
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) list
                    let ParaSize minSize =   getPara "MinSize" parameters
                    uncurry (widgetSetSizeRequest listView) minSize
                    sw          <-  scrolledWindowNew Nothing Nothing
                    containerAdd sw listView
                    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
                    containerAdd widget sw
                    treeSelectionUnselectAll sel
                    let mbInd = elemIndex obj list
                    case mbInd of
                        Nothing -> return ()
                        Just ind -> treeSelectionSelectPath sel [ind]
                    writeIORef coreRef (Just listView)
                Just listView -> do
                    sel <- treeViewGetSelection listView
                    treeSelectionUnselectAll sel
                    let mbInd = elemIndex obj list
                    case mbInd of
                        Nothing -> return ()
                        Just ind -> treeSelectionSelectPath sel [ind])
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listView -> do
                    sel <- treeViewGetSelection listView
                    treePaths <- treeSelectionGetSelectedRows sel
                    case treePaths of
                        [[i]] -> return (Just (list !! i))
                        _ -> return Nothing)
        parameters
        notifier


--
-- | Editor for the selection of a file path in the form of a text entry and a button,
-- | which opens a gtk file chooser
fileEditor :: Maybe FilePath -> FileChooserAction -> String -> Editor FilePath
fileEditor mbFilePath action buttonName parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget filePath -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel buttonName
                    widgetSetName button $ getParaS "Name" parameters ++ "-button"
                    entry   <-  entryNew
                    widgetSetName entry $ getParaS "Name" parameters ++ "-entry"
                    -- set entry [ entryEditable := False ]
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget button) notifier)
                            (Clicked:genericGUIEvents)
                        mapM_ (activateGUIEvent (castToWidget entry) notifier)
                            genericGUIEvents
                        registerGUIEvent notifier [Clicked] (buttonHandler entry)
                        retriggerAsChanged notifier [KeyPressed,ButtonPressed]) stateR

                    box <- case getPara "Direction" parameters of
                                ParaDir Horizontal  -> do
                                    r <- hBoxNew False 1
                                    return (castToBox r)
                                ParaDir Vertical    -> do
                                    r <- vBoxNew False 1
                                    return (castToBox r)
                    boxPackStart box entry PackGrow 0
                    boxPackEnd box button PackNatural 0
                    containerAdd widget box
                    entrySetText entry filePath
                    writeIORef coreRef (Just entry)
                Just entry -> entrySetText entry filePath)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just entry -> do
                    str <- entryGetText entry
                    return (Just str))
        parameters
        notifier
    where
    buttonHandler entry e =  reifyState $ \ stateR -> do
        mbFileName <- do
            dialog <- fileChooserDialogNew
                            (Just "Select File")
                            Nothing
                        action
                        [("gtk-cancel"
                        ,ResponseCancel)
                        ,("gtk-open"
                        ,ResponseAccept)]
            widgetShow dialog
            response <- dialogRun dialog
            case response of
                ResponseAccept -> do
                    f <- fileChooserGetFilename dialog
                    widgetDestroy dialog
                    return f
                ResponseCancel -> do
                    widgetDestroy dialog
                    return Nothing
                ResponseDeleteEvent-> do
                    widgetDestroy dialog
                    return Nothing
                _   -> return Nothing
        case mbFileName of
            Nothing -> return (e{geGtkReturn=True})
            Just fn -> do
--                let relative = case mbFilePath of
--                                Nothing -> fn
--                                Just rel -> makeRelative rel fn
                entrySetText entry fn
                reflectState (triggerGUIEvent notifier (GUIEvent {
                    geSelector    = MayHaveChanged,
                    geGtkEvent    = Gtk.Event True,
                    geText   = "",
                    geMbSelection = Nothing,
                    geGtkReturn   = True})) stateR
                return (e{geGtkReturn=True})

--
-- | Editor for a font selection
--
fontEditor :: Editor (Maybe String)
fontEditor parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget mbValue -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    fs <- fontButtonNew
                    widgetSetName fs $ getParaS "Name" parameters
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget fs) notifier)
                                    (Clicked: genericGUIEvents)
                        activateGUIEvent' (castToWidget fs) notifier
                            (\ w h -> do
                                res     <-  onFontSet (castToFontButton w)  (do
                                    h (Gtk.Event True)
                                    return ())
                                return (unsafeCoerce res)) MayHaveChanged) stateR
                    containerAdd widget fs
                    case mbValue of
                        Nothing -> return True
                        Just s  -> fontButtonSetFontName fs s
                    writeIORef coreRef (Just fs)
                Just fs ->   case mbValue of
                                Nothing -> return ()
                                Just s  -> do
                                    fontButtonSetFontName fs s
                                    return ())
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just fs -> do
                    f <- fontButtonGetFontName fs
                    return (Just (Just f)))
        parameters
        notifier

--
-- | Editor for color selection
--
colorEditor :: Editor Color
colorEditor parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget c -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    cs <- colorButtonNew
                    widgetSetName cs $ getParaS "Name" parameters
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget cs) notifier )
                            (Clicked: genericGUIEvents)
                        activateGUIEvent' (castToWidget cs) notifier
                            (\ w h -> do
                                res     <-  onColorSet (castToColorButton w)  (do
                                    h (Gtk.Event True)
                                    return ())
                                return (unsafeCoerce res)) MayHaveChanged) stateR
                    containerAdd widget cs
                    colorButtonSetColor cs c
                    writeIORef coreRef (Just cs)
                Just cs -> colorButtonSetColor cs c)
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just cs -> do
                    c <- colorButtonGetColor cs
                    return (Just c))
        parameters
        notifier

--
-- | An editor, which opens another editor
--   You have to inject a value before the button can be clicked.
--
otherEditor :: (alpha  -> String -> IO (Maybe alpha)) -> Editor alpha
otherEditor func parameters notifier = do
    coreRef <- liftIO $ newIORef Nothing
    mkEditor
        (\widget val -> reifyState $ \ stateR -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParaS "Name" parameters)
                    widgetSetName button $ getParaS "Name" parameters
                    containerAdd widget button
                    reflectState (do
                        mapM_ (activateGUIEvent (castToWidget button) notifier)
                             (Clicked:genericGUIEvents)
                        registerGUIEvent notifier [Clicked] (buttonHandler coreRef)
                        retriggerAsChanged notifier [KeyPressed,ButtonPressed,Clicked]) stateR
                    writeIORef coreRef (Just (button,val))
                Just (button, oldval) -> writeIORef coreRef (Just (button, val)))
        (liftIO $ do
            core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,val) -> return (Just val))
        (("Name",ParaString "") <<< parameters)
        notifier
    where
    buttonHandler coreRef e = liftIO $ do
        core <- readIORef coreRef
        case core of
            Nothing -> error "You have to inject a value before the button can be clicked"
            Just (b,val) -> do
                res <- func val (getParaS "Name" parameters)
                case res of
                    Nothing     -> return (e{geGtkReturn=True})
                    Just nval   -> do
                        writeIORef coreRef (Just (b, nval))
                        return (e{geGtkReturn=True})

--okCancelFields :: FieldDescription ()
--okCancelFields = HFD emptyParams [
--        mkField
--            (paraStockId <<<- ParaStockId stockCancel
--                $ paraName <<<- ParaName "Cancel"
--                    $ emptyParams)
--            (const ())
--            (\ _ b -> b)
--            (clickEditor False)
--    ,   mkField
--            (paraStockId <<<- ParaStockId stockOk
--                $ paraName <<<- ParaName "Ok"
--                    $ emptyParams)
--            (const ())
--            (\ a b -> b)
--            (clickEditor True)]

