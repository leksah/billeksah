{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.MakeEditor
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for making editors out of descriptions
--
-----------------------------------------------------------------------------------

module Graphics.Forms.Build (

    buildEditor

,   FieldDescriptionG(..)
,   GenFieldDescriptionG(..)
,   toFieldDescriptionG
,   mkFieldG

,   extractAndValidate
,   extract
,   mkEditor
,   parameters

,   getRealWidget
,   MkFieldDescriptionG
) where


import Base.Event
import Base.State
import Graphics.Forms.Parameters
import Graphics.Forms.Basics
import Graphics.Forms.GUIEvent
import Graphics.Panes (Direction(..))

import Graphics.UI.Gtk
import Control.Monad
import Data.List (intersperse, unzip4)
import Data.Maybe (isNothing)
import Data.IORef (newIORef)
import qualified Graphics.UI.Gtk.Gdk.Events as GTK (Event(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Typeable (Typeable)


--
-- | A constructor type for a field desciption
--
type MkFieldDescriptionG alpha beta =
    String ->
    Parameters ->
    (Getter alpha beta) ->
    (Setter alpha beta) ->
    (Editor beta) ->
    FieldDescriptionG alpha

--
-- | A type to describe a field of a record, which can be edited
-- | alpha is the type of the individual field of the record
data FieldDescriptionG alpha =  FieldG Parameters (alpha -> StateM (Widget, Injector alpha ,
                                    alpha -> Extractor alpha , GEvent)) -- Form
    | VertBoxG Parameters [FieldDescriptionG alpha] -- Vertical forms box
    | HoriBoxG Parameters [FieldDescriptionG alpha] -- Horizontal forms box
    | TabbedBoxG [(String,FieldDescriptionG alpha)]   -- Notebook box

data GenFieldDescriptionG = forall alpha . Typeable alpha => GenFG (FieldDescriptionG alpha) alpha


toFieldDescriptionG :: FieldDescription alpha  -> FieldDescriptionG alpha
toFieldDescriptionG (VertBox paras descrs) =  VertBoxG paras
                                                        (map toFieldDescriptionG descrs)
toFieldDescriptionG (HoriBox paras descrs) =  HoriBoxG paras
                                                        (map toFieldDescriptionG descrs)
toFieldDescriptionG (TabbedBox descrsp)    =  TabbedBoxG (map (\(s,d) ->
                                                    (s, toFieldDescriptionG d)) descrsp)
toFieldDescriptionG (Field parameters fieldPrinter fieldParser fieldEditor applicator) =
    (FieldG parameters fieldEditor)


parameters :: FieldDescriptionG alpha -> Parameters
parameters (FieldG p _) = p
parameters (VertBoxG p _) = p
parameters (HoriBoxG p _) = p
parameters (TabbedBoxG _) = defaultParams

--
-- | Construct a new notebook
--
newNotebook :: IO Notebook
newNotebook = do
    nb <- notebookNew
    notebookSetTabPos nb PosTop
    notebookSetShowTabs nb True
    notebookSetScrollable nb True
    notebookSetPopup nb True
    return nb

buildEditor :: FieldDescriptionG alpha -> alpha -> StateM (Widget, Injector alpha , alpha -> Extractor alpha ,
    GEvent)
buildEditor (FieldG paras editorf) v  =  editorf v
buildEditor (VertBoxG paras descrs) v =   buildBoxEditor paras descrs Vertical v
buildEditor (HoriBoxG paras descrs) v =   buildBoxEditor paras descrs Horizontal v
buildEditor (TabbedBoxG pairList)     v =   do
    reifyState $ \ stateR -> do
        nb <- newNotebook
        notebookSetShowTabs nb False
        resList <- reflectState (mapM (\d -> buildEditor d v) (map snd pairList)) stateR
        let (widgets, setInjs, getExts, notifiers) = unzip4 resList

        mapM_ (\ (labelString, widget) -> do
            sw <- scrolledWindowNew Nothing Nothing
            scrolledWindowAddWithViewport sw widget
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
            notebookAppendPage nb sw labelString)
             (zip (map fst pairList) widgets)
        listStore   <- listStoreNew (map fst pairList)
        listView    <- treeViewNewWithModel listStore
        widgetSetSizeRequest listView 100 (-1)
        sel         <- treeViewGetSelection listView
        treeSelectionSetMode sel SelectionSingle
        renderer    <- cellRendererTextNew
        col         <- treeViewColumnNew
        treeViewAppendColumn listView col
        cellLayoutPackStart col renderer True
        cellLayoutSetAttributes col renderer listStore $ \row ->
            [ cellText := row ]
        treeViewSetHeadersVisible listView False
        treeSelectionSelectPath sel [0]
        notebookSetCurrentPage nb 0
        sel `onSelectionChanged` (do
            selections <- treeSelectionGetSelectedRows sel
            case selections of
                [[i]] -> notebookSetCurrentPage nb i
                _ -> return ())

        hb      <-  hBoxNew False 0
        sw              <-  scrolledWindowNew Nothing Nothing
        containerAdd sw listView
        scrolledWindowSetPolicy sw PolicyNever PolicyAutomatic
        boxPackStart hb sw PackNatural 0
        boxPackEnd hb nb PackGrow 7
        let newInj = (\v -> mapM_ (\ setInj -> setInj v) setInjs)
        let newExt = (\v -> extract v getExts)
        notifier <- reflectState makeGUIEvent stateR
        reflectState (propagateEvent notifier notifiers) stateR
        return (castToWidget hb, newInj, newExt, notifier)

buildBoxEditor :: Parameters -> [FieldDescriptionG alpha] -> Direction -> alpha
    -> StateM (Widget, Injector alpha , alpha -> Extractor alpha , GEvent)
buildBoxEditor paras descrs dir v = do
    resList <- mapM (\d -> buildEditor d v)  descrs
    notifier <- makeGUIEvent
    let (widgets, setInjs, getExts, notifiers) = unzip4 resList
    case dir of
        Horizontal -> do
            let ParaBool hBoxHomogeneous = getPara "HBoxHomogeneous" paras
            b <- liftIO $ hBoxNew hBoxHomogeneous 0
            return (castToBox b)
            let newInj = (\v -> mapM_ (\ setInj -> setInj v) setInjs)
            let fieldNames = map (\fd -> getParaS "Name" (parameters fd)) descrs
            let packParas = map (\fd -> let ParaPack p = getPara "HPack" (parameters fd) in p) descrs
            propagateEvent notifier notifiers
            let newExt = (\v -> extractAndValidate v getExts fieldNames notifier)
            liftIO $ mapM_ (\ (w,p) -> boxPackStart b w p 0) $ zip widgets packParas
            return (castToWidget b, newInj, newExt, notifier)
        Vertical -> do
            let ParaBool vBoxHomogeneous = getPara "VBoxHomogeneous" paras
            b <- liftIO $ vBoxNew vBoxHomogeneous 0
            let newInj = (\v -> mapM_ (\ setInj -> setInj v) setInjs)
            let fieldNames = map (\fd -> getParaS "Name" (parameters fd)) descrs
            let packParas = map (\fd -> let ParaPack p = getPara "VPack" (parameters fd) in p) descrs
            propagateEvent notifier notifiers
            let newExt = (\v -> extractAndValidate v getExts fieldNames notifier)
            liftIO $ mapM_ (\ (w,p) -> boxPackStart b w p 0) $ zip widgets packParas
            return (castToWidget b, newInj, newExt, notifier)

flattenFieldDescriptionG :: FieldDescriptionG alpha -> [FieldDescriptionG alpha]
flattenFieldDescriptionG (VertBoxG paras descrs) = concatMap flattenFieldDescriptionG descrs
flattenFieldDescriptionG (HoriBoxG paras descrs) = concatMap flattenFieldDescriptionG descrs
flattenFieldDescriptionG (TabbedBoxG descrp)     = concatMap (flattenFieldDescriptionG.snd) descrp
flattenFieldDescriptionG fd                      =   [fd]

-- ------------------------------------------------------------
-- * Implementation of editing
-- ------------------------------------------------------------

--
-- | Function to construct a field description
--
mkFieldG :: Eq beta => MkFieldDescriptionG alpha beta
mkFieldG name parameters getter setter editor =
    let realParas = ("Name", ParaString name) <<< parameters
    in FieldG realParas
        (\ dat -> do
            noti <- makeGUIEvent
            (widget,inj,ext) <- editor realParas noti
            let pext = (\a -> do
                            b <- ext
                            case b of
                                Just b -> return (Just (setter b a))
                                Nothing -> return Nothing)
            inj (getter dat)
            return (widget,
                    (\a -> inj (getter a)),
                    pext,
                    noti))

-- | Function to construct an editor
--
mkEditor :: (Container -> Injector alpha) -> Extractor alpha -> Editor alpha
mkEditor injectorC extractor parameters notifier = liftIO $ do
    let ParaAlign (xalign, yalign, xscale, yscale) = getPara "OuterAlignment" parameters
    outerAlig <- alignmentNew xalign yalign xscale yscale
    let ParaPadding (paddingTop, paddingBottom, paddingLeft, paddingRight) = getPara "OuterPadding" parameters
    alignmentSetPadding outerAlig paddingTop paddingBottom paddingLeft paddingRight
    frame   <-  frameNew
    let ParaPos (x,y) = getPara "LabelAlign" parameters
    frameSetLabelAlign frame x y
    frameSetShadowType frame (let ParaShadow s = getPara "Shadow" parameters in s)
    case getParaS "Name" parameters of
        "" -> return ()
        str -> if getPara "ShowLabel" parameters == ParaBool True
                    then frameSetLabel frame str
                    else return ()
    case getParaS "Synopsis" parameters of
        "" -> return ()
        str -> set frame [widgetTooltipText := Just str]

    containerAdd outerAlig frame
    let ParaAlign (xalign, yalign, xscale, yscale) =  getPara "InnerAlignment" parameters
    innerAlig <- alignmentNew xalign yalign xscale yscale
    let ParaPadding (paddingTop, paddingBottom, paddingLeft, paddingRight) = getPara "InnerPadding" parameters
    alignmentSetPadding innerAlig paddingTop paddingBottom paddingLeft paddingRight
    containerAdd frame innerAlig
    let ParaSize (x,y) = getPara "MinSize" parameters
    widgetSetSizeRequest outerAlig x y
    let name  =  getParaS "Name" parameters
    widgetSetName outerAlig name
    let build = injectorC (castToContainer innerAlig)
    return (castToWidget outerAlig, build, extractor)


-- | Convenience method to validate and extract fields
--
extractAndValidate :: alpha -> [alpha -> Extractor alpha] -> [String] -> GEvent -> StateM (Maybe alpha)
extractAndValidate val getExts fieldNames notifier = do
    (newVal,errors) <- foldM (\ (val,errs) (ext,fn) -> do
        extVal <- ext val
        case extVal of
            Just nval -> return (nval,errs)
            Nothing -> return (val, (' ' : fn) : errs))
                (val,[]) (zip getExts fieldNames)
    if null errors
        then return (Just newVal)
        else do
            triggerGUIEvent notifier (dummyGUIEvent {
                    geSelector    = ValidationError,
                    geText        = concat (intersperse ", " errors)})
            return Nothing

extract :: alpha -> [alpha -> Extractor alpha] -> StateM (Maybe alpha)
extract val  =
    foldM (\ mbVal ext ->
        case mbVal of
            Nothing -> return Nothing
            Just val -> ext val)
            (Just val)

-- | get through outerAlignment, frame, innerAlignment
getRealWidget :: Widget -> StateM (Maybe Widget)
getRealWidget w = liftIO $ do
    mbF <- binGetChild (castToBin w)
    case mbF of
        Nothing -> return Nothing
        Just f -> do
            mbIA <- binGetChild (castToBin f)
            case mbIA of
                Nothing -> return Nothing
                Just iA -> binGetChild (castToBin iA)




