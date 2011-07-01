-----------------------------------------------------------------------------
--group_Test
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
,   FieldDescription(..)
,   mkField
,   extractAndValidate
,   extract
,   mkEditor
,   parameters

,   flattenFieldDescription
,   getRealWidget
,   MkFieldDescription

) where


import Base.Event
import Base.State
import Graphics.Forms.Parameters
import Graphics.Forms.Basics
import Graphics.Forms.GUIEvent

import Graphics.UI.Gtk
import Control.Monad
import Data.List (intersperse, unzip4)
import Data.Maybe (isNothing)
import Data.IORef (newIORef)
import qualified Graphics.UI.Gtk.Gdk.Events as GTK (Event(..))
import Control.Monad.IO.Class (MonadIO(..))

--
-- | A constructor type for a field desciption
--
type MkFieldDescription alpha beta =
    Parameters ->
    (Getter alpha beta) ->
    (Setter alpha beta) ->
    (Editor beta) ->
    FieldDescription alpha

--
-- | A type to describe a field of a record, which can be edited
-- | alpha is the type of the individual field of the record
data FieldDescription alpha =  FD Parameters (alpha -> StateM (Widget, Injector alpha ,
                                    alpha -> Extractor alpha , GEvent)) -- Form
    | VFD Parameters [FieldDescription alpha] -- Vertical forms box
    | HFD Parameters [FieldDescription alpha] -- Horizontal forms box
    | NFD [(String,FieldDescription alpha)]   -- Notebook box

parameters :: FieldDescription alpha -> Parameters
parameters (FD p _) = p
parameters (VFD p _) = p
parameters (HFD p _) = p
parameters (NFD _) = emptyParams

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

buildEditor :: FieldDescription alpha -> alpha -> StateM (Widget, Injector alpha , alpha -> Extractor alpha ,
    GEvent)
buildEditor (FD paras editorf) v  =  editorf v
buildEditor (HFD paras descrs) v =   buildBoxEditor descrs Horizontal v
buildEditor (VFD paras descrs) v =   buildBoxEditor descrs Vertical v
buildEditor (NFD pairList)     v =   do
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

buildBoxEditor :: [FieldDescription alpha] -> Direction -> alpha
    -> StateM (Widget, Injector alpha , alpha -> Extractor alpha , GEvent)
buildBoxEditor descrs dir v = do
    resList <- mapM (\d -> buildEditor d v)  descrs
    notifier <- makeGUIEvent
    let (widgets, setInjs, getExts, notifiers) = unzip4 resList
    hb <- case dir of
            Horizontal -> do
                b <- liftIO $ hBoxNew False 0
                return (castToBox b)
            Vertical -> do
                b <- liftIO $ vBoxNew False 0
                return (castToBox b)
    let newInj = (\v -> mapM_ (\ setInj -> setInj v) setInjs)
    let fieldNames = map (\fd -> case getParameterPrim paraName (parameters fd) of
                                    Just s -> s
                                    Nothing -> "Unnamed") descrs
    let packParas = map (\fd -> getParameter paraPack (parameters fd)) descrs
    propagateEvent notifier notifiers
    let newExt = (\v -> extractAndValidate v getExts fieldNames notifier)
    liftIO $ mapM_ (\ (w,p) -> boxPackStart hb w p 0) $ zip widgets packParas
    return (castToWidget hb, newInj, newExt, notifier)


flattenFieldDescription :: FieldDescription alpha -> [FieldDescription alpha]
flattenFieldDescription (VFD paras descrs)  =   concatMap flattenFieldDescription descrs
flattenFieldDescription (HFD paras descrs)  =   concatMap flattenFieldDescription descrs
flattenFieldDescription (NFD descrp)        =   concatMap (flattenFieldDescription.snd) descrp
flattenFieldDescription fd                  =   [fd]

-- ------------------------------------------------------------
-- * Implementation of editing
-- ------------------------------------------------------------

--
-- | Function to construct a field description
--
mkField :: Eq beta => MkFieldDescription alpha beta
mkField parameters getter setter editor =
    FD parameters
        (\ dat -> do
            noti <- makeGUIEvent
            (widget,inj,ext) <- editor parameters noti
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
    let (xalign, yalign, xscale, yscale) = getParameter paraOuterAlignment parameters
    outerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter paraOuterPadding parameters
    alignmentSetPadding outerAlig paddingTop paddingBottom paddingLeft paddingRight
    frame   <-  frameNew
    frameSetShadowType frame (getParameter paraShadow parameters)
    case getParameter paraName parameters of
        "" -> return ()
        str -> if getParameter paraShowLabel parameters
                then frameSetLabel frame str
                else return ()

    containerAdd outerAlig frame
    let (xalign, yalign, xscale, yscale) =  getParameter paraInnerAlignment parameters
    innerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter paraInnerPadding parameters
    alignmentSetPadding innerAlig paddingTop paddingBottom paddingLeft paddingRight
    containerAdd frame innerAlig
    let (x,y) = getParameter paraMinSize parameters
    widgetSetSizeRequest outerAlig x y
    let name  =  getParameter paraName parameters
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




