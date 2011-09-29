{-# Language
    FunctionalDependencies,
    MultiParamTypeClasses,
    DeriveDataTypeable,
    ExistentialQuantification,
    StandaloneDeriving,
    FlexibleInstances,
    ScopedTypeVariables,
    FlexibleContexts,
    CPP,
    TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Frame
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Splittable panes containing notebooks with any widgets
--
---------------------------------------------------------------------------------


module Graphics.Frame (

        -- * Pane class
    Pane(..)

        -- * Actions
,   viewSwitchTabs
,   viewTabsPos
,   viewNewGroup
,   viewCollapse
,   viewSplitHorizontal
,   viewSplitVertical
,   viewMove
,   viewDetach
,   viewClosePane
,   quit

-- * Events
,   FrameEvent(..)
,   FrameEventSel(..)
,   triggerFrameEvent
,   getFrameEvent
,   registerFrameEvent

    -- * Internals
,   getMainWindow
,   handleNotebookSwitch
,   newNotebook
,   viewSplit'
,   paneDirectionToPosType
,   viewDetach'
,   viewNest'
,   mbPaneFromName
,   paneFromName
,   posTypeToPaneDirection
,   getNotebook
,   getPaned
,   viewCollapse'
,   allGroupNames
,   closeGroup
,   GenPane(..)

    -- * Accesing state
,   initialFrameState
,   registerFrameState
,   getUiManagerSt
,   getWindowsSt
,   setWindowsSt
,   getPanesSt
,   setPanesSt
,   getPaneMapSt
,   setPaneMapSt
,   getActivePaneSt
,   setActivePaneSt
,   getLayoutSt
,   setLayoutSt
,   setPaneTypes
,   getPaneTypes
,   getSessionExt
,   setSessionExt
,   getToolbar
,   setToolbar
,   getStatusbar
,   setStatusbar
,   getPanePathFromNB
,   setPanePathFromNB
,   getRecentPanes
,   setRecentPanes

) where

import Base
import Graphics.Panes
import Graphics.FrameTypes

import Graphics.UI.Gtk hiding (afterToggleOverwrite,onToggleOverwrite)
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List
import Data.Maybe
import Data.Unique
import Data.Typeable
import Data.Version

import System.Glib (GObjectClass(..), isA)
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk.Layout.Notebook (gTypeNotebook)
#else
import Graphics.UI.Gtk.Types (gTypeNotebook)
#endif
import System.CPUTime (getCPUTime)
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk.Gdk.EventM (Modifier(..))
#else
import Graphics.UI.Gtk.Gdk.Enums (Modifier(..))
#endif
import Graphics.UI.Gtk.Gdk.EventM (TimeStamp(..))
import qualified Data.Set as  Set (unions, member)
import Data.Set (Set(..))
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Data.IORef(newIORef)

-- import Debug.Trace (trace)
trace a b = b


-- ----------------------------------
-- * Events
--
data FrameEventSel = FrameEventSel
    deriving (Eq, Ord, Show, Typeable)

instance Selector FrameEventSel where
    type ValueType FrameEventSel = PEvent FrameEvent

--
-- | Events the gui frame triggers
--
data FrameEvent =
      ActivatePane String
    | DeactivatePane String
    | MovePane String
    | ChangeLayout
    | RegisterActions [ActionDescr]
    | RegisterPane [(String, GenPane)]
    | RegisterSessionExt [GenSessionExtension]
    | RegisterStatusbarComp [CompDescr]

makeFrameEvent :: StateM(PEvent FrameEvent)
makeFrameEvent = makeEvent FrameEventSel

triggerFrameEvent :: FrameEvent -> StateM(FrameEvent)
triggerFrameEvent          = triggerEvent FrameEventSel

getFrameEvent :: StateM (PEvent FrameEvent)
getFrameEvent              = getEvent FrameEventSel

registerFrameEvent hdl = getFrameEvent >>= \ev -> registerEvent ev hdl

-- ------------------------------------
-- * The state connected with frames
--

-- | Shows the state for the implementation of the GUI Frame
--
data FrameState = FrameState {
    fsUiManager       ::  UIManager
,   fsWindows         ::  [Window]
,   fsPanes           ::  Map PaneName GenPane
,   fsPaneMap         ::  (Map PaneName (PanePath, Connections)) -- these connections are from the build
,   fsActivePane      ::  Maybe (PaneName, Connections)          -- and these connections from activate
,   fsPanePathFromNB  ::  Map Notebook PanePath
,   fsLayout          ::  PaneLayout
,   fsRecentPanes     ::  [PaneName]
,   fsPaneTypes       ::  [(String,GenPane)]     -- ^ The string is the paneType of the pane
                                                -- the second arg encapsulates the real type
,   fsSessionExt      ::  [GenSessionExtension]
,   fsToolbar         ::  (Maybe Toolbar)
,   fsStatusbar       ::  (Map CompName CompWidget, Maybe HBox)}
    deriving Typeable
--
-- |  Empty initial frame state
--
initialFrameState uim = FrameState {
    fsUiManager       =   uim
,   fsWindows         =   []
,   fsPanes           =   Map.empty
,   fsPaneMap         =   Map.empty
,   fsActivePane      =   Nothing
,   fsPanePathFromNB  =   Map.empty
,   fsLayout          =   initialLayout
,   fsRecentPanes     =   []
,   fsPaneTypes       =   []
,   fsSessionExt      =   []
,   fsToolbar         =   (Nothing)
,   fsStatusbar       =   (Map.empty,Nothing)}


data GenPane        =   forall alpha . Pane alpha  => PaneC alpha

instance Eq GenPane where
    (==) (PaneC x) (PaneC y) = paneName x == paneName y

instance Ord GenPane where
    (<=) (PaneC x) (PaneC y) = paneName x <=  paneName y

instance Show GenPane where
    show (PaneC x)    = "Pane " ++ paneName x

-- ---------------------------------------------------------------------
-- * Accessor functions
--

getThis :: (FrameState -> alpha) -> StateM alpha
getThis sel = do
    st <- getFrameState
    return (sel st)

setThis :: (FrameState -> alpha -> FrameState) -> alpha -> StateM ()
setThis sel value = do
    st <- getFrameState
    setFrameState (sel st value)

getWindowsSt    = getThis fsWindows
setWindowsSt    = setThis (\st value -> st{fsWindows = value})
getUiManagerSt  = getThis fsUiManager
getPanesSt      = getThis fsPanes
setPanesSt      = setThis (\st value -> st{fsPanes = value})
getPaneMapSt    = getThis fsPaneMap
setPaneMapSt    = setThis (\st value -> st{fsPaneMap = value})
getActivePaneSt = getThis fsActivePane
setActivePaneSt = setThis (\st value -> st{fsActivePane = value})
getLayoutSt     = getThis fsLayout
setLayoutSt     = setThis (\st value -> st{fsLayout = value})
getPanePathFromNB  = getThis fsPanePathFromNB
setPanePathFromNB  = setThis (\st value -> st{fsPanePathFromNB = value})
getRecentPanes  = getThis fsRecentPanes
setRecentPanes  = setThis (\st value -> st{fsRecentPanes = value})
getPaneTypes    = getThis fsPaneTypes
setPaneTypes    = setThis (\st value -> st{fsPaneTypes = value})
getSessionExt   = getThis fsSessionExt
setSessionExt   = setThis (\st value -> st{fsSessionExt = value})
getToolbar      = getThis fsToolbar
setToolbar      = setThis (\st value -> st{fsToolbar = value})
getStatusbar    = getThis fsStatusbar
setStatusbar    = setThis (\st value -> st{fsStatusbar = value})


--
-- | The handling of the state of the frame
--

data FrameStateSel = FrameStateSel
    deriving (Eq, Ord, Show, Typeable)

instance Selector FrameStateSel where
    type ValueType FrameStateSel = FrameState


registerFrameState :: FrameState -> StateM (Maybe String)
registerFrameState = registerState FrameStateSel

setFrameState :: FrameState -> StateM ()
setFrameState      = setState FrameStateSel

getFrameState :: StateM (FrameState)
getFrameState      = getState FrameStateSel

-- | Quit ide -- TODO
quit :: StateAction
quit = liftIO mainQuit

--  ----------------------------------------
--  * The main interface to the frame system

--
-- | All kinds of panes are instances of Pane
--
class PaneInterface alpha => Pane alpha where

    paneName        ::   alpha -> PaneName
    -- ^ gets a string which names this pane, which may include an added index ...
    paneName b      =   if getAddedIndex b == 0
                            then primPaneName b
                            else primPaneName b ++ "(" ++ show (getAddedIndex b) ++ ")"

    getAddedIndex   ::   alpha -> Int
    -- ^ ..., which is used if more then one pane has the same name
    getAddedIndex _ =   0

    makeActive      ::   alpha -> Connections -> StateM ()
    -- ^ activates this pane, should probably be private
    makeActive pane conn = do
        mbAP <- getActivePaneSt
        case mbAP of
            Just (pn,_) | pn == paneName pane -> return ()
            _  -> do
                deactivatePane
                liftIO $ bringPaneToFront pane
                setActivePaneSt (Just (paneName pane,conn))
    -- use it for error reporting
                triggerFrameEvent (ActivatePane (paneName pane))
                recent <- getRecentPanes
                setRecentPanes
                    (paneName pane : filter (/= paneName pane) recent)
                return ()

    closePane       ::   alpha -> StateM Bool
    -- ^ closes this pane
    closePane pane = do
        (panePath,conn)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  liftIO $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  liftIO $ do
                error ("notebook page not found: unexpected " ++ paneName pane ++ " " ++ show panePath)
                return False
            Just i  ->  do
                liftIO $ signalDisconnectAll conn
                liftIO $ do
                    notebookRemovePage nb i
                    widgetDestroy (getTopWidget pane)
                deactivatePaneIfActive pane
                removePaneAdmin pane
                recent <- getRecentPanes
                setRecentPanes  (filter (/= paneName pane) recent)
                return True

    getPane         ::   StateM (Maybe alpha)
    -- ^get a pane of this type, if one is open
    getPane = do
        selectedPanes <- getPanes
        if null selectedPanes || length selectedPanes > 1
            then return Nothing
            else (return (Just $ head selectedPanes))

    forceGetPane    ::  Either PanePath String  -> StateM alpha
    -- ^get a pane of this type, if not one is open panic
    forceGetPane pp =   do  mbPane <- getOrBuildPane pp
                            case mbPane of
                                Nothing -> error "Can't get pane "
                                Just p -> return p

    getOrBuildPane  ::  Either PanePath String -> StateM (Maybe alpha)
    -- ^get a pane of this type, if one is open, or build one and for this specify either
    -- a pane path to put it, or a group name, from which a pane path may be derived
    getOrBuildPane ePpoPid =  do
        mbPane <- getPane
        case mbPane of
            Nothing -> do
                pp          <-  case ePpoPid of
                                    Right pId  -> getBestPathForId pId
                                    Left ppp -> do
                                        layout      <- getLayoutSt
                                        return (getBestPanePath ppp layout)
                nb          <-  getNotebook pp
                buildPane pp nb builder
            Just pane ->   return (Just pane)


    displayPane     ::  alpha -> Bool -> StateM ()
    -- ^ makes this pane visible
    displayPane pane shallGrabFocus = do
        liftIO $ bringPaneToFront pane
        when shallGrabFocus $ liftIO $ widgetGrabFocus $ getTopWidget pane


    getOrBuildDisplay :: Either PanePath String -> Bool  -> StateM (Maybe alpha)
    -- ^ is a concatination of getOrBuildPane and displayPane
    getOrBuildDisplay pps b = do
        mbP <- getOrBuildPane pps
        case mbP of
            Nothing -> return Nothing
            Just p  -> do
                displayPane p b
                return (Just p)

    buildPane       ::  PanePath ->
                        Notebook ->
                        (PanePath -> Notebook -> Window -> StateM (Maybe alpha,Connections)) ->
                        StateM (Maybe alpha)
    buildPane panePath notebook builder = do
        windows       <-  getWindowsSt

        (mbBuf,cids)  <-  builder panePath notebook (head windows)
        case mbBuf of
            Nothing -> return Nothing
            Just buf -> do
                panes'          <-  getPanesSt
                paneMap'        <-  getPaneMapSt
                let b1 = case Map.lookup (paneName buf) paneMap' of
                            Nothing -> True
                            Just it -> False
                let b2 = case Map.lookup (paneName buf) panes' of
                            Nothing -> True
                            Just it -> False
                if b1 && b2
                    then do
                        idx <- notebookInsertOrdered notebook (getTopWidget buf) (paneName buf) Nothing False
                        mbPage <- liftIO $ notebookGetNthPage notebook idx
                        mbCid <- case mbPage of
                                    Nothing -> return Nothing
                                    Just page -> liftM Just (reifyState (\ stateR ->
                                        on (castToContainer page) setFocusChild (\ _ ->
                                                liftIO (reflectState (makeActive buf []) stateR))))
                        addPaneAdmin buf (case mbCid of {Nothing -> cids; Just c -> castCID c:cids}) panePath
                        liftIO $ do
                            widgetSetName (getTopWidget buf) (paneName buf)
                            widgetShowAll (getTopWidget buf)
                            widgetGrabFocus (getTopWidget buf)
                            bringPaneToFront buf
                        return (Just buf)
                    else return Nothing

    setChanged :: alpha -> Bool -> StateM ()
    -- ^ Set the state of this pane to changed or not changed
    setChanged pane hasChanged = liftIO $ do
        let topWidget = getTopWidget pane
        mbNb <- getNotebookForWidget topWidget
        case mbNb of
            Nothing -> return ()
            Just nb -> markLabel nb topWidget hasChanged


-- ---------------------------------------------------------------------
-- Activating and deactivating Panes.
-- This is here and not in Views because it needs some dependencies
-- (e.g. Events for history)
--

deactivatePane :: StateAction
deactivatePane = do
    mbAP    <-  getActivePaneSt
    case mbAP of
        Nothing      -> return ()
        Just (name,signals) -> do
            liftIO $ signalDisconnectAll signals
            setActivePaneSt Nothing
            triggerFrameEvent (DeactivatePane name)
            return ()

deactivatePaneWithoutEvents :: StateAction
deactivatePaneWithoutEvents = do
    mbAP    <-  getActivePaneSt
    case mbAP of
        Just (_,signals) -> liftIO $do
            signalDisconnectAll signals
        Nothing -> return ()
    setActivePaneSt Nothing

deactivatePaneIfActive :: Pane alpha => alpha -> StateAction
deactivatePaneIfActive pane = do
    mbActive <- getActivePaneSt
    case mbActive of
        Nothing -> return ()
        Just (n,_) -> if n == paneName pane
                        then deactivatePane
                        else return ()


addPaneAdmin :: Pane alpha => alpha -> Connections -> PanePath -> StateM Bool
addPaneAdmin pane conn pp = do
    panes'          <-  getPanesSt
    paneMap'        <-  getPaneMapSt
    liftIO $ widgetSetName (getTopWidget pane) (paneName pane)
    let b1 = case Map.lookup (paneName pane) paneMap' of
                Nothing -> True
                Just it -> False
    let b2 = case Map.lookup (paneName pane) panes' of
                Nothing -> True
                Just it -> False
    if b1 && b2
        then do
            setPaneMapSt (Map.insert (paneName pane) (pp, conn) paneMap')
            setPanesSt (Map.insert (paneName pane) (PaneC pane) panes')
            return True
        else do
            return False

paneFromName :: PaneName -> StateM GenPane
paneFromName pn = do
    mbPane <- mbPaneFromName pn
    case mbPane of
        Just p -> return p
        Nothing -> error $ "ViewFrame>>paneFromName:Can't find pane from unique name " ++ pn

mbPaneFromName :: PaneName -> StateM (Maybe GenPane)
mbPaneFromName pn = do
    panes  <- getPanesSt
    return (Map.lookup pn panes)

type StandardPath = PanePath

--
-- | Get a valid panePath from a standard path.
--
getBestPanePath :: StandardPath -> PaneLayout -> PanePath
getBestPanePath sp pl = reverse $ getStandard' sp pl []
    where
    getStandard' (GroupP group:sp) (TerminalP {paneGroups = groups}) p
        | group `Map.member` groups                 =   getStandard' sp (groups Map.! group) (GroupP group:p)
    getStandard' _ (TerminalP {}) p              =   p
    getStandard' (SplitP LeftP:sp) (VerticalP l r _) p     =   getStandard' sp l (SplitP LeftP:p)
    getStandard' (SplitP RightP:sp) (VerticalP l r _) p    =   getStandard' sp r (SplitP RightP:p)
    getStandard' (SplitP TopP:sp) (HorizontalP t b _) p    =   getStandard' sp t (SplitP TopP:p)
    getStandard' (SplitP BottomP:sp) (HorizontalP t b _) p =   getStandard' sp b (SplitP BottomP:p)
    -- if no match get leftmost topmost
    getStandard' _ (VerticalP l r _) p              =   getStandard' [] l (SplitP LeftP:p)
    getStandard' _ (HorizontalP t b _) p            =   getStandard' [] t (SplitP TopP:p)

--
-- | Get a standard path.
--
getBestPathForId :: String -> StateM PanePath
getBestPathForId  id = do
    p <- panePathForGroup id
    l <- getLayoutSt
    return (getBestPanePath p l)

findGroupPath :: String -> PaneLayout -> Maybe PanePath
findGroupPath group layout =
    let terminalPairs = terminalsWithPanePath layout
    in case (filter filterFunc terminalPairs) of
        [] -> Nothing
        (pp,_) : [] -> Just (pp ++ [GroupP group])
        _ -> error ("ViewFrame>>group name not unique: " ++ group)
    where
        filterFunc (_,(TerminalP groups _ _ _ _)) =  group  `Set.member` Map.keysSet groups
        filterFunc _                              =  error "ViewFrame>>findGroupPath: impossible"


terminalsWithPanePath :: PaneLayout -> [(PanePath,PaneLayout)]
terminalsWithPanePath pl = map (\ (pp,l) -> (reverse pp,l)) $ terminalsWithPP [] pl
    where
        terminalsWithPP pp t@(TerminalP groups _ _ _ _) =  [(pp,t)]
                                            ++ concatMap (terminalsFromGroup pp) (Map.toList groups)
        terminalsWithPP pp (VerticalP l r _)       =  terminalsWithPP (SplitP LeftP : pp) l
                                                        ++ terminalsWithPP (SplitP RightP : pp) r
        terminalsWithPP pp (HorizontalP t b _)     =  terminalsWithPP (SplitP TopP : pp) t
                                                        ++ terminalsWithPP (SplitP BottomP : pp) b
        terminalsFromGroup pp (name,layout)        =  terminalsWithPP (GroupP name : pp) layout



getNotebookOrPaned :: PanePath -> (Widget -> beta) -> StateM beta
getNotebookOrPaned p cf = do
    layout <- getLayoutSt
    mwn <- mainWindowName
    (widgetGet $ getWidgetNameList p layout mwn) cf

widgetGet :: [String] -> (Widget -> b) -> StateM (b)
widgetGet strL cf = do
    windows <- getWindowsSt
    r <- liftIO $chooseWidgetFromPath (map castToWidget windows) strL
    return (cf r)

getWidgetNameList :: PanePath -> PaneLayout -> String -> [String]
getWidgetNameList path layout mainWindowName = reverse $ nameList (reverse path) (reverse $ layoutsFromPath path layout)
    where
        nameList [] _ = reverse [mainWindowName,"topBox","root"]
        nameList (pe:_) (TerminalP{detachedId = Just id}:_) = [panePathElementToWidgetName pe, id]
        nameList (pe:rpath) (_:rlayout) = panePathElementToWidgetName pe : nameList rpath rlayout
        nameList _ _ = error $ "inconsistent layout (getWidgetNameList) " ++ show path ++ " " ++ show layout

mainWindowName :: StateM String
mainWindowName = do
    windows <- getWindowsSt
    case windows of
        [] -> return ""
        (w:_) -> liftIO (widgetGetName w)

--
-- | Bring the pane to the front position in its notebook
--
bringPaneToFront :: Pane alpha => alpha -> IO ()
bringPaneToFront pane = do
    let tv = getTopWidget pane
    setCurrentNotebookPages tv

setCurrentNotebookPages widget = do
    mbParent <- widgetGetParent widget
    case mbParent of
        Just parent -> do
            setCurrentNotebookPages parent
            if parent `isA` gTypeNotebook
                then do
                    mbPageNum <- notebookPageNum ((castToNotebook' "setCurrentNotebookPage 1") parent) widget
                    case mbPageNum of
                        Just pageNum -> do
                            notebookSetCurrentPage ((castToNotebook' "setCurrentNotebookPage 2") parent) pageNum
                            return ()
                        Nothing      -> return ()
                else return ()
        Nothing -> return ()

getNotebookForWidget :: (WidgetClass alpha) => alpha -> IO (Maybe Notebook)
getNotebookForWidget widget = do
    mbParent <- widgetGetParent widget
    case mbParent of
        Just parent -> do
            if parent `isA` gTypeNotebook
                then return (Just (castToNotebook parent))
                else return Nothing
        Nothing -> return Nothing

-- | Translates a pane direction to the widget name
--
paneDirectionToWidgetName           :: PaneDirection -> String
paneDirectionToWidgetName TopP      =  "top"
paneDirectionToWidgetName BottomP   =  "bottom"
paneDirectionToWidgetName LeftP     =  "left"
paneDirectionToWidgetName RightP    =  "right"

panePathElementToWidgetName :: PanePathElement -> String
panePathElementToWidgetName (SplitP dir)   = paneDirectionToWidgetName dir
panePathElementToWidgetName (GroupP group) = groupPrefix ++ group

--
-- | Get the layout at the given pane path
--
layoutFromPath :: PanePath -> PaneLayout -> PaneLayout
layoutFromPath [] l                                   = l
layoutFromPath (GroupP group:r) (TerminalP {paneGroups = groups})
    | group `Map.member` groups                       = layoutFromPath r (groups Map.! group)
layoutFromPath (SplitP TopP:r) (HorizontalP t _ _)    = layoutFromPath r t
layoutFromPath (SplitP BottomP:r) (HorizontalP _ b _) = layoutFromPath r b
layoutFromPath (SplitP LeftP:r) (VerticalP l _ _)     = layoutFromPath r l
layoutFromPath (SplitP RightP:r) (VerticalP _ ri _)   = layoutFromPath r ri
layoutFromPath pp l                                   = error
    $"inconsistent layout (layoutFromPath) " ++ show pp ++ " " ++ show l

layoutsFromPath :: PanePath -> PaneLayout -> [PaneLayout]
layoutsFromPath (GroupP group:r) layout@(TerminalP {paneGroups = groups})
    | group `Map.member` groups
        = layout:layoutsFromPath r (groups Map.! group)
layoutsFromPath [] layout                                     =   [layout]
layoutsFromPath (SplitP TopP:r) layout@(HorizontalP t b _)    =   layout:layoutsFromPath r t
layoutsFromPath (SplitP BottomP:r) layout@(HorizontalP t b _) =   layout:layoutsFromPath r b
layoutsFromPath (SplitP LeftP:r) layout@(VerticalP l ri _)    =   layout:layoutsFromPath r l
layoutsFromPath (SplitP RightP:r) layout@(VerticalP l ri _)   =   layout:layoutsFromPath r ri
layoutsFromPath pp l                                      = error
    $"inconsistent layout (layoutsFromPath) " ++ show pp ++ " " ++ show l

--
-- | Get the widget from a list of strings
--
widgetFromPath :: Widget -> [String] -> IO (Widget)
widgetFromPath w [] = return w
widgetFromPath w path = do
    children    <- containerGetChildren (castToContainer w)
    chooseWidgetFromPath children path

chooseWidgetFromPath :: [Widget] -> [String] -> IO (Widget)
chooseWidgetFromPath _ [] = error $"Cant't find widget (empty path)"
chooseWidgetFromPath widgets (h:t) = do
    names       <- mapM widgetGetName widgets
    let mbiInd  =  findIndex (== h) names
    case mbiInd of
        Nothing     -> error $"Cant't find widget path " ++ show (h:t) ++ " found only " ++ show names
        Just ind    -> widgetFromPath (widgets !! ind) t


--
-- | Get the notebook widget for the given pane path
--
getNotebook :: PanePath -> StateM  Notebook
getNotebook p = getNotebookOrPaned p (castToNotebook' ("getNotebook " ++ show p))

getNotebook' :: String -> PanePath -> StateM  Notebook
getNotebook' str p = getNotebookOrPaned p (castToNotebook' ("getNotebook' " ++ str ++ " " ++ show p))


castToNotebook' :: GObjectClass obj => String -> obj -> Notebook
castToNotebook' str obj = if obj `isA` gTypeNotebook
                            then castToNotebook obj
                            else error ("Not a notebook " ++ str)

notebookInsertOrdered :: (NotebookClass self, WidgetClass child)		
    => self	
    -> child	-- child - the Widget to use as the contents of the page.
    -> String
    -> Maybe Label	-- the label for the page as String or Label
    -> Bool
    -> StateM Int
notebookInsertOrdered nb widget labelStr mbLabel isGroup = do
    label	    <-  case mbLabel of
                        Nothing  -> liftIO $ labelNew (Just labelStr)
                        Just l  -> return l
    menuLabel   <-  liftIO $ labelNew (Just labelStr)
    numPages    <-  liftIO $ notebookGetNPages nb
    mbWidgets   <-  liftIO $ mapM (notebookGetNthPage nb) [0 .. (numPages-1)]
    let widgets =   map (\v -> forceJust v "ViewFrame.notebookInsertOrdered: no widget") mbWidgets
    labelStrs   <-  liftIO $ mapM widgetGetName widgets
    let pos     =   case findIndex (\ s -> withoutGroupPrefix s > withoutGroupPrefix labelStr) labelStrs of
                        Just i  ->  i
                        Nothing ->  -1
    labelBox    <-  if isGroup then groupLabel labelStr else mkLabelBox label labelStr
    liftIO $ do
        markLabel nb labelBox False
        realPos     <-  notebookInsertPageMenu nb widget labelBox menuLabel pos
        widgetShowAll labelBox
        notebookSetCurrentPage nb realPos
        return realPos

--
-- | used to identify a group from a pane name
--
groupPrefix = "_group_"

--
-- | Get a pane name without group prefix
--
withoutGroupPrefix :: String -> String
withoutGroupPrefix s = case groupPrefix `stripPrefix` s of
                            Nothing -> s
                            Just s' -> s'


groupLabel :: String -> StateM EventBox
groupLabel group = do
    label <- liftIO $ labelNew Nothing
    liftIO $ labelSetUseMarkup label True
    liftIO $ labelSetMarkup label ("<b>" ++ group ++ "</b>")
    labelBox <- mkLabelBox label (groupPrefix ++ group)
    liftIO $ widgetShowAll labelBox
    return labelBox

-- | Add the change mark or removes it
markLabel :: (WidgetClass alpha, NotebookClass beta) => beta -> alpha -> Bool -> IO ()
markLabel nb topWidget modified = do
    mbBox   <- notebookGetTabLabel nb topWidget
    case mbBox of
        Nothing  -> return ()
        Just box -> do
            mbContainer <- binGetChild (castToBin box)
            case mbContainer of
                Nothing -> return ()
                Just container -> do
                    children <- containerGetChildren (castToContainer container)
                    let label = castToLabel $ forceHead children "ViewFrame>>markLabel: empty children"
                    text <- widgetGetName topWidget
                    labelSetUseMarkup (castToLabel label) True
                    labelSetMarkup (castToLabel label)
                        (if modified
                              then "<span foreground=\"red\">" ++ text ++ "</span>"
                          else text)


-- | Returns a label box
mkLabelBox :: Label -> String -> StateM EventBox
mkLabelBox lbl paneName = do
    (tb,lb) <- liftIO $ do
        miscSetAlignment (castToMisc lbl) 0.0 0.0
        miscSetPadding  (castToMisc lbl) 0 0

        labelBox  <- eventBoxNew
        eventBoxSetVisibleWindow labelBox False
        innerBox  <- hBoxNew False 0

        tabButton <- buttonNew
        widgetSetName tabButton "leksah-close-button"
        buttonSetFocusOnClick tabButton False
        buttonSetRelief tabButton ReliefNone
        buttonSetAlignment tabButton (0.0,0.0)

        image     <- imageNewFromStock stockClose IconSizeMenu
        mbPB <- widgetRenderIcon tabButton stockClose IconSizeMenu ""
        (height,width)   <-  case mbPB of
                                Nothing -> return (14,14)
                                Just pb -> do
                                h <- pixbufGetHeight pb
                                w <- pixbufGetWidth pb
                                return (h,w)
        on tabButton styleSet (\style -> do
            widgetSetSizeRequest tabButton (height + 2) (width + 2))
        containerSetBorderWidth tabButton 0
        containerAdd tabButton image

        boxPackStart innerBox lbl PackNatural 0
        boxPackEnd innerBox tabButton PackNatural 0

        containerAdd labelBox innerBox
        dragSourceSet labelBox [Button1] [ActionCopy,ActionMove]
        tl        <- targetListNew
        targetListAddTextTargets tl 0
        dragSourceSetTargetList labelBox tl
        on labelBox dragDataGet (\ cont id timeStamp -> do
            selectionDataSetText paneName
            return ())
        return (tabButton,labelBox)
    cl <- runInIO closeHandler
    liftIO $ onClicked tb (cl ())

    return lb
    where
        closeHandler :: () -> StateM ()
        closeHandler _ =    case groupPrefix `stripPrefix` paneName of
                                Just group  -> do
                                    closeGroup group
                                Nothing -> do
                                    (PaneC pane) <- paneFromName paneName
                                    closePane pane
                                    return ()

closeGroup :: String -> StateM ()
closeGroup groupName = do
    layout <- getLayoutSt
    let mbPath = findGroupPath groupName layout
    mainWindow <- getMainWindow
    case mbPath of
        Nothing -> message Warning ("ViewFrame>>closeGroup: Group path not found: " ++ groupName) >> return ()
        Just path -> do
            panesMap <- getPaneMapSt
            let nameAndpathList  = filter (\(a,pp) -> path `isPrefixOf` pp)
                            $ map (\(a,b) -> (a,fst b)) (Map.assocs panesMap)
            continue <- case nameAndpathList of
                            (_:_) -> liftIO $ do
                                md <- messageDialogNew (Just mainWindow) [] MessageQuestion ButtonsYesNo
                                    ("Group " ++ groupName ++ " not empty. Close with all contents?")
                                rid <- dialogRun md
                                widgetDestroy md
                                case rid of
                                    ResponseYes ->  return True
                                    otherwise   ->  return False
                            []  -> return True
            when continue $ do
                panes <- mapM paneFromName $ map fst nameAndpathList
                results <- mapM (\ (PaneC p) -> closePane p) panes
                when (foldr (&&) True results) $ do
                    nbOrPaned  <- getNotebookOrPaned path castToWidget
                    mbParent <- liftIO $ widgetGetParent nbOrPaned
                    case mbParent of
                        Nothing -> error "ViewFrame>>closeGroup: closeGroup: no parent"
                        Just parent -> liftIO $ containerRemove (castToContainer parent) nbOrPaned
                    setLayoutSt (removeGL path layout)
                    ppMap <- getPanePathFromNB
                    setPanePathFromNB (Map.filter (\pa -> not (path `isPrefixOf` pa)) ppMap)

getMainWindow   = liftM head getWindowsSt

--
-- | Remove group layout at a certain path
--
removeGL :: PanePath -> PaneLayout -> PaneLayout
removeGL [GroupP group] t@(TerminalP oldGroups _ _ _ _)
    | group `Map.member` oldGroups                        =  t{paneGroups = group `Map.delete` oldGroups}
removeGL (GroupP group:r)  old@(TerminalP {paneGroups = groups})
    | group `Map.member` groups                             = old{paneGroups = Map.adjust (removeGL r) group groups}
removeGL (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (removeGL r tp) bp 0
removeGL (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (removeGL r bp) 0
removeGL (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (removeGL r lp) rp 0
removeGL (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (removeGL r rp) 0
removeGL p l = error $"ViewFrame>>removeGL: inconsistent layout " ++ show p ++ " " ++ show l


removePaneAdmin :: Pane alpha =>  alpha -> StateM ()
removePaneAdmin pane = do
    panes'          <-  getPanesSt
    paneMap'        <-  getPaneMapSt
    setPanesSt      (Map.delete (paneName pane) panes')
    setPaneMapSt    (Map.delete (paneName pane) paneMap')


getPanePrim ::  Typeable alpha => Pane alpha => StateM (Maybe alpha)
getPanePrim = do
    selectedPanes <- getPanes
    if null selectedPanes || length selectedPanes > 1
        then return Nothing
        else (return (Just $ head selectedPanes))

--
-- | Get all panes of a certain type
--
getPanes ::  Typeable alpha => Pane alpha => StateM ([alpha])
getPanes = do
    panes' <- getPanesSt
    return (catMaybes
                $ map (\(PaneC p) -> cast p)
                    $ Map.elems panes')



-- | Constructs a unique pane name, which is an index and a string
figureOutPaneName :: String -> Int -> StateM (Int,String)
figureOutPaneName bn ind = do
    bufs <- getPanesSt
    let ind = foldr (\(PaneC buf) ind ->
                if primPaneName buf == bn
                    then max ind ((getAddedIndex buf) + 1)
                    else ind)
                0 (Map.elems bufs)
    if ind == 0
        then return (0,bn)
        else return (ind,bn ++ "(" ++ show ind ++ ")")


-- |
guiPropertiesFromName :: PaneName -> StateM (PanePath, Connections)
guiPropertiesFromName pn = do
    paneMap <- getPaneMapSt
    case Map.lookup pn paneMap of
            Just it -> return it
            otherwise  -> error $"Cant't find guiProperties from unique name " ++ pn

posTypeToPaneDirection PosLeft      =   LeftP
posTypeToPaneDirection PosRight     =   RightP	
posTypeToPaneDirection PosTop       =   TopP
posTypeToPaneDirection PosBottom    =   BottomP	

paneDirectionToPosType LeftP        =   PosLeft
paneDirectionToPosType RightP       =   PosRight   	
paneDirectionToPosType TopP         =   PosTop
paneDirectionToPosType BottomP      =   PosBottom

--
-- | Closes the current pane
--
viewClosePane :: StateM  ()
viewClosePane = do
    mbPane <- getActivePaneSt
    case mbPane of
        Nothing -> do
            return ()
        Just (paneName,_) -> do
            (PaneC pane) <- paneFromName paneName
            closePane pane >> return ()

--
-- | Toggle the tabs of the current notebook
--
viewSwitchTabs :: StateM ()
viewSwitchTabs = do
    mbNb <- getActiveNotebook
    case mbNb of
        Nothing -> return ()
        Just nb -> liftIO $ do
            b <- notebookGetShowTabs nb
            notebookSetShowTabs nb (not b)

--
-- | Sets the tab position in the current notebook
--
viewTabsPos :: PositionType -> StateM ()
viewTabsPos pos = do
    mbNb <- getActiveNotebook
    case mbNb of
        Nothing -> return ()
        Just nb -> liftIO $notebookSetTabPos nb pos

--
-- | Split the currently active pane in horizontal direction
--
viewSplitHorizontal     :: StateM ()
viewSplitHorizontal     = viewSplit Horizontal

--
-- | Split the currently active pane in vertical direction
--
viewSplitVertical :: StateM ()
viewSplitVertical = viewSplit Vertical

--
-- | The active view can be split in two (horizontal or vertical)
--
viewSplit :: Direction -> StateM ()
viewSplit dir = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewSplit' panePath dir

viewSplit' :: PanePath -> Direction -> StateM ()
viewSplit' panePath dir = do
    l <- getLayoutSt
    case layoutFromPath panePath l of
        (TerminalP _ _ _ (Just _) _) -> message Warning ("ViewFrame>>viewSplit': can't split detached: ") >> return ()
        _                            -> do
            activeNotebook  <- (getNotebook' "viewSplit") panePath
            ind <- liftIO $ notebookGetCurrentPage activeNotebook
            mbPD <- do
                mbParent  <- liftIO $ widgetGetParent activeNotebook
                case mbParent of
                    Nothing -> message Warning ("ViewFrame>>viewSplit': parent not found: ") >> return Nothing
                    Just parent -> do
                        (nb,paneDir) <- do
                            let (name,altname,paneDir,
                                 oldPath,newPath) =  case dir of
                                                        Horizontal  -> ("top",
                                                                        "bottom",
                                                                        TopP,
                                                                        panePath ++ [SplitP TopP],
                                                                        panePath ++ [SplitP BottomP])
                                                        Vertical    -> ("left",
                                                                        "right",
                                                                        LeftP,
                                                                        panePath ++ [SplitP LeftP],
                                                                        panePath ++ [SplitP RightP])
                            adjustNotebooks panePath oldPath
                            ppNb <- getPanePathFromNB
                            setPanePathFromNB $ Map.insert activeNotebook oldPath ppNb
                            nb  <- newNotebook newPath
                            (np,nbi) <- liftIO $ do
                                newpane <- case dir of
                                              Horizontal  -> do  h <- vPanedNew
                                                                 return (castToPaned h)
                                              Vertical    -> do  v <- hPanedNew
                                                                 return (castToPaned v)
                                rName <- widgetGetName activeNotebook
                                widgetSetName newpane rName
                                widgetSetName nb altname
                                panedPack2 newpane nb True True
                                nbIndex <- if parent `isA` gTypeNotebook
                                            then notebookPageNum ((castToNotebook' "viewSplit'1") parent) activeNotebook
                                            else trace ("ViewFrame>>viewSplit': parent not a notebook: ")
                                                $ return Nothing
                                containerRemove (castToContainer parent) activeNotebook
                                widgetSetName activeNotebook name
                                panedPack1 newpane activeNotebook True True
                                return (newpane,nbIndex)
                            case (reverse panePath, nbi) of
                                (SplitP dir:_, _)
                                    | dir `elem` [TopP, LeftP] -> liftIO $ panedPack1 (castToPaned parent) np True True
                                    | otherwise                -> liftIO $ panedPack2 (castToPaned parent) np True True
                                (GroupP group:_, Just n) -> do
                                    liftIO $ notebookInsertPage ((castToNotebook' "viewSplit' 2") parent) np group n
                                    label <- groupLabel group
                                    liftIO $ notebookSetTabLabel ((castToNotebook' "viewSplit' 3") parent) np label
                                    label2 <- groupMenuLabel group
                                    liftIO $ notebookSetMenuLabel ((castToNotebook' "viewSplit' 4") parent) np label2
                                    return ()
                                ([], _) -> do
                                    liftIO $ boxPackStart (castToBox parent) np PackGrow 0
                                    liftIO $ boxReorderChild (castToVBox parent) np 2
                                _ -> error "No notebook index found in viewSplit"
                            liftIO $ do
                                widgetShowAll np
                                widgetGrabFocus activeNotebook
                                case nbi of
                                    Just n -> do
                                        notebookSetCurrentPage ((castToNotebook' "viewSplit' 5") parent) n
                                        return ()
                                    _      -> trace ("ViewFrame>>viewSplit': parent not a notebook2: ")
                                                $ return ()
                                return (nb,paneDir)
                        handleFunc <-  runInIO (handleNotebookSwitch nb)
                        liftIO $ afterSwitchPage nb handleFunc
                        return (Just (paneDir,dir))
            case mbPD of
              Just (paneDir,pdir) -> do
                  adjustPanes panePath (panePath ++ [SplitP paneDir])
                  adjustLayoutForSplit paneDir panePath
                  mbWidget <- liftIO $ notebookGetNthPage activeNotebook ind
                  when (isJust mbWidget) $ do
                    name <- liftIO $ widgetGetName (fromJust mbWidget)
                    mbPane  <- mbPaneFromName name
                    case mbPane of
                        Just (PaneC pane) -> move (panePath ++ [SplitP (otherDirection paneDir)]) pane
                        Nothing -> return ()
              Nothing -> return ()

--
-- | Two notebooks can be collapsed to one
--
viewCollapse :: StateM ()
viewCollapse = do
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewCollapse' panePath

viewCollapse' :: PanePath -> StateM ()
viewCollapse' panePath = do
    layout1           <- getLayoutSt
    case layoutFromPath panePath layout1 of
        (TerminalP _ _ _ (Just _) _) -> message Debug ("ViewFrame>>viewCollapse': can't collapse detached: ")
                                            >> return ()
        _                            -> do
            let newPanePath     = init panePath
            let mbOtherSidePath = otherSide panePath
            case mbOtherSidePath of
                Nothing -> trace ("ViewFrame>>viewCollapse': no other side path found: ") return ()
                Just otherSidePath -> do
                    nbop <- getNotebookOrPaned otherSidePath castToWidget
                    let nb = if nbop `isA` gTypeNotebook
                                then Just ((castToNotebook' "viewCollapse' 0") nbop)
                                else Nothing
                    case nb of
                        Nothing -> trace ("ViewFrame>>viewCollapse': other side path not collapsedXX: ") $
                                case layoutFromPath otherSidePath layout1 of
                                    VerticalP _ _ _ -> do
                                        viewCollapse' (otherSidePath ++ [SplitP LeftP])
                                        viewCollapse' panePath
                                    HorizontalP _ _ _ -> do
                                        viewCollapse' (otherSidePath ++ [SplitP TopP])
                                        viewCollapse' panePath
                                    otherwise -> trace ("ViewFrame>>viewCollapse': impossible1 ") return ()
                        Just otherSideNotebook -> do
                            paneMap           <- getPaneMapSt
                            activeNotebook    <- (getNotebook' "viewCollapse' 1") panePath
                            -- 1. Move panes and groups to one side (includes changes to paneMap and layout)
                            let paneNamesToMove = map (\(w,(p,_)) -> w)
                                                    $filter (\(w,(p,_)) -> otherSidePath == p)
                                                        $Map.toList paneMap
                            panesToMove       <- mapM paneFromName paneNamesToMove
                            mapM_ (\(PaneC p) -> move panePath p) panesToMove
                            let groupNames    =  map (\n -> groupPrefix ++ n) $
                                                        getGroupsFrom otherSidePath layout1
                            mapM_ (\n -> move' (n,activeNotebook)) groupNames
                            -- 2. Remove unused notebook from admin
                            ppNb <- getPanePathFromNB
                            let ! newMap = Map.delete otherSideNotebook ppNb
                            setPanePathFromNB newMap
                            -- 3. Remove one level and reparent notebook
                            mbParent <- liftIO $ widgetGetParent activeNotebook
                            case mbParent of
                                Nothing -> error "collapse: no parent"
                                Just parent -> do
                                    mbGrandparent <- liftIO $ widgetGetParent parent
                                    case mbGrandparent of
                                        Nothing -> error "collapse: no grandparent"
                                        Just grandparent -> do
                                            nbIndex <- if grandparent `isA` gTypeNotebook
                                                then liftIO $ notebookPageNum ((castToNotebook' "viewCollapse'' 1") grandparent) parent
                                                else return Nothing
                                            liftIO $ containerRemove (castToContainer grandparent) parent
                                            liftIO $ containerRemove (castToContainer parent) activeNotebook
                                            if length panePath > 1
                                                then do
                                                    let lasPathElem = last newPanePath
                                                    case (lasPathElem, nbIndex) of
                                                        (SplitP dir, _) | dir == TopP || dir == LeftP ->
                                                            liftIO $ panedPack1 (castToPaned grandparent) activeNotebook True True
                                                        (SplitP dir, _) | dir == BottomP || dir == RightP ->
                                                            liftIO $ panedPack2 (castToPaned grandparent) activeNotebook True True
                                                        (GroupP group, Just n) -> do
                                                            liftIO $ notebookInsertPage ((castToNotebook' "viewCollapse'' 2") grandparent) activeNotebook group n
                                                            label <- groupLabel group
                                                            liftIO $ do
                                                                notebookSetTabLabel ((castToNotebook' "viewCollapse'' 3") grandparent) activeNotebook label
                                                                notebookSetCurrentPage ((castToNotebook' "viewCollapse'' 4") grandparent) n
                                                                return ()
                                                        _ -> error "collapse: Unable to find page index"
                                                    liftIO $ widgetSetName activeNotebook $panePathElementToWidgetName lasPathElem
                                                else liftIO $ do
                                                    boxPackStart (castToVBox grandparent) activeNotebook PackGrow 0
                                                    boxReorderChild (castToVBox grandparent) activeNotebook 2
                                                    widgetSetName activeNotebook "root"
                            -- 4. Change panePathFromNotebook
                            adjustNotebooks panePath newPanePath
                            -- 5. Change paneMap
                            adjustPanes panePath newPanePath
                            -- 6. Change layout
                            adjustLayoutForCollapse panePath

getGroupsFrom :: PanePath -> PaneLayout -> [String]
getGroupsFrom path layout =
    case layoutFromPath path layout of
        t@(TerminalP _ _ _ _ _)   -> Map.keys (paneGroups t)
        HorizontalP _ _ _   -> []
        VerticalP _ _ _     -> []

viewNewGroup :: StateM ()
viewNewGroup = do
    mainWindow <- getMainWindow
    mbGroupName <- liftIO $ groupNameDialog mainWindow
    case
     mbGroupName of
        Just groupName -> do
            layout <- getLayoutSt
            if groupName `Set.member` allGroupNames layout
                then liftIO $ do
                    md <- messageDialogNew (Just mainWindow) [] MessageWarning ButtonsClose
                        ("Group name not unique " ++ groupName)
                    dialogRun md
                    widgetDestroy md
                    return ()
                else viewNest groupName
        Nothing -> return ()

newGroupOrBringToFront :: String -> PanePath -> StateM (Maybe PanePath,Bool)
newGroupOrBringToFront groupName pp = do
    layout <- getLayoutSt
    if groupName `Set.member` allGroupNames layout
        then do
            mbPP <- bringGroupToFront groupName
            return (mbPP,False)
        else let realPath = getBestPanePath pp layout in do
            viewNest' realPath groupName
            return (Just (realPath ++ [GroupP groupName]),True)

bringGroupToFront :: String -> StateM (Maybe PanePath)
bringGroupToFront groupName = do
    layout <- getLayoutSt
    case findGroupPath groupName layout   of
        Just path -> do
            widget <- getNotebookOrPaned path castToWidget
            liftIO $ setCurrentNotebookPages widget
            return (Just path)
        Nothing -> return Nothing


--  Yet another stupid little dialog

groupNameDialog :: Window -> IO (Maybe String)
groupNameDialog parent =  liftIO $ do
    dia                        <-   dialogNew
    windowSetTransientFor dia parent
    windowSetTitle dia "Enter group name"
    upper                      <-   dialogGetUpper dia
    lower                      <-   dialogGetActionArea dia

    buttonBox                  <-   hButtonBoxNew
    okButton                   <-   buttonNewFromStock "gtk-ok"
    cancelButton               <-   buttonNewFromStock "gtk-cancel"
    onClicked okButton (dialogResponse dia ResponseOk)
    onClicked cancelButton (dialogResponse dia ResponseCancel)
    boxPackStartDefaults buttonBox cancelButton
    boxPackStartDefaults buttonBox okButton
    boxPackStart lower buttonBox PackNatural 7
    dialogSetDefaultResponse dia ResponseOk

    label                      <-   labelNew (Just "Group Name")
    textField                  <-   entryNew
--    entrySetActivatesDefault textField True
    windowSetDefault dia (Just okButton)
    boxPackStartDefaults upper label
    boxPackStartDefaults upper textField


    widgetShowAll dia
    resp  <- dialogRun dia
    value <- entryGetText textField
    widgetDestroy dia
    case resp of
        ResponseOk | value /= ""       -> return (Just value)
        _                             -> return Nothing

viewNest ::  String -> StateM ()
viewNest group = do
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewNest' panePath group

viewNest' :: PanePath -> String -> StateM ()
viewNest' panePath group = do
    activeNotebook  <- (getNotebook' "viewNest' 1") panePath
    mbParent  <- liftIO $ widgetGetParent activeNotebook
    case mbParent of
        Nothing -> return ()
        Just parent -> do
            layout          <-  getLayoutSt
            let paneLayout  =   layoutFromPath panePath layout
            case paneLayout of
                (TerminalP {}) -> do
                    nb <- newNotebook (panePath ++ [GroupP group])
                    liftIO $ widgetSetName nb (groupPrefix ++ group)
                    notebookInsertOrdered activeNotebook nb group Nothing True
                    liftIO $ widgetShowAll nb
                        --widgetGrabFocus activeNotebook
                    handleFunc <-  runInIO (handleNotebookSwitch nb)
                    liftIO $ afterSwitchPage nb handleFunc
                    adjustLayoutForNest group panePath
                _ -> return ()

viewDetach :: StateM (Maybe (Window,Widget))
viewDetach = do
    id <- liftIO $ fmap show getCPUTime
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return Nothing
        Just panePath -> do
            viewDetach' panePath id

viewDetach' :: PanePath -> String -> StateM (Maybe (Window,Widget))
viewDetach' panePath id = do
    activeNotebook  <- (getNotebook' "viewDetach'") panePath
    mbParent  <- liftIO $ widgetGetParent activeNotebook
    case mbParent of
        Nothing -> return Nothing
        Just parent -> do
            layout          <-  getLayoutSt
            let paneLayout  =   layoutFromPath panePath layout
            case paneLayout of
                (TerminalP{detachedSize = size}) -> do
                    window <- liftIO $ do
                        window <- windowNew
                        windowSetTitle window "Leksah detached window"
                        widgetSetName window id
                        case size of
                            Just (width, height) -> do
                                windowSetDefaultSize window width height
                            Nothing -> do
                                (curWidth, curHeight) <- widgetGetSize activeNotebook
                                windowSetDefaultSize window curWidth curHeight
                        containerRemove (castToContainer parent) activeNotebook
                        containerAdd window activeNotebook
                        widgetShowAll window
                        return window
                    handleFunc <-  runInIO (handleReattach id window)
                    liftIO $ window `onDelete` handleFunc
                    windows <- getWindowsSt
                    setWindowsSt $ windows ++ [window]
                    adjustLayoutForDetach id panePath
                    return (Just (window, castToWidget activeNotebook))
                _ -> return Nothing



handleReattach :: String -> Window -> Event -> StateM Bool
handleReattach windowId window _ = do
    layout <- getLayoutSt
    case findDetachedPath windowId layout of
        Nothing -> trace ("ViewFrame>>handleReattach: panePath for id not found: " ++ windowId)
                $ do
            windows <- getWindowsSt
            setWindowsSt $ delete window windows
            return False
        Just pp -> do
            nb      <- (getNotebook' "handleReattach") pp
            parent  <- getNotebookOrPaned (init pp) castToContainer
            liftIO $ containerRemove (castToContainer window) nb
            liftIO $ containerAdd parent nb
            adjustLayoutForReattach pp
            windows <- getWindowsSt
            setWindowsSt $ delete window windows
            case last pp of
                GroupP groupName -> do
                    label <- groupLabel groupName
                    liftIO $ notebookSetTabLabel ((castToNotebook' "handleReattach") parent) nb label
                otherwise       -> return ()
            return False -- "now destroy the window"



groupMenuLabel :: String -> StateM (Maybe Label)
groupMenuLabel group = liftM Just (liftIO $ labelNew (Just group))

handleNotebookSwitch :: Notebook -> Int -> StateM ()
handleNotebookSwitch nb index = do
    mbW <- liftIO $ notebookGetNthPage nb index
    case mbW of
        Nothing -> error "ViewFrame/handleNotebookSwitch: Can't find widget"
        Just w  -> do
            name   <-  liftIO $ widgetGetName w
            mbPane <-  findPaneFor name
            case mbPane of
                Nothing         ->  return ()
                Just (PaneC p)  ->  makeActive p []
    where
        findPaneFor :: String -> StateM (Maybe GenPane)
        findPaneFor n1   =   do
            panes'      <-  getPanesSt
            foldM (\r (PaneC p) -> do
                n2 <- liftIO $ widgetGetName (getTopWidget p)
                return (if n1 == n2 then (Just (PaneC p)) else r))
                        Nothing (Map.elems panes')


--
-- | Moves the activePane in the given direction, if possible
-- | If their are many possibilities choose the leftmost and topmost
--
viewMove :: PaneDirection -> StateM  ()
viewMove direction = do
    mbPane <- getActivePaneSt
    case mbPane of
        Nothing -> do
            return ()
        Just (paneName,_) -> do
            (PaneC pane) <- paneFromName paneName
            mbPanePath <- getActivePanePath
            case mbPanePath of
                Nothing -> do
                    return ()
                Just panePath -> do
                  layout <- getLayoutSt
                  case findMoveTarget panePath layout direction of
                      Nothing -> do
                        return ()
                      Just moveTo -> move moveTo pane

--
-- | Find the target for a move
--
findMoveTarget :: PanePath -> PaneLayout -> PaneDirection -> Maybe PanePath
findMoveTarget panePath layout direction=
    let oppositeDir          = otherDirection direction
        canMove []           = []
        canMove reversedPath =
            case head reversedPath of
                SplitP d | d == oppositeDir
                    -> SplitP direction : (tail reversedPath)
                GroupP group -> []
                _                     -> canMove (tail reversedPath)
        basePath = reverse (canMove $ reverse panePath)
    in case basePath of
        [] -> Nothing
        _  -> let layoutP  = layoutFromPath basePath layout
             in  Just $basePath ++ findAppropriate layoutP oppositeDir

--
-- | Moves the given Pane to the given path
--
move ::  Pane alpha => PanePath -> alpha -> StateM ()
move toPanePath pane = do
    let name    = paneName pane
    toNB        <- (getNotebook' "move") toPanePath
    move' (name,toNB)

--
-- | Moves the given Pane to the given path, care for groups (layout, paneMap)
--
move' :: (PaneName,Notebook) -> StateM ()
move' (paneName,toNB) = do
    paneMap         <-  getPaneMapSt
    panes           <-  getPanesSt
    layout          <-  getLayoutSt
    case groupPrefix `stripPrefix` paneName of
        Just group  -> do
            case findGroupPath group layout of
                Nothing -> trace ("ViewFrame>>move': group not found: " ++ group) return ()
                Just fromPath -> do
                    groupNBOrPaned <- getNotebookOrPaned fromPath castToWidget
                    fromNB  <- (getNotebook' "move'") (init fromPath)
                    ppNb <- getPanePathFromNB
                    case toNB `Map.lookup` ppNb of
                        Nothing -> trace "ViewFrame>>move': panepath for Notebook not found1" return ()
                        Just toPath -> do
                            when (fromNB /= toNB && not (isPrefixOf fromPath toPath)) $ do
                                mbNum <- liftIO $ notebookPageNum fromNB groupNBOrPaned
                                case mbNum of
                                    Nothing ->  trace "ViewFrame>>move': group notebook not found" return ()
                                    Just num -> do
                                        liftIO $ notebookRemovePage fromNB num
                                        label <- groupLabel group
                                        notebookInsertOrdered toNB groupNBOrPaned group Nothing True
                                        liftIO $ notebookSetTabLabel toNB groupNBOrPaned label
                                        adjustPanes fromPath (toPath ++ [GroupP group])
                                        adjustLayoutForGroupMove fromPath toPath group
                                        adjustNotebooks fromPath (toPath ++ [GroupP group])
                                        layout2          <-  getLayoutSt
                                        return ()
        Nothing     ->
            case paneName `Map.lookup` panes of
                Nothing -> trace ("ViewFrame>>move': pane not found: " ++ paneName) return ()
                Just (PaneC pane) -> do
                    ppNb <- getPanePathFromNB
                    case toNB `Map.lookup` ppNb of
                        Nothing -> trace "ViewFrame>>move': panepath for Notebook not found2" return ()
                        Just toPath ->
                            case paneName `Map.lookup`paneMap of
                                Nothing -> trace ("ViewFrame>>move': pane data not found: " ++ paneName)
                                            return ()
                                Just (fromPath,_) -> do
                                    let child = getTopWidget pane
                                    (fromPane,cid)  <-  guiPropertiesFromName paneName
                                    fromNB          <-  (getNotebook' "move'") fromPane
                                    when (fromNB /= toNB) $ do
                                        mbNum <- liftIO $ notebookPageNum fromNB child
                                        case mbNum of
                                            Nothing ->  trace "ViewFrame>>move': widget not found" return ()
                                            Just num -> do
                                                liftIO $ notebookRemovePage fromNB num
                                                notebookInsertOrdered toNB child paneName Nothing False
                                                let paneMap1    =   Map.delete paneName paneMap
                                                setPaneMapSt    $   Map.insert paneName (toPath,cid) paneMap1

findAppropriate :: PaneLayout -> PaneDirection -> PanePath
findAppropriate  (TerminalP {}) _ =   []
findAppropriate  (HorizontalP t b _) LeftP     =   SplitP TopP    :  findAppropriate t LeftP
findAppropriate  (HorizontalP t b _) RightP    =   SplitP TopP    :  findAppropriate t RightP
findAppropriate  (HorizontalP t b _) BottomP   =   SplitP BottomP :  findAppropriate b BottomP
findAppropriate  (HorizontalP t b _) TopP      =   SplitP TopP    :  findAppropriate b TopP
findAppropriate  (VerticalP l r _) LeftP       =   SplitP LeftP   :  findAppropriate l LeftP
findAppropriate  (VerticalP l r _) RightP      =   SplitP RightP  :  findAppropriate r RightP
findAppropriate  (VerticalP l r _) BottomP     =   SplitP LeftP   :  findAppropriate l BottomP
findAppropriate  (VerticalP l r _) TopP        =   SplitP LeftP   :  findAppropriate l TopP

		
--
-- | Construct a new notebook
--
newNotebook' :: IO Notebook
newNotebook' = do
    nb <- notebookNew
    notebookSetTabPos nb PosTop
    notebookSetShowTabs nb True
    notebookSetScrollable nb True
    notebookSetPopup nb True
    return nb

--
-- | Construct a new notebook,
--
newNotebook :: PanePath -> StateM Notebook
newNotebook pp = do
    nb  <- liftIO newNotebook'
    ppNb <- getPanePathFromNB
    setPanePathFromNB $ Map.insert nb pp ppNb
    func <- runInIO move'
    liftIO $ do
        tl <- targetListNew
        targetListAddTextTargets tl 0
        dragDestSet nb [DestDefaultAll] [ActionCopy, ActionMove]
        dragDestSetTargetList nb tl
        on nb dragDataReceived (dragFunc nb func)
        return nb
    where
        dragFunc ::
            Notebook ->
            ((PaneName,Notebook) -> IO ()) ->
            DragContext ->
            Point ->
            InfoId ->
            TimeStamp ->
            (SelectionDataM ())
        dragFunc nb func cont point id timeStamp = do
            mbText <- selectionDataGetText
            case mbText of
                Nothing -> return ()
                Just str -> do
                    liftIO $ func (str,nb)
                    return ()

findDetachedPath :: String -> PaneLayout -> Maybe PanePath
findDetachedPath id layout =
    let terminalPairs = terminalsWithPanePath layout
    in case (filter filterFunc terminalPairs) of
        [] -> Nothing
        (pp,_) : [] -> Just pp
        _ -> error ("ViewFrame>>window id not unique: " ++ id)
    where
        filterFunc (_,(TerminalP _ _ _ (Just lid) _)) = lid == id
        filterFunc _                                  = False


allGroupNames :: PaneLayout -> Set String
allGroupNames pl = Set.unions $ map getFunc (terminalsWithPanePath pl)
    where
        getFunc (_,(TerminalP groups _ _ _ _)) =  Map.keysSet groups
        getFunc _                              =  error "ViewFrame>>allGroupNames: impossible"


--
-- | Get another pane path which points to the other side at the same level
--
otherSide :: PanePath -> Maybe PanePath
otherSide []    =   Nothing
otherSide p     =   let rp = reverse p
                    in case head rp of
                        SplitP d -> Just (reverse $ SplitP (otherDirection d) : tail rp)
                        _        -> Nothing

--
-- | Get the opposite direction of a pane direction
--
otherDirection :: PaneDirection -> PaneDirection
otherDirection LeftP    = RightP
otherDirection RightP   = LeftP
otherDirection TopP     = BottomP
otherDirection BottomP  = TopP

--
-- | Get the (gtk) Paned widget for a given path
--
getPaned :: PanePath -> StateM Paned
getPaned p = getNotebookOrPaned p castToPaned

--
-- | Get the path to the active pane
--
getActivePanePath :: StateM  (Maybe PanePath)
getActivePanePath = do
    mbPane   <- getActivePaneSt
    case mbPane of
        Nothing -> return Nothing
        Just (paneName,_) -> do
            (pp,_)  <- guiPropertiesFromName paneName
            return (Just pp)

getActivePanePathOrStandard :: StandardPath -> StateM  (PanePath)
getActivePanePathOrStandard sp = do
    mbApp <- getActivePanePath
    case mbApp of
        Just app -> return app
        Nothing -> do
            layout <- getLayoutSt
            return (getBestPanePath sp layout)

--
-- | Get the active notebook
--
getActiveNotebook :: StateM  (Maybe Notebook)
getActiveNotebook = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Just panePath -> do
            nb <- (getNotebook' "getActiveNotebook") panePath
            return (Just nb)
        Nothing -> return Nothing

--
-- | Changes a pane path in the pane map
--
adjustPanes :: PanePath -> PanePath -> StateM ()
adjustPanes fromPane toPane  = do
    paneMap     <- getPaneMapSt
    setPaneMapSt (Map.map (\(pp,other) ->
        case stripPrefix fromPane pp of
            Just rest -> (toPane ++ rest,other)
            _         -> (pp,other)) paneMap)

adjustNotebooks :: PanePath -> PanePath -> StateM ()
adjustNotebooks fromPane toPane  = do
    npMap <- trace ("+++ adjustNotebooks from: " ++ show fromPane ++ " to " ++ show toPane)
                getPanePathFromNB
    setPanePathFromNB  (Map.map (\pp ->
        case stripPrefix fromPane pp of
            Just rest -> toPane ++ rest
            _         -> pp) npMap)

--
-- | Changes the layout for a split
--
adjustLayoutForSplit :: PaneDirection -> PanePath -> StateM ()
adjustLayoutForSplit  dir path  = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newLayout   =   TerminalP Map.empty Nothing 0 Nothing Nothing
        newTerm     =   case dir of
                            LeftP   -> VerticalP paneLayout newLayout 0
                            RightP  -> VerticalP newLayout paneLayout 0
                            TopP    -> HorizontalP paneLayout newLayout 0
                            BottomP -> HorizontalP newLayout paneLayout 0
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a nest
--
adjustLayoutForNest :: String -> PanePath -> StateM ()
adjustLayoutForNest group path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {paneGroups = groups}) -> paneLayout {
                                paneGroups = Map.insert group (TerminalP Map.empty Nothing 0 Nothing Nothing) groups}
                            _          -> error "Unexpected layout type in adjustLayoutForNest"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a detach
--
adjustLayoutForDetach :: String -> PanePath -> StateM ()
adjustLayoutForDetach id path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {}) -> paneLayout {detachedId = Just id}
                            _              -> error "Unexpected layout type in adjustLayoutForDetach"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a reattach
--
adjustLayoutForReattach :: PanePath -> StateM ()
adjustLayoutForReattach path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {}) -> paneLayout {detachedId = Nothing, detachedSize = Nothing}
                            _   -> error "Unexpected layout type in adjustLayoutForReattach"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a collapse
--
adjustLayoutForCollapse :: PanePath -> StateM ()
adjustLayoutForCollapse oldPath  = do
    layout          <-  getLayoutSt
    let pathLayout  =   layoutFromPath oldPath layout
    setLayoutSt     $   adjustLayout (init oldPath) layout pathLayout

--
-- | Changes the layout for a move
--
adjustLayoutForGroupMove :: PanePath -> PanePath -> String -> StateM ()
adjustLayoutForGroupMove fromPath toPath group = do
    layout <- getLayoutSt
    let layoutToMove = layoutFromPath fromPath layout
    let newLayout = removeGL fromPath layout
    setLayoutSt (addGL layoutToMove (toPath ++ [GroupP group])  newLayout)

--
-- | Changes the layout for a remove
--
adjustLayoutForGroupRemove :: PanePath -> String -> StateM ()
adjustLayoutForGroupRemove fromPath group = do
    layout <- getLayoutSt
    setLayoutSt (removeGL fromPath layout)


--
-- | Add group layout at a certain path
--
addGL :: PaneLayout -> PanePath -> PaneLayout -> PaneLayout
addGL toAdd [GroupP group] t@(TerminalP oldGroups _ _ _ _)  =  t{paneGroups = Map.insert group toAdd oldGroups}
addGL toAdd (GroupP group:r)  old@(TerminalP {paneGroups = groups})
    | group `Map.member` groups = old{paneGroups       = Map.adjust (addGL toAdd r) group groups}
addGL toAdd (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (addGL toAdd r tp) bp 0
addGL toAdd (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (addGL toAdd r bp) 0
addGL toAdd (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (addGL toAdd r lp) rp 0
addGL toAdd (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (addGL toAdd r rp) 0
addGL _ p l = error $"ViewFrame>>addGL: inconsistent layout" ++ show p ++ " " ++ show l

--
-- | Changes the layout by replacing element at pane path (pp) with replace
--
adjustLayout :: PanePath -> PaneLayout -> PaneLayout -> PaneLayout
adjustLayout pp layout replace    = adjust' pp layout
    where
    adjust' [] _                                       = replace
    adjust' (GroupP group:r)  old@(TerminalP {paneGroups = groups})
        | group `Map.member` groups =
            old{paneGroups = Map.adjust (adjustPaneGroupLayout r) group groups}
    adjust' (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (adjust' r tp) bp 0
    adjust' (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (adjust' r bp) 0
    adjust' (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (adjust' r lp) rp 0
    adjust' (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (adjust' r rp) 0
    adjust' p l = error $"inconsistent layout (adjust) " ++ show p ++ " " ++ show l
    adjustPaneGroupLayout p group = adjust' p group



widgetGetRel :: Widget -> [String] -> (Widget -> b) -> IO (b)
widgetGetRel w sl cf = do
    r <- widgetFromPath w sl
    return (cf r)

getUIAction :: String -> (Action -> a) -> StateM (a)
getUIAction str f = do
    uiManager <- getUiManagerSt
    liftIO $ do
        findAction <- uiManagerGetAction uiManager str
        case findAction of
            Just act -> return (f act)
            Nothing  -> error $"getUIAction can't find action " ++ str


