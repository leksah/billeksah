{-# Language DeriveDataTypeable, StandaloneDeriving, ExistentialQuantification,
    RankNTypes, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Menu
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Menu services
--
-----------------------------------------------------------------------------

module Graphics.Menu (
    initActions,
    setSensitivity,
    registerActionState,
    initialActionState
)where

import Base
import Graphics.FrameTypes
import Graphics.Frame
import Graphics.Panes


import Data.Version (Version(..))
import Debug.Trace (trace)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk
import Control.Monad (liftM, when, foldM_, filterM, foldM)
import Data.List (nub, elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, lookup, empty)
import Data.Maybe (mapMaybe, catMaybes)



--
-- | The handling of the state of the frame
--

type ActionState = Map GenSelector [String]

registerActionState :: ActionState -> StateM (Maybe String)
registerActionState = registerState ActionStateSel

setActionState :: ActionState -> StateM ()
setActionState    st  = trace ("setActionState: " ++ show st) $ setState ActionStateSel st

getActionState :: StateM (ActionState)
getActionState      = getState ActionStateSel

initialActionState = Map.empty

--------------------------------------------------------------
-- * MenuBar state

-- | * Builds the menu and toolbar from the action description,
--     and registers accelerators from the action descriptions
initActions :: UIManager -> [ActionDescr] -> StateM (MenuBar,Toolbar)
initActions uiManager actionDescrs = trace "initActions" $ do
    mb           <- liftIO $ menuBarNew
    tb           <- liftIO $ toolbarNew
    actionGroup  <- liftIO $ actionGroupNew "global"
    accGroup     <- liftIO $ uiManagerGetAccelGroup uiManager
    foldM_ (buildAction uiManager accGroup actionGroup tb mb) (Nothing,Nothing) actionDescrs
    liftIO $ uiManagerInsertActionGroup uiManager actionGroup 1
    setActionState (buildActionState actionDescrs)
    return (mb,tb)


buildActionState :: [ActionDescr] -> Map GenSelector [String]
buildActionState actionDescrs = Map.fromList $ zip allSensitivities (map actionStringsFor allSensitivities)
  where
    allSensitivities = nub $ concatMap adSensitivities actionDescrs
    actionStringsFor sens = mapMaybe (actionStringFor sens) actionDescrs
    actionStringFor sens AD{adName = name, adSensitivities = sensitivities}
        | elem sens sensitivities = Just name
        | otherwise               = Nothing

buildAction uiManager accGroup actionGroup toolBar mb lastPosition actionDescr =
    let (acc,accString) = case adAccelerator actionDescr of
                               Nothing ->  (Nothing,"=" ++ adName actionDescr)
                               Just ha ->  (Just ha, ha ++ "=" ++ adName actionDescr)
    in case adActionType actionDescr of
        ActionSubmenu -> reifyState $ \ stateR -> do
            fst <- buildMenuItem uiManager mb (Nothing :: Maybe Action)
                        actionDescr (fst lastPosition)
            return (fst, snd lastPosition)
        ActionNormal -> reifyState $ \ stateR -> do
            act <- actionNew (adName actionDescr)
                (adLabel actionDescr) (adSynopsis actionDescr) (adStockID actionDescr)
            actionSetAccelGroup act accGroup
            onActionActivate act (reflectState (adAction actionDescr) stateR)
            actionGroupAddActionWithAccel actionGroup act acc
            fst <- buildMenuItem uiManager mb (Just act) actionDescr (fst lastPosition)
            snd <- buildToolItem uiManager toolBar (Just act) actionDescr (snd lastPosition)
            return (fst,snd)
        ActionToggle -> reifyState $ \ stateR -> do
            act <- toggleActionNew (adName actionDescr)
                (adLabel actionDescr) (adSynopsis actionDescr) (adStockID actionDescr)
            actionSetAccelGroup act accGroup
            on act actionToggled (reflectState (adAction actionDescr) stateR)
            actionGroupAddActionWithAccel actionGroup act acc
            fst <- buildMenuItem uiManager mb (Just act) actionDescr (fst lastPosition)
            snd <- buildToolItem uiManager toolBar (Just act) actionDescr (snd lastPosition)
            return (fst,snd)



buildMenuItem :: ActionClass alpha => UIManager -> MenuBar -> Maybe alpha -> ActionDescr
                    -> Maybe (MenuShell,Int) -> IO (Maybe (MenuShell,Int))
buildMenuItem uiManager mb mbAction ad@AD{adMenu = menuPos, adName = name} mbLast
    | menuPos == MPNo = case adAccelerator ad of
                            Nothing -> return mbLast
                            Just str -> do
                                uiManagerAddUiFromString uiManager $
                                    "<accelerator name=\"" ++ name
                                        ++ "\" action=\"" ++ name  ++ "\"/>"
                                return mbLast
    | otherwise        = do
        menuItem <- mkMenuItem mbAction ad
        res <- getInsertion (castToMenuShell mb) menuPos
        case res of
            Nothing -> error ("Menu>>buildMenuItem: No valid position for: " ++ adName ad)
            Just (Prepend ms) -> do
                menuShellPrepend ms (castToMenuItem menuItem)
                return (Just (ms,0))
            Just (Append ms separated) -> do
                when separated $ do
                    sep <- separatorMenuItemNew
                    menuShellAppend ms sep
                menuShellAppend ms (castToMenuItem menuItem)
                idx <- getMenuIndexForItem ms (castToMenuItem menuItem)
                case idx of
                    Nothing -> return (Just (ms,0))
                    Just idx -> return (Just (ms,idx))
            Just (Insert idx ms separated) -> do
                idx' <-  if separated
                                then do
                                    sep <- separatorMenuItemNew
                                    menuShellInsert ms sep idx
                                    return (idx + 1)
                                else return idx
                menuShellInsert ms (castToMenuItem menuItem) idx'
                return (Just (ms,idx'))
            Just (AfterLast separated) -> do
                case mbLast of
                    Nothing -> error ("Menu>>buildMenuItem: No last insertion for: " ++ adName ad)
                    Just (ms,idx) -> do
                        idx' <-  if separated
                                then do
                                    sep <- separatorMenuItemNew
                                    menuShellInsert ms sep (idx + 1)
                                    return (idx + 1)
                                else return idx
                        menuShellInsert ms (castToMenuItem menuItem) (idx' + 1)
                        return (Just (ms,idx' + 1))

mkMenuItem :: ActionClass alpha => Maybe alpha -> ActionDescr -> IO MenuItem
mkMenuItem Nothing AD{adActionType = actionType, adLabel = label}
    | actionType == ActionSubmenu = do
        menuItem <- menuItemNewWithMnemonic label
        subMenu <- menuNew
        menuItemSetSubmenu menuItem subMenu
        return menuItem
    | otherwise = error "Menu>>mkMenuItem: Impossible"
mkMenuItem (Just action) AD{adActionType = actionType}
    | actionType == ActionNormal || actionType == ActionToggle  = do
        menuItem <- actionCreateMenuItem action
        return (castToMenuItem menuItem)

buildToolItem :: ActionClass alpha => UIManager -> Toolbar -> Maybe alpha -> ActionDescr
                    -> Maybe Int -> IO (Maybe Int)
buildToolItem uiManager tig Nothing ad@AD{adToolbar = toolPos, adName = name} mbLast
                     = return mbLast
buildToolItem uiManager tb (Just action) ad@AD{adToolbar = toolPos, adName = name} mbLast
    | toolPos == TPNo = return mbLast
    | otherwise      = do
        toolItem <- liftM castToToolItem (actionCreateToolItem action)
        res <- getToolInsertion tb toolPos
        case res of
            Nothing -> error ("Menu>>buildToolItem: No valid position for: " ++ adName ad)
            Just (InsertTool ind True) -> do
                        sep <- separatorToolItemNew
                        toolbarInsert tb sep ind
                        toolbarInsert tb toolItem (ind + 1)
                        return (Just (ind + 2))
            Just (InsertTool ind False) -> do
                        toolbarInsert tb toolItem ind
                        return (Just (ind + 1))
            Just (AfterLastTool True) -> do
                        case mbLast of
                            Nothing ->  error $ "Menu>>buildToolItem: No last insertion for: "
                                                    ++ adName ad
                            Just ind -> do
                                toolbarInsert tb toolItem ind
                                return (Just (ind + 1))
            Just (AfterLastTool False) -> do
                        case mbLast of
                            Nothing ->  error $ "Menu>>buildToolItem: No last insertion for: "
                                                    ++ adName ad
                            Just ind -> do
                                sep <- separatorToolItemNew
                                toolbarInsert tb sep ind
                                toolbarInsert tb toolItem (ind + 1)
                                return (Just (ind + 2))

data MenuPos = Prepend MenuShell  | Append MenuShell Bool  | Insert Int MenuShell Bool
                | AfterLast Bool

getInsertion :: MenuShell ->  MenuPosition -> IO (Maybe MenuPos)
getInsertion mb (MPFirst path)      = do
                                        res <- getMenuShellForPath path mb
                                        case res of
                                            Nothing -> return Nothing
                                            Just ms -> return (Just (Prepend ms))
getInsertion mb (MPLast path sep)   = do
                                        res <- getMenuShellForPath path mb
                                        case res of
                                            Nothing -> return Nothing
                                            Just ms -> return (Just (Append ms sep))
getInsertion mb (MPAfter [] sep)    = error "Menu>>getInsertion: Empty After path"
getInsertion mb (MPAfter (name:path) sep)
                                    = do
                                        res <- getMenuShellForPath path mb
                                        case res of
                                            Nothing -> return Nothing
                                            Just ms -> do
                                                mbIdx <- getMenuIndexForName ms name
                                                case mbIdx of
                                                    Nothing -> return Nothing
                                                    Just idx -> return
                                                        (Just (Insert (idx + 1)
                                                            ms sep))
getInsertion mb (MPBefore [])       = error "Menu>>getInsertion: Empty Before path"
getInsertion mb (MPBefore (n:p))    = do
                                        res <- getMenuShellForPath p mb
                                        case res of
                                            Nothing -> return Nothing
                                            Just ms -> do
                                                mbIdx <- getMenuIndexForName ms n
                                                case mbIdx of
                                                    Nothing -> return Nothing
                                                    Just idx -> return
                                                        (Just (Insert idx ms False))
getInsertion mb (MPAppend sep)      = return (Just (AfterLast sep))
getInsertion mb (MPOr mp1 mp2)      = do
                                        mbFirst <- getInsertion mb mp1
                                        case mbFirst of
                                            Just t -> return (Just t)
                                            Nothing -> getInsertion mb mp2

data ToolPos = InsertTool Int Bool | AfterLastTool Bool


getToolInsertion :: Toolbar ->  ToolPosition -> IO (Maybe ToolPos)
getToolInsertion tb TPFirst             = return $ Just (InsertTool 0 False)
getToolInsertion tb (TPLast sep)        = do
                                            n <- toolbarGetNItems tb
                                            return $ Just (InsertTool n sep)
getToolInsertion tb (TPAfter str sep)   = do
                                            mbIndex <- getToolIndexForName tb str
                                            case mbIndex of
                                                Just ind -> return $ Just (InsertTool (ind +1) sep)
                                                Nothing -> return $ Nothing
getToolInsertion tb (TPBefore str)      = do
                                            mbIndex <- getToolIndexForName tb str
                                            case mbIndex of
                                                Just ind -> return $ Just
                                                    (InsertTool ind False)
                                                Nothing -> return $ Nothing
getToolInsertion tb (TPAppend sep)      = return $ Just (AfterLastTool sep)
getToolInsertion tb (TPOr mp1 mp2)      = do
                                            mbFirst <- getToolInsertion tb mp1
                                            case mbFirst of
                                                Just t -> return (Just t)
                                                Nothing -> getToolInsertion tb mp2

getMenuShellForPath :: [String] -> MenuShell -> IO (Maybe MenuShell)
getMenuShellForPath [] menu        = return (Just menu)
getMenuShellForPath (hd:rest) menu = trace ("getMenuShellForPath " ++ show (hd:rest)) $ do
    widgets <- containerGetChildren menu
    res <- filterM (\ w -> do
                            mbLabel <- binGetChild (castToBin w)
                            case mbLabel of
                                Nothing -> return False
                                Just label -> do
                                        n <- labelGetText (castToLabel label)
                                        trace ("widget name" ++ n) $ return (n == hd)) widgets
    case res of
        [w]        -> do
            submenu <- menuItemGetSubmenu (castToMenuItem w)
            case submenu of
                Nothing -> error ("Menu>>getMenuShellForPath: Can't find submenu for " ++ hd)
                Just w  -> getMenuShellForPath rest (castToMenuShell w)
        otherwise  -> return Nothing

getMenuIndexForName :: MenuShell  -> String -> IO (Maybe Int)
getMenuIndexForName menu name = do
    widgets <- containerGetChildren menu
    res <- filterM (\ (w,_) -> do
                            mbLabel <- binGetChild (castToBin w)
                            case mbLabel of
                                Nothing -> return False
                                Just label -> do
                                        n <- labelGetText (castToLabel label)
                                        return (n == name))
                    (zip widgets [0..])
    case res of
        [(w,idx)]  -> return (Just idx)
        otherwise  -> return Nothing

getMenuIndexForItem :: MenuShell  -> MenuItem -> IO (Maybe Int)
getMenuIndexForItem menu item = do
    widgets <- containerGetChildren menu
    return (elemIndex item (map castToMenuItem widgets))

getToolIndexForName :: Toolbar -> String -> IO (Maybe Int)
getToolIndexForName tb name = do
    widgets <- containerGetChildren tb
    res <- filterM (\ (w,_) -> do
                            mbString <- get  (castToToolButton w) toolButtonLabel
                            case mbString of
                                Nothing -> return False
                                Just string -> do
                                        return (string == name))
                    (zip widgets [0..])
    case res of
        [(w,idx)]  -> return (Just idx)
        otherwise  -> return Nothing

--
-- | Setting sensivity
--
setSensitivity :: Selector s => [(s, Bool)] -> StateM ()
setSensitivity l = trace ("setSensitivity" ++ show l) $
    do mapM_ setSensitivitySingle l
       trace ("after setSensitivity" ++ show l) $ return ()
    where   setSensitivitySingle (sens,bool) = do
                actions <- getActionsFor sens
                liftIO $ mapM_ (\a -> actionSetSensitive a bool) actions

getActionsFor :: Selector s => s -> StateM [Action]
getActionsFor sens = do
    actMap <-  getActionState
    uiManager <- trace ("catMap: " ++ show actMap) getUiManagerSt
    case Map.lookup (GS sens) actMap of
        Nothing -> return []
        Just l -> do
            maybeList <- liftIO $ (mapM (getActionFor uiManager) l)
            return (catMaybes maybeList)
  where
    getActionFor uiManager string = do
        actionGroups <- uiManagerGetActionGroups uiManager
        actionGroupGetAction (head actionGroups) string


