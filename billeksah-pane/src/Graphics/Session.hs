{-# Language TypeSynonymInstances, ScopedTypeVariables, RankNTypes, TypeFamilies, NoMonomorphismRestriction,
    FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Session
-- Copyright   :  (c) Juergen Nicklisch-Franken
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Module for saving and recovering the layout
--
---------------------------------------------------------------------------------


module Graphics.Session (
    saveSession
,   recoverSession
,   asRegisterType
) where

import Base
import Graphics.Panes
import Graphics.Frame
import Graphics.FrameTypes
import Graphics.Menu

import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk
import qualified Data.Set as Set (toList)
import Control.Monad (forM_, when, forM, liftM)
import Data.Maybe (fromJust, isJust)
import Control.Exception (SomeException)
import qualified Data.Map as Map
       (toList, fromAscList, keys, toAscList, elems, empty)
import qualified Text.PrettyPrint as PP (text)
import Data.Time.Clock (getCurrentTime)
import Data.Time (getTimeZone, utcToLocalTime)
import Data.Typeable (Typeable, cast)

------------------------------------------------------------------------
-- * Interface

asRegisterType :: forall alpha. Pane alpha => alpha -> (String,GenPane)
asRegisterType p = (paneType p, PaneC p)

--
-- | Retrieves a string with all session information
--
saveSession :: StateM String
saveSession = do
        wdw             <-  getMainWindow
        layout          <-  mkLayout
        population      <-  getPopulation
        size            <-  liftIO $ windowGetSize wdw
        activePane'     <-  getActivePaneSt
        let activeP =   case activePane' of
                            Nothing -> Nothing
                            Just (s,_) -> Just s
        timeNow         <- liftIO getCurrentTime
        timeZone        <- liftIO $ getTimeZone timeNow
        extensions      <- getSessionExtensions
        tbv             <- toolbarVisible
        let state = SessionState {
            ssSaveTime            =   show $ utcToLocalTime timeZone timeNow
        ,   ssLayout              =   layout
        ,   ssPopulation          =   population
        ,   ssWindowSize          =   size
        ,   ssActivePane          =   activeP
        ,   ssToolbarVisible      =   tbv
        ,   ssExtensions          =   extensions}
        return(showFields state sessionDescr)

--
-- | Recovers a session from a previously saved string
--
recoverSession :: String -> StateM ()
recoverSession string = do
    detachedCloseAll
    paneCloseAll
    groupsCloseAll
    viewCollapseAll
    recoverSession' string
    return ()



-- ---------------------------------------------------------------------

data SessionState = SessionState {
        ssSaveTime            ::   String
    ,   ssLayout              ::   PaneLayout
    ,   ssPopulation          ::   [(String,Maybe String,PanePath)]
    ,   ssWindowSize          ::   (Int,Int)
    ,   ssActivePane          ::   Maybe String
    ,   ssToolbarVisible      ::   Bool
    ,   ssExtensions          ::   [(String,String)]
}


defaultSession = SessionState {
        ssSaveTime            =   ""
    ,   ssLayout              =   TerminalP Map.empty (Just TopP) (-1) Nothing Nothing
    ,   ssPopulation          =   []
    ,   ssWindowSize          =   (1024,768)
    ,   ssActivePane          =   Nothing
    ,   ssToolbarVisible      =   True
    ,   ssExtensions          =   []
}

sessionDescr :: [FieldDescriptionS SessionState]
sessionDescr = [
        mkFieldS
            "Time of storage"
            Nothing
            (PP.text . show)
            stringParser
            ssSaveTime
            (\ b a -> a{ssSaveTime = b})
    ,   mkFieldS
            "Layout"
            Nothing
            (PP.text . show)
            readParser
            ssLayout
            (\ b a -> a{ssLayout  = b})
    ,   mkFieldS
            "Population"
            Nothing
            (PP.text . show)
            readParser
            ssPopulation
            (\ b a -> a{ssPopulation = b})
    ,   mkFieldS
            "Window size"
            Nothing
            (PP.text . show)
            (pairParser intParser)
            ssWindowSize
            (\(c,d) a -> a{ssWindowSize = (c,d)})
    ,   mkFieldS
            "Maybe active pane"
            Nothing
            (PP.text . show)
            readParser
            ssActivePane
            (\ b a -> a{ssActivePane = b})
    ,   mkFieldS
            "Toolbar visible"
            Nothing
            (PP.text . show)
            readParser
            ssToolbarVisible
            (\ b a -> a{ssToolbarVisible = b})
    ,   mkFieldS
            "Extensions"
            Nothing
            (PP.text . show)
            (readParser)
            ssExtensions
            (\b a -> a{ssExtensions = b})]



detachedCloseAll :: StateM ()
detachedCloseAll = do
    windows <- getWindowsSt
    liftIO $ mapM_ widgetDestroy (tail windows)

paneCloseAll :: StateM ()
paneCloseAll = do
    panes' <- getPanesSt
    mapM_ (\ (PaneC p) -> closePane p) (Map.elems panes')

groupsCloseAll :: StateM ()
groupsCloseAll = do
    layout' <- getLayoutSt
    mapM_ closeGroup (Set.toList $ allGroupNames layout')

viewCollapseAll :: StateM ()
viewCollapseAll = do
    layout' <- getLayoutSt
    case layout' of
        TerminalP {}      -> return ()
        VerticalP _ _ _   -> viewCollapse' [SplitP LeftP]
        HorizontalP _ _ _ -> viewCollapse' [SplitP TopP]

mkLayout :: StateM(PaneLayout)
mkLayout = do
    rawLayout <- getLayoutSt
    getLayout' rawLayout []
    where
    getLayout' (HorizontalP l r _) pp = do
        l2          <-  getLayout' l (pp ++ [SplitP TopP])
        r2          <-  getLayout' r (pp ++ [SplitP BottomP])
        pane        <-  getPaned pp
        pos         <-  liftIO $ panedGetPosition pane
        return (HorizontalP l2 r2 pos)
    getLayout' (VerticalP l r _) pp = do
        l2          <-  getLayout' l (pp ++ [SplitP LeftP])
        r2          <-  getLayout' r (pp ++ [SplitP RightP])
        pane        <-  getPaned pp
        pos         <-  liftIO $ panedGetPosition pane
        return (VerticalP l2 r2 pos)
    getLayout' raw@(TerminalP {paneGroups = groups}) pp = do
        groups2     <-  forM (Map.toAscList groups) $ \(group, g) -> do
            l <- getLayout' g (pp ++ [GroupP group])
            return (group, l)
        nb          <-  getNotebook pp
        showTabs    <-  liftIO $ notebookGetShowTabs nb
        pos         <-  liftIO $ notebookGetTabPos nb
        current     <-  liftIO $ notebookGetCurrentPage nb
        size <- case detachedId raw of
            Just _  -> do
                Just parent <- liftIO $ widgetGetParent nb
                liftIO $ fmap Just $ windowGetSize (castToWindow parent)
            Nothing -> return $ detachedSize raw
        return raw {
                paneGroups   = Map.fromAscList groups2
            ,   paneTabs     = if showTabs then Just (posTypeToPaneDirection pos) else Nothing
            ,   currentPage  = current
            ,   detachedSize = size}

getPopulation :: StateM[(String,Maybe String,PanePath)]
getPopulation = do
    paneMap <- getPaneMapSt
    mapM (\ (pn,v) -> do
        (PaneC p) <- paneFromName pn
        mbSt <- saveState p
        let paneType' =  paneType p
        case mbSt of
            Nothing -> return (paneType p, Nothing, fst v)
            Just st -> return (paneType p, Just (show st), fst v))
                $ Map.toList paneMap

getSessionExtensions :: StateM [(String,String)]
getSessionExtensions = do
    ext <- getSessionExt
    mapM getSessionExtension ext

getSessionExtension :: GenSessionExtension -> StateM (String,String)
getSessionExtension (GenS SessionExtension{seName = name, seRetriever = retriever}) = do
    val <- retriever
    return (name,show val)


-- ------------------------------------------------------------
-- * Recovering
-- ------------------------------------------------------------

--
-- | Read and apply the saved layout
--

recoverSession' :: String -> StateM (Maybe String)
recoverSession' sessionString = catchState (do
    wdw         <-  getMainWindow
    let sessionSt = parseFields sessionString sessionDescr defaultSession
    liftIO $ windowSetDefaultSize wdw (fst (ssWindowSize sessionSt))(snd (ssWindowSize sessionSt))
    applyLayout (ssLayout sessionSt)
    populate (ssPopulation sessionSt)
    setCurrentPages (ssLayout sessionSt)
    when (isJust (ssActivePane sessionSt)) $ do
        mbPane <- mbPaneFromName (fromJust (ssActivePane sessionSt))
        case mbPane of
            Nothing -> return ()
            Just (PaneC p) -> makeActive p []
    showToolbar (ssToolbarVisible sessionSt)
    extensions      <- getSessionExt
    applyExtensions extensions (ssExtensions sessionSt)
    return Nothing)
    (\ (e :: SomeException) -> do
        return $ Just ("Session>>recoverSession: " ++ show e))

applyLayout :: PaneLayout -> StateM ()
applyLayout layoutS = do
    old <- getLayoutSt
    case old of
        TerminalP {} ->   applyLayout' layoutS []
        otherwise    ->   error "apply Layout can only be allied to empty Layout"
    where
    applyLayout' (TerminalP groups mbTabPos _ mbDetachedId mbDetachedSize) pp = do
        forM_ (Map.keys groups) $ \group -> viewNest' pp group
        nb          <-  getNotebook pp
        case (mbDetachedId, mbDetachedSize) of
            (Just id, Just (width, height)) -> do
                mbPair <- viewDetach' pp id
                case mbPair of
                    Nothing     -> return ()
                    Just (win,wid) -> do
                        liftIO $ widgetShowAll win
                        liftIO $ windowSetDefaultSize win width height
            _ -> return ()
        liftIO $notebookSetShowTabs nb (isJust mbTabPos)
        case mbTabPos of
            Just p -> liftIO $notebookSetTabPos nb (paneDirectionToPosType p)
            _      -> return ()
        forM_ (Map.toAscList groups) $ \(group, g) -> do
            applyLayout' g (pp ++ [GroupP group])
    applyLayout' (VerticalP l r pos) pp = do
        viewSplit' pp Vertical
        pane        <-  getPaned pp
        liftIO $panedSetPosition pane pos
        applyLayout' l (pp ++ [SplitP LeftP])
        applyLayout' r (pp ++ [SplitP RightP])
    applyLayout' (HorizontalP t b pos) pp = do
        viewSplit' pp Horizontal
        pane        <-  getPaned pp
        liftIO $panedSetPosition pane pos
        applyLayout' t (pp ++ [SplitP TopP])
        applyLayout' b (pp ++ [SplitP BottomP])

populate :: [(String, Maybe String, PanePath)] -> StateM ()
populate = mapM_ (\ (typeString,mbPs,pp) -> do
    paneTypes <- getPaneTypes
    case mbPs of
        Nothing -> return ()
        Just s ->  let mbTypeHint = case [pt | (ps,pt) <- paneTypes, ps == typeString] of
                            [th] -> Left th
                            [] -> Right ("Type not found: " ++ typeString)
                            l -> Right ("Type not unique: " ++ typeString)
                  in case mbTypeHint of
                    Right str -> message Error str
                    Left (PaneC gth) -> populate' gth s pp >> return ())

populate' :: forall alpha . Pane alpha =>  alpha  -> String -> PanePath -> StateM ()
populate' _ readString panePath = do
    let paneState :: (PaneState alpha) = read readString
    recoverState panePath paneState  >> return ()

setCurrentPages :: PaneLayout -> StateM ()
setCurrentPages layout = setCurrentPages' layout []
    where
    setCurrentPages' (HorizontalP t b _) p  =   do  setCurrentPages' t (SplitP TopP : p)
                                                    setCurrentPages' b (SplitP BottomP : p)
    setCurrentPages' (VerticalP l r _) p    =   do  setCurrentPages' l (SplitP LeftP : p)
                                                    setCurrentPages' r (SplitP RightP : p)
    setCurrentPages' (TerminalP groups _ ind _ _) p  =  do
                                                    forM_ (Map.toAscList groups) $ \(group, g) -> do
                                                        setCurrentPages' g (GroupP group : p)
                                                    when (ind >=  0) $ do
                                                        nb <- getNotebook (reverse p)
                                                        liftIO $ notebookSetCurrentPage nb ind

applyExtensions :: [GenSessionExtension] -> [(String,String)] -> StateM ()
applyExtensions gs = mapM_ (applyExtension gs)

applyExtension :: [GenSessionExtension] -> (String,String) -> StateM ()
applyExtension genList (name,readString) =
    case findExtension name genList of
        Nothing                                                    ->
            message Error ("Session>>applyExtension: Extension not found: " ++ name)
        Just (GenS (SessionExtension {seApplicator = applicator})) -> do
            let val = read readString
            applicator val

findExtension :: String -> [GenSessionExtension] -> Maybe GenSessionExtension
findExtension str [] = Nothing
findExtension str (g@(GenS (SessionExtension {seName = name})):r) | name == str = Just g
                                                                  | otherwise  = findExtension str r

