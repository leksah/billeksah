-- {-# Language  #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Statusbar
-- Copyright   :  Jürgen (jutaro) Nicklisch-Franken
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
--
-- | Statusbar services
--
-----------------------------------------------------------------------------

module Graphics.Statusbar (
    buildStatusbar,
    setStatusText
)where

import Base
import Graphics.FrameTypes

import Graphics.UI.Gtk
import Data.List (elemIndex, foldl')
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.Frame (getStatusbar, setStatusbar)
import qualified Data.Map as Map (lookup, fromList)

type CompState = [(CompName,CompWidget)]

--
-- | Building of a statusbar
--
buildStatusbar :: [CompDescr] -> StateM HBox
buildStatusbar descrs = do
    comWidgets <-  liftIO $ mapM buildWidget descrs
    let sortedWidgets = foldl' buildOrder [] (zip descrs comWidgets)
    hb <- liftIO $ do
        hb <- hBoxNew False 1
        widgetSetName hb "statusBox"
        mapM_ (\ (_,CompWText w) -> boxPackStart hb w PackGrow 0) sortedWidgets
        return hb
    setStatusbar (Map.fromList sortedWidgets, Just hb)
    return hb

buildWidget :: CompDescr -> IO CompWidget
buildWidget TextCompDescr{scName = name, scHasResizeGrip = rg, scRequestedSize = size} = do
    sb <- statusbarNew
    widgetSetName sb name
    statusbarSetHasResizeGrip sb rg
    widgetSetSizeRequest sb size (-1)
    return (CompWText sb)


buildOrder :: CompState -> (CompDescr,CompWidget) -> CompState
buildOrder accu (TextCompDescr{scName = name,scPosition = pos},w) =
    case pos of
        CPFirst -> (name,w) : accu
        CPLast  -> accu ++ [(name,w)]
        CPAfter str ->
            let index = elemIndex str (map fst accu)
            in case index of
                Just i ->  insertAt i (name,w) accu
                Nothing -> accu ++ [(name,w)]
        CPBefore str ->
            let index = elemIndex str (map fst accu)
            in case index of
                Just i | i > 0 ->  insertAt (i-1) (name,w) accu
                otherwise -> accu ++ [(name,w)]

--
-- | Setting text in a statusbar
--
setStatusText :: CompName -> String -> StateM ()
setStatusText compName string = do
    (theMap,_) <- getStatusbar
    case Map.lookup compName theMap of
        Just (CompWText sb) -> do
            liftIO $ statusbarPop sb 1
            liftIO $ statusbarPush sb 1 string
            return ()
        otherwise           -> return ()
