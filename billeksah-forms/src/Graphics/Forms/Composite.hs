{-# Language ExistentialQuantification #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Forms.Composite
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
import Base
import Graphics.Pane (Direction(..))

import Control.Monad.IO.Class(liftIO)
import Graphics.UI.Gtk
import Control.Monad
import Data.IORef
import Data.Maybe
import Distribution.Simple
    (PackageName(..))
import Distribution.Text (simpleParse, display)
import Data.Version (Version(..))


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






