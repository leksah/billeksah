{-# Language EmptyDataDecls, DeriveDataTypeable, ExistentialQuantification,
    StandaloneDeriving, TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.DescriptionPP
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Description of a editor with additional fileds for printing and parsing
--
-----------------------------------------------------------------------------------
module Graphics.Forms.Description (
    mkField
,   formsPluginInterface
,   initPrefs
) where

import Graphics.Forms.Basics
import Graphics.Forms.Parameters
import Graphics.Forms.Build
import Graphics.Forms.GUIEvent
import Graphics.Pane
import Base

import Graphics.UI.Gtk
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.ParserCombinators.Parsec as P
import Data.Version (Version(..))
import qualified Data.Map as Map (empty)
import Base.Preferences (loadPrefs, validatePrefs)
import Graphics.Forms.Composite
       (pairEditor, ColumnDescr(..), multisetEditor)
import Graphics.Forms.Simple (genericEditor, stringEditor)
import Data.List (sortBy)
import Graphics.Panes.Preferences
       (PreferencesPane, openPreferencesPane)
import Control.Monad (liftM, when)
import System.FilePath ((</>), dropFileName)
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (MonadIO(..))

-- ----------------------------------------------
-- * It's a plugin
--

formsPluginInterface :: StateM (PluginInterface FormsEvent)
formsPluginInterface = do
    fe <- makeEvent FormsEventSel
    return $ PluginInterface {
         piInit1   = formsInit1,
         piInit2   = formsInit2,
         piEvent   = fe,
         piName    = pluginNameForms,
         piVersion = Version [1,0,0][]}

-- -----------------------------------------------
-- * Initialization
--

formsInit1 :: BaseEvent -> PEvent FormsEvent -> StateM ()
formsInit1 _baseEvent _myEvent = do
    message Debug ("init1 " ++ pluginNameForms)
    initialRegister
    return ()

formsInit2 :: BaseEvent -> PEvent FormsEvent -> StateM ()
formsInit2 _baseEvent _myEvent = do
    message Debug ("init2 " ++ pluginNameForms)
    registerFrameEvent handler >> return ()
  where handler (RegisterActions actions) = return $ RegisterActions $ actions ++ myActions
        handler (RegisterPane paneTypes)  = return $ RegisterPane $ paneTypes ++ myPaneTypes
        handler e                         = return e

myActions :: [ActionDescr]
myActions =
    [AD "Configuration" "_Configuration" Nothing Nothing (return ()) Nothing ActionSubmenu
            (Just $ MPAfter ["View"] False) Nothing [],
     AD "EditPrefs" "EditPrefs" Nothing Nothing openPreferencesPane Nothing ActionNormal
        (Just $ MPLast ["Configuration"] False) Nothing []]

myPaneTypes :: [(String,GenPane)]
myPaneTypes =
    [asRegisterType (undefined :: PreferencesPane)]

defaultPrefsName :: String
defaultPrefsName = "Default.prefs"

initialRegister :: StateM (Maybe String)
initialRegister = do
    registerState GuiHandlerStateSel (Handlers Map.empty)
    registerState GtkEventsStateSel (GtkRegMap Map.empty)
    registerState PrefsDescrState []
    currentConfigPath <- liftM dropFileName getCurrentConfigPath
    registerCurrentPrefsPath (currentConfigPath </> defaultPrefsName)

type MkFieldDescription alpha beta =
    Parameters      ->
    (Printer beta)     ->
    (Parser beta)      ->
    (Getter alpha beta)    ->
    (Setter alpha beta)    ->
    (Editor beta)      ->
    (Applicator beta)  ->
    FieldDescription alpha

mkField :: Eq beta => MkFieldDescription alpha beta
mkField parameters printer parser getter setter editor applicator  =
    let FieldG _ ed = mkFieldG (getParaS "Name" parameters) parameters getter setter editor
    in Field parameters
        (\ dat -> (PP.text (getParaS "Name" parameters) PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case getPara "Synopsis" parameters of
                                    ParaString "" -> PP.empty
                                    ParaString str -> PP.text $"--" ++ str
                                    _ -> error "Description>>mkField: impossible")))
        (\ dat -> P.try (do
            symbol (let ParaString str = getPara "Name" parameters in str)
            colon
            val <- parser
            return (setter val dat)))
        ed
        (\ newDat oldDat -> do --applicator
            let newField = getter newDat
            let oldField = getter oldDat
            if newField == oldField
                then return ()
                else applicator newField)

-- ----------------------------------------------
-- * Editing preferences
--

initPrefs :: StateM ()
initPrefs  = do
    prefsPath <- getCurrentPrefsPath
    RegisterPrefs allPrefs <- triggerFormsEvent
        (RegisterPrefs [("Frame",GenF panesPrefs defaultPanePrefs)])
    liftIO $ putStrLn ("Categories: " ++ show (map fst allPrefs))
    case validatePrefs allPrefs of
        Nothing -> return ()
        Just str -> error $ "Description>>formsInit2::"++ str
    setState PrefsDescrState allPrefs
    hasPrefsFile <- liftIO $ doesFileExist prefsPath
    when hasPrefsFile $
        loadPrefs prefsPath

defaultPanePrefs =  PanePrefs [] [] [SplitP LeftP]

panesPrefs :: FieldDescription PanePrefs
panesPrefs =
    VertBox defaultParams
        [mkField
            (("Name",ParaString "Categories for panes") <<<
             ("Shadow",ParaShadow ShadowIn) <<<
             ("Direction",ParaDir Vertical) <<<
             ("MinSize",ParaSize (-1,130)) <<<
             defaultParams)
            (PP.text . show)
            readParser
            (\ a -> ppCategoryForPane a)
            (\ b a -> a{ppCategoryForPane = b})
            (multisetEditor
                (ColumnDescr True [("Pane Id",\(n,_) -> [cellText := n], Nothing)
                                   ,("Pane Category",\(_,v) -> [cellText := v],
                                        Nothing)])
                ((pairEditor
                    (stringEditor (\s -> not (null s)) True,defaultParams)
                    (stringEditor (\s -> not (null s)) True,defaultParams)),defaultParams)
                (Just (sortBy (\(a,_) (a2,_) -> compare a a2)))
                (Just (\(a,_) (a2,_) -> a == a2)))
            (\_i -> return ())
    ,   mkField
            (("Name", ParaString "Pane path for category") <<<
            ("Shadow", ParaShadow ShadowIn) <<<
            ("Direction", ParaDir Vertical) <<<
            ("MinSize", ParaSize (-1,130)) <<<
                defaultParams)
            (PP.text . show)
            readParser
            ppPathForCategory
            (\b a -> a{ppPathForCategory = b})
            (multisetEditor
                (ColumnDescr True [("Pane category",\(n,_) -> [cellText := n],
                                    Nothing)
                                   ,("Pane path",\(_,v) -> [cellText := show v],
                                     Nothing)])
                ((pairEditor
                    (stringEditor (\s -> not (null s)) True,defaultParams)
                    (genericEditor,defaultParams)),defaultParams)
                (Just (sortBy (\(a,_) (a2,_) -> compare a a2)))
                (Just (\(a,_) (a2,_) -> a == a2)))
            (\_i -> return ())
    ,   mkField
            (("Name", ParaString "Default pane path") <<< defaultParams)
            (PP.text . show)
            readParser
            ppDefaultPath
            (\b a -> a{ppDefaultPath = b})
            genericEditor
            (\_i -> return ())]

