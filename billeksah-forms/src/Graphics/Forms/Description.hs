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
) where

import Graphics.Forms.Basics
import Graphics.Forms.Parameters
import Graphics.Forms.Build
import Graphics.Forms.GUIEvent
import Base

import Graphics.UI.Gtk
import Control.Monad
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.ParserCombinators.Parsec as P
import Data.Version (Version(..))
import Data.Typeable (Typeable)
import qualified Data.Map as Map (empty)
import Base.Preferences (validatePrefs)

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
formsInit1 baseEvent myEvent = do
    message Debug ("init1 " ++ pluginNameForms)
    initialRegister
    return ()

framePrefs = []

formsInit2 :: BaseEvent -> PEvent FormsEvent -> StateM ()
formsInit2 baseEvent myEvent = do
    message Debug ("init2 " ++ pluginNameForms)
    RegisterPrefs allPrefs <- triggerFormsEvent (RegisterPrefs framePrefs)
    case validatePrefs allPrefs of
        Nothing -> return ()
        Just str -> error $ "Description>>formsInit2::"++ str
    setState PrefsDescrState allPrefs
    return ()

initialRegister = do
    registerState GuiHandlerStateSel (Handlers Map.empty)
    registerState GtkEventsStateSel (GtkRegMap Map.empty)
    registerState PrefsDescrState framePrefs

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
                                    ParaString str -> PP.text $"--" ++ str)))
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


