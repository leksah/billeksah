{-# Language EmptyDataDecls, DeriveDataTypeable #-}
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
    FieldDescriptionPP(..)
,   mkFieldPP
,   extractFieldDescription
,   flattenFieldDescriptionPP
,   flattenFieldDescriptionPPToS

,   formsPluginInterface
) where

import Graphics.Forms.Basics
import Graphics.Forms.Parameters
import Graphics.Forms.Build
import Graphics.Forms.GUIEvent
import Base.State
import Base.Event
import Base.PluginTypes
import Base.PrinterParser hiding (fieldParser)

import Graphics.UI.Gtk
import Control.Monad
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.ParserCombinators.Parsec as P
import Data.Version (Version(..))
import Data.Typeable (Typeable)
import Debug.Trace (trace)
--import IDE.Core.State

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
-- * Events the gui frame triggers
--

-- | Nothing interesting so far
data FormsEvent = FormsEvent
        deriving (Show, Typeable)

triggerFormsEvent :: FormsEvent -> StateM (FormsEvent)
triggerFormsEvent = triggerEvent FormsEventSel

getFormsEvent :: StateM (PEvent FormsEvent)
getFormsEvent = getEvent FormsEventSel

-- -----------------------------------------------
-- * Initialization
--

formsInit1 :: BaseEvent -> PEvent FormsEvent -> StateM ()
formsInit1 baseEvent myEvent = trace ("init1 " ++ pluginNameForms) $ do
    initialRegister
    return ()

formsInit2 :: BaseEvent -> PEvent FormsEvent -> StateM ()
formsInit2 baseEvent myEvent = trace ("init2 " ++ pluginNameForms) $ return ()


data FieldDescriptionPP alpha =  FDPP {
        fdParameters      ::  Parameters
    ,   fdFieldPrinter    ::  alpha -> PP.Doc
    ,   fdFieldParser     ::  alpha -> P.CharParser () alpha
    ,   fdFieldEditor     ::  alpha -> StateM (Widget, Injector alpha , alpha -> Extractor alpha , GEvent)
    ,   fdApplicator      ::  alpha -> alpha -> StateM ()}
    | VFDPP Parameters [FieldDescriptionPP alpha] -- ^ Vertical Box
    | HFDPP Parameters [FieldDescriptionPP alpha] -- ^ Horizontal Box
    | NFDPP [(String,FieldDescriptionPP alpha)]   -- ^ Notebook

type MkFieldDescriptionPP alpha beta =
    Parameters      ->
    (Printer beta)     ->
    (Parser beta)      ->
    (Getter alpha beta)    ->
    (Setter alpha beta)    ->
    (Editor beta)      ->
    (Applicator beta)  ->
    FieldDescriptionPP alpha

mkFieldPP :: Eq beta => MkFieldDescriptionPP alpha beta
mkFieldPP parameters printer parser getter setter editor applicator  =
    let FD _ ed = mkField parameters getter setter editor
    in FDPP parameters
        (\ dat -> (PP.text (case getParameterPrim paraName parameters of
                                    Nothing -> ""
                                    Just str -> str) PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case getParameterPrim paraSynopsis parameters of
                                    Nothing -> PP.empty
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> P.try (do
            symbol (case getParameterPrim paraName parameters of
                                    Nothing -> ""
                                    Just str -> str)
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

extractFieldDescription :: FieldDescriptionPP alpha  -> FieldDescription alpha
extractFieldDescription (VFDPP paras descrs) =  VFD paras (map extractFieldDescription descrs)
extractFieldDescription (HFDPP paras descrs) =  HFD paras (map extractFieldDescription descrs)
extractFieldDescription (NFDPP descrsp)      =  NFD (map (\(s,d) ->
                                                    (s, extractFieldDescription d)) descrsp)
extractFieldDescription (FDPP parameters fieldPrinter fieldParser fieldEditor applicator) =
    (FD parameters fieldEditor)

flattenFieldDescriptionPP :: FieldDescriptionPP alpha  -> [FieldDescriptionPP alpha]
flattenFieldDescriptionPP (VFDPP paras descrs)  =   concatMap flattenFieldDescriptionPP descrs
flattenFieldDescriptionPP (HFDPP paras descrs)  =   concatMap flattenFieldDescriptionPP descrs
flattenFieldDescriptionPP (NFDPP descrsp)       =   concatMap (flattenFieldDescriptionPP . snd) descrsp
flattenFieldDescriptionPP fdpp                  =   [fdpp]

flattenFieldDescriptionPPToS :: FieldDescriptionPP alpha -> [FieldDescriptionS alpha]
flattenFieldDescriptionPPToS = map ppToS . flattenFieldDescriptionPP


ppToS :: FieldDescriptionPP alpha -> FieldDescriptionS alpha
ppToS (FDPP para print pars _ _) = FDS (getParameter paraName para) print pars
                                       (Just (getParameter paraSynopsis para))
ppToS _                          = error "DescriptionPP.ppToS Can't transform"

