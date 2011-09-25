{-# Language ExistentialQuantification, TypeFamilies, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Forms.Parameters
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Module for parameters for editors
--  Setting of parameters: ("Name", ParaString "Hello") `setPara` defaultParameters
-- Getting parameters getPara "Name" paras
--
-----------------------------------------------------------------------------

module Graphics.Forms.Parameters (
    defaultParams,
    (<<<),
    getPara,
    getParaS,
    ParaType(..),
    Parameters
) where

import Base
import Graphics.Pane

import Graphics.UI.Gtk (Packing, ShadowType)
import Graphics.UI.Gtk.General.Enums (Packing(..), ShadowType(..))
import Data.Typeable (Typeable)

-- ParaName "Hallo" <<< defaultParams
--          selector

-- setPara :: Selector alpha => alpha => (ValueType alpha) => Parameters -> Parameters

type Parameters = [Para ParaType]

-- type Parameters = forall alpha . (Selector alpha) [(alpha, ValueType alpha)]



-- | A generalised attribute with independent get and set types.
data Para alpha = Para
    {paName   :: String,
     paValue  :: alpha}
     deriving Show

infixr 9 <<<

(<<<) :: (String, ParaType) -> Parameters -> Parameters
(<<<) (name,newValue)  paras =
    case [ para | para <- paras, paName para == name] of
        [p] -> p{paValue = newValue} : [ para | para <- paras, paName para /=  name]
        _   -> error $ "Parameters >> Cant find parameter " ++ name

getPara :: String -> Parameters -> ParaType
getPara name paras =
    case [ para | para <- paras, paName para == name] of
        [p] -> paValue p
        _   -> error $ "Parameters >> Cant find parameter " ++ name

getParaS :: String -> Parameters -> String
getParaS name paras = case getPara name paras of
                    ParaString s -> s
                    _ -> error "Parameters>>Not a string"

data StringSel = Name | StockId | Synopsis
    deriving (Eq, Ord, Show, Typeable)

instance Selector StringSel where
    type ValueType StringSel = String


data ParaType  =
    ParaString String
    | ParaAlign (Float,Float,Float,Float)
    | ParaPadding (Int,Int,Int,Int)
    | ParaSize (Int,Int)
    | ParaPos (Float,Float)
    | ParaBool Bool
    | ParaHori HorizontalAlign
    | ParaDir Direction
    | ParaShadow ShadowType
    | ParaPack Packing
    deriving(Eq,Show)

data HorizontalAlign =   StartHorizontal | StopHorizontal | Keep
    deriving (Eq,Show)

--data StringPara = ParaName | ParaStockId | ParaSynopsis
--    deriving (Eq,Ord,Show,Typeable)
--
--instance Selector StringPara where
--    type ValueType = String
--


defaultParams :: Parameters
defaultParams =
    [   Para "Name"            (ParaString "")
    ,   Para "StockId"         (ParaString "")
    ,   Para "Synopsis"        (ParaString "")
-- For boxes
    ,   Para "VBoxHomogeneous" (ParaBool False)
    ,   Para "HBoxHomogeneous" (ParaBool True)
    ,   Para "VPack"           (ParaPack PackNatural)
    ,   Para "HPack"           (ParaPack PackGrow)
-- For fields
    ,   Para "OuterAlignment"  (ParaAlign (0.0, 0.0, 1, 1))
    ,   Para "OuterPadding"    (ParaPadding (5, 5, 5, 5))
-- the label
    ,   Para "ShowLabel"       (ParaBool True)
    ,   Para "LabelAlign"      (ParaPos (0.5, 0.0))
    ,   Para "Shadow"          (ParaShadow ShadowNone)
-- the inner alignment
    ,   Para "InnerAlignment"  (ParaAlign (0.0, 0.0, 1, 1))
    ,   Para "InnerPadding"    (ParaPadding (5, 5, 5, 5))

-- for the field
    ,   Para "MinSize"         (ParaSize (-1,-1))

-- for special widgets
    ,   Para "Direction"       (ParaDir Horizontal)
    ,   Para "MultiSel"        (ParaBool True)


--    ,   Para "Horizontal"      (ParaHori Keep)
    ]


