{-# Language StandaloneDeriving, DeriveDataTypeable, ExistentialQuantification,
    TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Module      :  Base.Selector
-- Copyright   :  Juergen "jutaro" Nicklisch-Franken
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Base.Selector (
    Selector(..),
    GenSelector(..),

) where

import Data.Typeable (typeRepKey, cast, Typeable(..), Typeable)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

----
---- | A type family for unique selectors
----
class (Eq alpha, Ord alpha, Show alpha, Typeable alpha) => Selector alpha where
    type ValueType alpha :: *

--
-- | Boxing for selectors
--
data GenSelector = forall alpha . Selector alpha =>  GS alpha

deriving instance Show GenSelector


-- | Equality and comparision for selectors

instance Eq GenSelector where
    (==) (GS a) (GS b) = if typeOf a == typeOf b then
                    fromJust (cast a) == b
                    else False

instance Ord GenSelector where
    compare (GS a) (GS b) = if typeOf a == typeOf b then
                    compare (fromJust (cast a)) b
                    else compare (unsafePerformIO $ typeRepKey $ typeOf a)
                                 (unsafePerformIO $ typeRepKey $ typeOf b)
