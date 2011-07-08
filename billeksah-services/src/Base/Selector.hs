{-# Language StandaloneDeriving, DeriveDataTypeable, ExistentialQuantification #-}

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
    Selector,
    GenSelector(..),

) where

import Data.Typeable (typeRepKey, cast, Typeable(..), Typeable)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

----
---- | A type family for unique selectors
----
class (Eq s, Ord s, Show s, Typeable s) => Selector s

--
-- | Boxing for selectors
--
data GenSelector = forall s . (Selector s) =>  GS s
    deriving (Typeable)

deriving instance Show GenSelector

--
-- | Equality and comparision for selectors
--
instance Eq GenSelector where
    (==) (GS a) (GS b) = if typeOf a == typeOf b then
                    fromJust (cast a) == b
                    else False

instance Ord GenSelector where
    compare (GS a) (GS b) = if typeOf a == typeOf b then
                    compare (fromJust (cast a)) b
                    else compare (unsafePerformIO $ typeRepKey $ typeOf a)
                                 (unsafePerformIO $ typeRepKey $ typeOf b)
