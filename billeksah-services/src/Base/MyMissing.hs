{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Base.MyMissing
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for missing base functions
--
------------------------------------------------------------------------------

module Base.MyMissing (
    allOf
,   forceJust
,   forceHead
,   splitString
,   replace
,   nonEmptyLines
,   trim
,   insertAt
,   myCast
,   maybeRead
) where

import Data.List (find,unfoldr)
import Data.Maybe (listToMaybe, isJust)
import Data.Char (isSpace)
import Data.Typeable (Typeable(..), Typeable)
import Unsafe.Coerce (unsafeCoerce)


-- | remove leading and trailing spaces
trim :: String -> String
trim      = f . f
   where f = reverse . dropWhile isSpace

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (\line -> isJust $ find (not . isSpace) line) . lines


allOf :: forall alpha. (Bounded alpha, Enum alpha) =>  [alpha]
allOf = map toEnum [fromEnum (minBound :: alpha) .. fromEnum (maxBound :: alpha)]

-- ---------------------------------------------------------------------
-- Convenience methods with error handling
--
forceJust :: Maybe alpha -> String -> alpha
forceJust mb str = case mb of
                        Nothing -> error str
                        Just it -> it

-- ---------------------------------------------------------------------
-- Convenience methods with error handling
--
forceHead :: [alpha] -> String -> alpha
forceHead (h:_) str = h
forceHead [] str = error str


-- ---------------------------------------------------------------------
-- Splitting a string into parts based on a token delimiter
--

splitString :: Eq a => a -> [a] -> [[a]]
splitString =  unfoldr . split'

split' :: Eq a => a -> [a] -> Maybe ([a], [a])
split' c l
  | null l = Nothing
  | otherwise = Just (h, drop 1 t)
  where (h, t) = span (/=c) l

-- ---------------------------------------------------------------------
-- Simple replacement
--

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace from to xs@(a:as) =
    if isPrefixOf from xs
        then to ++ replace from to (drop (length from) xs)
        else a : replace from to as
    where isPrefixOf as bs = and $ zipWith (== ) as bs

-- ---------------------------------------------------------------------
-- Insert in a list at index
--
insertAt :: Int -> a -> [a] -> [a]
insertAt i e l | i < 0        = e : l
               | i >= length l = l ++ [e]
               | otherwise    = let (start,end) = splitAt i l
                                in start ++ (e : end)

-- | The type-safe cast operation
myCast :: (Typeable a, Typeable b) => String -> a -> b
myCast errorString x = r
  where
    xt = typeOf x
    rt = typeOf r
    r  = if xt == rt
           then unsafeCoerce x
           else error $ errorString ++ ". Cast error inputType: " ++ show xt
                            ++ " outputType: " ++ show rt

-- | a read which may fail

maybeRead :: Read alpha => String -> Maybe alpha
maybeRead = listToMaybe . map fst . filter (null . snd) . reads


