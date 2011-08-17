{-# Language ExistentialQuantification #-}

-----------------------------------------------------------------------------
--
-- Module      :  Base.Preferences
-- Copyright   :  (c) Juergen Nicklisch-Franken
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
--
-- | Module for handling preferences
--
-----------------------------------------------------------------------------------

module Base.Preferences (
    loadPrefs
,   savePrefs
,   editPrefs
,   validatePrefs

,   getPref
,   setPref
,   FieldDescription(..)
,   GenFieldDescription
) where

import Base
import Data.List ((\\), nub)
import Graphics.Forms.Parameters (Parameters)
import qualified Text.PrettyPrint as PP (Doc)
import qualified Text.ParserCombinators.Parsec as P (CharParser)
import Graphics.UI.Gtk (Widget)
import Graphics.Forms.Basics (GEvent, Extractor, Injector)

data FieldDescription alpha =  Field {
        fdParameters      ::  Parameters
    ,   fdFieldPrinter    ::  alpha -> PP.Doc
    ,   fdFieldParser     ::  alpha -> P.CharParser () alpha
    ,   fdFieldEditor     ::  alpha -> StateM (Widget, Injector alpha , alpha -> Extractor alpha , GEvent)
    ,   fdApplicator      ::  alpha -> alpha -> StateM ()}
    | VertBox Parameters [FieldDescription alpha] -- ^ Vertical Box
    | HoriBox Parameters [FieldDescription alpha] -- ^ Horizontal Box
    | TabbedBox [(String,FieldDescription alpha)]   -- ^ Notebook

data GenFieldDescription = forall alpha . GF (FieldDescription alpha)

--
-- | Checks uniqness of categories
--
validatePrefs :: [(String, [GenFieldDescription])] -> Maybe String
validatePrefs prefsDescr =
    let categories = map fst prefsDescr
        nCats = nub categories
    in if nCats /= categories
            then Just $ "duplicate categories:" ++ show (categories \\ nCats)
            else Nothing

--
-- | Save preferences to filepath.
-- Pref descriptions needs to be registered before.
savePrefs :: FilePath -> StateM ()
savePrefs fp          =  undefined

--
-- | Load preferences from filepath.
-- Pref descriptions needs to be registered before
loadPrefs :: FilePath -> StateM ()
loadPrefs fp          =  undefined


--
-- | Save preferences to filepath.
-- Pref descriptions needs to be registered before
editPrefs             ::  StateM ()
editPrefs             =  undefined

--
-- | Gets a preference value from a category and a key
--
getPref :: String -> String -> StateM alpha
getPref category key = undefined

--
-- | Sets a preference value for a category and a key
--
setPref :: String -> String -> alpha -> StateM ()
setPref category key value = undefined
