{-# Language ExistentialQuantification, TypeFamilies, DeriveDataTypeable #-}

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
,   validatePrefs

,   getPrefs
,   setPrefs

) where

import Base

import Graphics.Forms.Parameters
import Graphics.Forms.Basics

import Data.List (sortBy, (\\), nub)
import Data.Typeable (Typeable(..), Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Graphics.Panes
       (Direction(..), PaneDirection(..), PanePathElement(..))
import Graphics.UI.Gtk.General.Enums (ShadowType(..))
import qualified Text.PrettyPrint as PP (text)
import Graphics.Forms.Composite
       (pairEditor, ColumnDescr(..), multisetEditor)
import Graphics.UI.Gtk (cellText)
import System.Glib.Attributes (AttrOp(..))
import Graphics.Forms.Simple (genericEditor, stringEditor)
import Debug.Trace (trace)

-------------------------------------
-- * The functions to handle preferences
--



--
-- | Checks uniqness of categories
--
validatePrefs :: [(String,GenFieldDescription)] -> Maybe String
validatePrefs prefsDescr =
    let categories = map (\(cat,_) -> cat) prefsDescr
        nCats = nub categories
    in if nCats /= categories
            then Just $ "duplicate categories:" ++ show (categories \\ nCats)
            else Nothing

--
-- | Load preferences from filepath.
-- Pref descriptions needs to be registered before
loadPrefs :: FilePath -> StateM ()
loadPrefs fp          =  trace "loadPrefs" $ do
    allPrefs <- getState PrefsDescrState
    let allPrefsS = map (\(s,GenF descr val) -> (s, GenFS (toFieldDescriptionS descr) val)) allPrefs
    Left loadedPrefs <- liftIO $ readFields fp (Left allPrefsS)
    let newPrefs =  map (exchangeValues loadedPrefs) allPrefs
    trace ("loadedPrefs " ++ show (length loadedPrefs)) $ setState PrefsDescrState newPrefs
    triggerFormsEvent PrefsChanged >> return ()
  where
    exchangeValues :: [(String, GenFieldDescriptionS)] -> (String, GenFieldDescription)
        -> (String, GenFieldDescription)
    exchangeValues loadedPrefs (s1, GenF fda a) =
        case [gen | (s2, gen) <- loadedPrefs, s1 == s2] of
            [GenFS _ newValue] ->   let nv = myCast "Preferences>>loadPrefs:" newValue
                            in (s1, GenF fda nv)
            _ -> (s1, GenF fda a)

--
-- | Save preferences to filepath.
-- Pref descriptions needs to be registered before.
savePrefs :: FilePath -> StateM ()
savePrefs fp          =  do
    allPrefs <- getState PrefsDescrState
    let allPrefsS = map (\(s,GenF fda a) -> (s, GenFS (toFieldDescriptionS fda) a)) allPrefs
    let string = showFields (Left allPrefsS)
    liftIO $ writeFile fp string
    triggerFormsEvent PrefsChanged >> return ()


--
-- | Gets a preference value from a category and a key
--
getPrefs :: Typeable alpha => String -> StateM alpha
getPrefs category = do
    allPrefs <- getState PrefsDescrState
    case [ genF | (s,genF) <- allPrefs, s == category] of
        [GenF _gd a] ->
            let a' = myCast ("Preferences>>getPrefs:" ++
                                "cast error for category: " ++ category) a
            in return a'
        _        -> error ("Preferences>>getPrefs: category not found: " ++ category)

--
-- | Sets a preference value for a category and a key
--
setPrefs :: (Eq alpha , Typeable alpha) => String -> alpha -> StateM ()
setPrefs category value = do
    allPrefs <- getState PrefsDescrState
    setState PrefsDescrState
        (map (\ old@(s,GenF des a) ->
            if s == category
                then if typeOf a == typeOf value
                    then (s,GenF des (myCast "setPrefs" value))
                    else  error ("Preferences>>setPrefs: type error for category: "
                                ++ category)
                else old)
            allPrefs)


-- -----------------------------------------------
-- * Convert to a form, which is usable for reading and writing to files
--


toFieldDescriptionS :: FieldDescription alpha -> [FieldDescriptionS alpha]
toFieldDescriptionS = map ppToS . flattenFieldDescription


ppToS :: FieldDescription alpha -> FieldDescriptionS alpha
ppToS (Field para print pars _ _) =
    FieldS (let ParaString str = getPara "Name" para in str) print pars
                                    (Just (let ParaString str = getPara "Synopsis" para in str))
ppToS _                           = error "DescriptionPP.ppToS Can't transform"

flattenFieldDescription :: FieldDescription alpha  -> [FieldDescription alpha]
flattenFieldDescription (VertBox _paras descrs)  =   concatMap flattenFieldDescription descrs
flattenFieldDescription (HoriBox _paras descrs)  =   concatMap flattenFieldDescription descrs
flattenFieldDescription (TabbedBox descrsp)     =   concatMap (flattenFieldDescription . snd) descrsp
flattenFieldDescription fdpp                  =   [fdpp]
