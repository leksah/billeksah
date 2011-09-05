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
,   editPrefs
,   validatePrefs

,   getPrefs
,   setPrefs

) where

import Base

import Graphics.Forms.Parameters
import Graphics.Forms.Basics
import Graphics.Forms.Build (GenFieldDescriptionG(..),toFieldDescriptionG)

import Data.List ((\\), nub)
import qualified Text.PrettyPrint as PP (Doc)
import qualified Text.ParserCombinators.Parsec as P (CharParser)
import Graphics.UI.Gtk (Widget)
import Data.Typeable (cast, Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.Panes.Preferences (openPreferencesPane)

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
loadPrefs fp          =  do
    allPrefs <- getState PrefsDescrState
    let allPrefsS = map (\(s,GenF fda a) -> (s, GenFS (toFieldDescriptionS fda) a)) allPrefs
    Left loadedPrefs <- liftIO $ readFields fp (Left allPrefsS)
    let newPrefs = map (exchangeValues loadedPrefs) allPrefs
    setState PrefsDescrState newPrefs
    triggerFormsEvent PrefsChanged >> return ()
  where
    exchangeValues loadedPrefs (s1, GenF fda a) =
        case [gfs | (s2,gfs) <- loadedPrefs, s1 == s2] of
            [GenFS _ newValue] -> case cast newValue of
                                    Nothing -> error "Preferences>>loadPrefs: castError"
                                    Just nv -> (s1, GenF fda nv)
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

--
-- | Visaully edit preferences.
--
editPrefs             ::  StateM ()
editPrefs             =  openPreferencesPane



--
-- | Gets a preference value from a category and a key
--
getPrefs :: Typeable alpha => String -> StateM alpha
getPrefs category = do
    allPrefs <- getState PrefsDescrState
    case [ gd | (s,gd) <- allPrefs, s == category] of
        [GenF _ a] -> case cast a of
                            Just a' -> return a'
                            Nothing -> error
                                ("Preferences>>getPrefs: cast error for category: "
                                ++ category)
        otherwise -> error ("Preferences>>getPrefs: category not found: " ++ category)


--
-- | Sets a preference value for a category and a key
--
setPrefs :: Typeable alpha => String -> alpha -> StateM ()
setPrefs category value = do
    allPrefs <- getState PrefsDescrState
    setState PrefsDescrState
        (map (\ old@(s,(GenF des a)) ->
            if s == category
                then case cast value of
                    Just v -> (s,GenF des v)
                    _ -> error ("Preferences>>setPrefs: cast error for category: "
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
flattenFieldDescription (VertBox paras descrs)  =   concatMap flattenFieldDescription descrs
flattenFieldDescription (HoriBox paras descrs)  =   concatMap flattenFieldDescription descrs
flattenFieldDescription (TabbedBox descrsp)     =   concatMap (flattenFieldDescription . snd) descrsp
flattenFieldDescription fdpp                  =   [fdpp]
