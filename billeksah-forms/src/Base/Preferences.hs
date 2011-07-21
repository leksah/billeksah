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

,   getPref
,   setPref
) where

import Base


--
-- | Load preferences from filepath.
-- Pref descriptions needs to be registered before
loadPrefs :: FilePath -> StateM ()
loadPrefs fp          =  undefined

--
-- | Save preferences to filepath.
-- Pref descriptions needs to be registered before.
savePrefs :: FilePath -> StateM ()
savePrefs fp          =  undefined

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
