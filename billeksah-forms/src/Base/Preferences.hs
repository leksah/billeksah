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
savePrefs fp          =  do
    allPrefs <- getState PrefsDescrState
    let allPrefsS = map (\(s,GenF fda a) -> (s, GenFS (toFieldDescriptionS fda) a)) allPrefs
    let string = showFields (Left allPrefsS)
    liftIO $ writeFile fp string
    triggerFormsEvent PrefsChanged >> return ()

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
