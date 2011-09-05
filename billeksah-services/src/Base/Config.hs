-----------------------------------------------------------------------------
--
-- Module      :  Base.Config
-- Copyright   :  Juergen "jutaro" Nicklisch-Franken
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :
-- Portability :
--
-- | Plugin Configurations
--
-----------------------------------------------------------------------------

module Base.Config (
    loadPluginDescr,
    loadPluginDescription,
    writePluginDescr,
    loadPluginConfig,
    writePluginConfig,

    loadListFromConfig,

    allKnownPlugins,
    getPrereqChoices,

    defaultConfig,
    defaultPlugin

) where

import Base.PluginTypes
import Base.ConfigFile
import Base.Event
import Base.State

import qualified Text.PrettyPrint as PP (text)
import Control.Monad (foldM, filterM, unless)
import Data.Version (showVersion, parseVersion, Version(..))
import Data.List (nubBy, intersperse, isPrefixOf)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath
       (dropFileName, (</>), dropExtension, takeExtension, takeFileName)
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
       (elems, toList, lookup, member, insert, empty)
import Data.Map (Map)
import Base.Graph (topSortGraph)
import Base.State (StateM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Typeable (cast)

-- | Plugin file (.lksp)

defaultPlugin = Plugin {
    plName          = "",
    plVersion       = Version[][],
    plModule        = "",
    plInterface     = "",
    plPrerequisites = [],
    plChoices       = [],
    plSynopsis      = ""}

pluginDescr :: [FieldDescriptionS Plugin]
pluginDescr = [
        mkFieldS
            "Name of the plugin"
            Nothing
            stringPrinter
            stringParser
            plName
            (\ b a -> a{plName = b})
    ,   mkFieldS
            "Version"
            Nothing
            (stringPrinter . showVersion)
            versionParser
            plVersion
            (\ b a -> a{plVersion = b})
    ,   mkFieldS
            "Module"
            Nothing
            stringPrinter
            stringParser
            plModule
            (\ b a -> a{plModule = b})
    ,   mkFieldS
            "Interface"
            Nothing
            stringPrinter
            stringParser
            plInterface
            (\ b a -> a{plInterface = b})
    ,   mkFieldS
            "Prerequisite list"
            Nothing
            (stringPrinter . showPluginList)
            parsePluginList
            plPrerequisites
            (\ b a -> a{plPrerequisites = b})
    ,   mkFieldS
            "Synopsis"
            (Just "or call it comment")
            stringPrinter
            stringParser
            plSynopsis
            (\ b a -> a{plSynopsis = b})
           ]

loadPluginDescr :: FilePath -> IO Plugin
loadPluginDescr fn = readFieldsSimple fn pluginDescr defaultPlugin

writePluginDescr :: FilePath -> Plugin -> IO ()
writePluginDescr fpath descr = do
    exists <- doesFileExist fpath
    if exists
        then do
            descrOld <- loadPluginDescr fpath
            unless (descrOld == descr) $
                writeFile fpath (showFieldsSimple descr pluginDescr)
        else writeFile fpath (showFieldsSimple descr pluginDescr)

-- | Plugin configfile (.lkshc)

defaultConfig = PluginConfig {
    cfName          = "Unnamed zombie",
    cfVersion       = Version[0][],
    cfPlugins       = [],
    cfChoices       = [],
    cfSynopsis      = ""}


pluginConf :: [FieldDescriptionS PluginConfig]
pluginConf = [
        mkFieldS
            "Name of the config"
            Nothing
            stringPrinter
            stringParser
            cfName
            (\ b a -> a{cfName = b})
    ,   mkFieldS
            "Version"
            Nothing
            (stringPrinter . showVersion)
            versionParser
            cfVersion
            (\ b a -> a{cfVersion = b})
    ,   mkFieldS
            "Plugin list"
            Nothing
            (stringPrinter . showPluginList)
            parsePluginList
            cfPlugins
            (\ b a -> a{cfPlugins = b})
    ,   mkFieldS
            "Synopsis"
            (Just "or call it comment")
            stringPrinter
            stringParser
            cfSynopsis
            (\ b a -> a{cfSynopsis = b})
           ]

loadPluginConfig  :: FilePath -> IO PluginConfig
loadPluginConfig fn = readFieldsSimple fn pluginConf defaultConfig

writePluginConfig :: FilePath -> PluginConfig -> IO ()
writePluginConfig fpath config = do
    configOld <- loadPluginConfig fpath
    unless (configOld == config) $
        writeFile fpath (showFieldsSimple config pluginConf)

type Error = String

loadPluginDescription :: FilePath -> (PluginName, VersionBounds) -> IO (Either Plugin Error)
loadPluginDescription fp (name,bounds) = do
    versions <- getPluginVersions fp name
    case selectOptimalVersion versions bounds of
        Nothing -> return (Right ("Can't select version for plugin: " ++ name))
        Just v  -> getPluginDescr fp name v >>= \ p -> return (Left p)

-- | Returns the description of a plugin
getPluginDescr :: FilePath -> String -> Version -> IO Plugin
getPluginDescr fp name version =  loadPluginDescr
    (fp </> name ++ "-" ++ showVersion version ++ ".lkshp")

-- | Take the latest allowed
selectOptimalVersion :: [Version] -> VersionBounds -> Maybe Version
selectOptimalVersion [] _                       = Nothing
selectOptimalVersion versions (Nothing,Nothing) = Just $ maximum versions
selectOptimalVersion versions (Just vl,Nothing) = case [ v | v <- versions, v >= vl] of
                                                    [] -> Nothing
                                                    l  -> Just $ maximum l
selectOptimalVersion versions (Nothing,Just vu) = case [ v | v <- versions, v <= vu] of
                                                    [] -> Nothing
                                                    l  -> Just $ maximum l
selectOptimalVersion versions (Just vl,Just vu) = case [ v | v <- versions, v >= vl && v <= vu] of
                                                    [] -> Nothing
                                                    l  -> Just $ maximum l

-- | Returns a list of versions
getPluginVersions :: FilePath -> String -> IO [Version]
getPluginVersions fp name = catch (do
    filesAndDirs <- getDirectoryContents fp
    files        <- filterM (\f -> doesFileExist (fp </> f)) filesAndDirs
    let relevantFiles =  [ nameWithVersion | nameWithVersion <- files,
                                             isPrefixOf name nameWithVersion,
                                             takeExtension nameWithVersion == ".lkshp"]

    return (mapMaybe (\ f ->
        case filter (\r -> snd r == "") ((readP_to_S parseVersion) (extractVersion f)) of
                        (a,_):_  -> Just a
                        []       -> Nothing) relevantFiles))
    (\e -> do
--        putStrLn "Config>>getPluginVersions: ++ show e"
        return [])
  where
    extractVersion string = drop (length name + 1) (dropExtension string)

-- | First collect all Plugins, then make them a graph and topsort it
loadListFromConfig :: BaseEvent -> FilePath -> PluginConfig -> StateM [Plugin]
loadListFromConfig baseEvent fp PluginConfig{cfPlugins = prerequ} = do
    collectedPlugins <- foldM loadDeep Map.empty prerequ
    let pluginGraph = foldr (buildGraph collectedPlugins) Map.empty (Map.elems collectedPlugins)
    return (reverse (topSortGraph pluginGraph))
  where
    loadDeep :: Map String Plugin ->  Prerequisite -> StateM (Map String Plugin)
    loadDeep map prerequ@(name,version) | Map.member name map = return map
                                  | otherwise = do
        res <- liftIO $ loadPluginDescription fp prerequ
        case res of
            Left plugin -> do
                let map2 = Map.insert name plugin map
                foldM loadDeep map2 (plPrerequisites plugin)
            Right error -> do
                message Error ("Can't load plugin descr " ++ show prerequ ++ " " ++ error)
                return map
    buildGraph allPlugs plug graph =
        Map.insert plug
            (mapMaybe (\ (name,_) -> Map.lookup name allPlugs) (plPrerequisites plug)) graph

allKnownPlugins :: FilePath -> IO [Plugin]
allKnownPlugins fp = do
    filesAndDirs <- getDirectoryContents fp
    files        <- filterM (\f -> doesFileExist (fp </> f)) filesAndDirs
    let relevantFiles =  [ fp | fp <- files, takeExtension fp == ".lkshp"]
    mapM (\ fp -> loadPluginDescr fp) relevantFiles

getPrereqChoices :: FilePath -> IO [Prerequisite]
getPrereqChoices currentConfigPath= do
    possiblePlugins     <-  allKnownPlugins (dropFileName currentConfigPath)
    let possibleBounds  =   map getStandardBounds possiblePlugins
    let possibleChoices =   nubBy (\e1 e2 -> fst e1 == fst e2) $
                                [ e | e@(n,_) <- possibleBounds]
    return possibleChoices

getStandardBounds :: Plugin -> Prerequisite
getStandardBounds Plugin{plName = name, plVersion = version} =
    (name,(Just version,Just (nextVersion version)))
  where
    nextVersion (Version (a:b:_) []) = Version [a,b+1] []
    nextVersion (Version [a] [])     = Version [a,1] []
    nextVersion (Version [] [])      = Version [1,1] []
