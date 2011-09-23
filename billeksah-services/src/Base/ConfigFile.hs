{-# OPTIONS_GHC -XTypeSynonymInstances -XExistentialQuantification #-}
-----------------------------------------------------------------------------
--
-- Module      :  Base.ConfigFile
-- Copyright   :  (c) Juergen Nicklisch-Franken
-- License     :  GNU-GPL
-- Maintainer  :  <maintainer at leksah.org>
--
-- | Module for saving and restoring preferences and settings
-- ina way similiar to what you would find in Microsoft Windows INI files.
--
-- The configuration file may consists of sections,
-- led by a [section] header and followed by name: value entries
-- or it only contains name: value entries.

-- Lines beginning with '#' or '--' (after optional whitespace)
-- are ignored and may be used to provide comments.

-- Newlines may be added between a value, but the next line must begin with a whitespace
-- (names have to start at column 0, without preceeding whitespace).

module Base.ConfigFile (

    Printer
,   Parser
,   FieldDescriptionS(..)
,   GenFieldDescriptionS(..)
,   mkFieldS

,   applyFieldParsers
,   boolParser
,   intParser
,   pairParser
,   identifier
,   emptyParser
,   whiteSpace
,   stringParser
,   readParser
,   versionParser
,   parsePluginList
,   showPluginList
,   boundParser

,   emptyPrinter
,   stringPrinter
,   maybePP

,   symbol
,   colon

,   showFields
,   readFields
,   parseFields

,   readFieldsSimple
,   parseFieldsSimple
,   showFieldsSimple
) where

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.PrettyPrint as PP

import Data.Maybe (catMaybes, listToMaybe)
import Data.List (intersperse, foldl')
import Data.Version (showVersion, Version(..))
import Base.PluginTypes (VersionBounds, PluginName, Prerequisite)
import Control.Monad (liftM)
import Base.MyMissing (maybeRead, myCast, trim)
import Data.Typeable (Typeable)

--import Debug.Trace
trace a b = b

-- ------------------------------------------------------------
-- * Description of fields
-- ------------------------------------------------------------


-- | A type for printing something
type Printer beta       =   beta -> PP.Doc

-- | A type for parsing something
type Parser beta        =   CharParser () beta

-- |  A type for getting a field of a record
type Getter alpha beta = alpha -> beta

-- |  A type for setting the field of a record
type Setter alpha beta = beta -> alpha -> alpha

-- | A description of a printable and parsable entity
data FieldDescriptionS alpha =  FieldS {
        fieldName       ::  String,
        fieldPrinter    ::  alpha -> PP.Doc,
        fieldParser     ::  alpha -> CharParser () alpha,
        fieldSynopsis   ::  Maybe String
    }

data GenFieldDescriptionS = forall alpha . Typeable alpha => GenFS [FieldDescriptionS alpha] alpha


-- | A type for categories in ini files
type Category = String

-- | Make a field description

mkFieldS :: String ->
    Maybe String ->
    (Printer beta) ->
    (Parser beta) ->
    (Getter alpha beta) ->
    (Setter alpha beta) ->
    FieldDescriptionS alpha
mkFieldS name synopsis printer parser getter setter =
    FieldS name
        (\ dat -> (PP.text name PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case synopsis of
                                    Nothing -> PP.empty
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> (do
            try (symbol name)
            colon
            value <- parser
            return (setter value dat)))
        synopsis

-- ------------------------------------------------------------
-- * Read and write
-- ------------------------------------------------------------

type PrefDescr = Either [(String,GenFieldDescriptionS)]
    GenFieldDescriptionS

showFields :: PrefDescr -> String
showFields (Right (GenFS dateDesc date))  = PP.render $
    foldl' (\ doc FieldS{fieldPrinter = printer} ->  doc PP.$+$ printer date)
        PP.empty dateDesc
showFields (Left catList)  = PP.render $ PP.vcat (map showField' catList)
  where
    showField' (catString, (GenFS dateDesc date)) =
        foldl' (\ doc FieldS{fieldPrinter = printer} ->  doc PP.$+$ printer date)
            (PP.text ("[" ++ catString ++ "]")) dateDesc

readFields :: FilePath -> PrefDescr -> IO PrefDescr
readFields fn descrs = catch (do
    res <- parseFromFile (parseFields' descrs) fn
    case res of
                Left pe -> error $ "Error reading file " ++ show pe
                Right r -> return r)
    (\ e -> error $ "Error reading file " ++ show e)

parseFields :: String -> PrefDescr -> PrefDescr
parseFields str fieldDescrs =
    case parse (parseFields' fieldDescrs) "" str of
                Left pe -> error $ "Parse error " ++ show pe
                Right r -> r

parseFields' ::   PrefDescr -> CharParser () PrefDescr
parseFields' (Right (GenFS fds defaultValue)) =
    let parsersF = map fieldParser fds in do
        res <-  applyFieldParsers defaultValue parsersF
        return (Right (GenFS fds res))
        <?> "parseFields' parser1"
parseFields' (Left descrList) = do
    newDescrList <- parseCategory [] descrList
    return (Left newDescrList)
        <?> "parseFields' parser2"

parseCategory :: [(String,GenFieldDescriptionS)] -> [(String,GenFieldDescriptionS)]
    -> CharParser () [(String,GenFieldDescriptionS)]
parseCategory accu descrList = trace "parseCategory1" (
    do
        eof
        return accu
    <|> do
        name <- parseCatName
        trace ("parseCategory2 " ++ name) (
            case [genFs | (n,genFs) <- descrList, n == name] of
                [(GenFS fieldList defaultValue)] -> do
                    res <-  applyFieldParsers defaultValue (map fieldParser fieldList)
                    parseCategory ((name,GenFS fieldList res) : accu) descrList
                otherwise -> do
                    skipToNextCategory
                    parseCategory accu descrList) -- no error message, unknown categories are just skipped
    <?> "parseCategory")

parseCatName ::  CharParser () String
parseCatName = do
    symbol "["
    str <- many (noneOf "]\n")
    symbol "]"
    return str
        <?> "parseCatName"

skipToNextCategory  ::  CharParser () ()
skipToNextCategory = do
    eof
    <|> do
        many (noneOf "\n")
        do
            optional (char '\n')
            c <- lookAhead anyChar
            if c == '['
                then return ()
                else skipToNextCategory
    <?> "skipToNextCategory"

applyFieldParsers ::  a ->  [a ->  CharParser () a] ->  CharParser () a
applyFieldParsers prefs parseF = trace "afp1" (do
        eof
        return (prefs))
    <|> trace "afp2" (do
        lookAhead (char '[')
        return (prefs))
    <|> trace "afp3" (do
        let parsers = map (\a -> trace "xx" (a prefs)) parseF
        newprefs <-  choice parsers
        whiteSpace
        applyFieldParsers newprefs parseF)
    <?> trace "afp4" "applyFieldParsers"

-- ------------------------------------------------------------
-- * Convenience methods with simpler interfaces for files without category
-- ------------------------------------------------------------

readFieldsSimple :: Typeable alpha => FilePath -> [FieldDescriptionS alpha] -> alpha -> IO alpha
readFieldsSimple fn descriptions defaultValue = catch (do
    putStrLn ("readFieldsSimple " ++ fn)
    res <- parseFromFile (parseFields' (Right $ GenFS descriptions defaultValue)) fn
    case res of
                Left pe -> error $ "Error reading file " ++ show pe
                Right (Right (GenFS _ r)) ->
                    return $ myCast "ConfigFile>>readFieldsSimple:" r
                Right (Left _) -> error "ConfigFile>>readFieldsSimple: impossible")
    (\ e -> error $ "Error reading file " ++ show e)

parseFieldsSimple :: Typeable alpha => String -> [FieldDescriptionS alpha] -> alpha -> alpha
parseFieldsSimple str descriptions defaultValue  =
    let res = parseFields str (Right $ GenFS descriptions defaultValue)
    in case res of
        (Right (GenFS _ r)) -> myCast "ConfigFile>>parseFieldsSimple:" r
        otherwise -> error "ConfigFile>>parseFieldsSimple: impossible"

showFieldsSimple :: Typeable alpha =>  alpha -> [FieldDescriptionS alpha] -> String
showFieldsSimple date dateDesc = showFields (Right $ GenFS dateDesc date)

-- ------------------------------------------------------------
-- * Parsers and printers
-- ------------------------------------------------------------

-- A String parser parses some string, which may span multiple lines
-- Carriage returns are not significant
stringParser :: CharParser () String
stringParser = trace "str1" (do
    firstLine <- many (noneOf ['\n'])
    optional (char '\n')
    lines <- many $ parseStringLine
    let res = trim (unwords (firstLine : lines))
    trace ("stringParser \"" ++ res ++ "\"") $ return res)
    <?> "stringParser"

parseStringLine :: CharParser () String
parseStringLine =  trace "parseValueLine" (do
    many1 (oneOf " \t") -- fails for lines which starts with a non blank
    (do
            char '-' -- skip comments
            char '-'
            many (noneOf ['\n'])
            optional (char '\n')
            return []) --skip comment
        <|> (do
                line <- many (noneOf ['\n'])
                optional (char '\n')
                return line)
    <?> trace "vl3" "parseValueLine")

boolParser ::  CharParser () Bool
boolParser = trace "bp1" (do
    (symbol "True" <|> symbol "true")
    return True)
    <|> trace "bp2" (do
    (symbol "False"<|> symbol "false")
    return False)
    <?> "bool parser"

readParser ::  Read a =>  CharParser () a
readParser = trace "rp1" (do
    str <- stringParser
    trace ("readParser on:" ++ str) $ if null str
        then unexpected "read parser on empty string"
        else do
            case maybeRead str of
                Nothing -> unexpected $ "read parser no parse " ++ str
                Just r -> return r)
    <?> "read parser"

pairParser ::  CharParser () alpha ->  CharParser () (alpha,alpha)
pairParser p2 = trace "pp1" (do
    parens $ do
        v1 <-  p2
        comma
        v2 <-  p2
        return (v1,v2))
    <?> "pair parser"

versionParser :: CharParser () Version
versionParser = trace "vp1" (do
    branch <-  sepBy1 intParser dot
    return Version{versionBranch=branch, versionTags=[]})
    <?> "version parser"


intParser ::  CharParser () Int
intParser = trace "ip1" (do
    i <-  integer
    return (fromIntegral i))
    <?> "int parser"

emptyParser ::  CharParser () ()
emptyParser = pzero

prefsStyle  ::  P.LanguageDef st
prefsStyle  = emptyDef  {
        P.commentStart   = "{-"
    ,   P.commentEnd     = "-}"
    ,   P.commentLine    = "--"
    }

lexer :: P.TokenParser st
lexer = P.makeTokenParser prefsStyle

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> CharParser st String
symbol = P.symbol lexer

identifier, colon :: CharParser st String
identifier = P.identifier lexer
colon      = P.colon lexer
comma      = P.comma lexer
parens     = P.parens lexer
dot        = P.dot lexer
integer    = P.integer lexer
brackets   = P.brackets lexer

skipToCategory str = do
            symbol ("[" ++ str ++ "]")
            return ()
    <|> do
            many (noneOf ['\n'])
            char '\n'
            skipToCategory str
    <?>
            "skipToCategory"

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

stringPrinter :: String -> PP.Doc
stringPrinter str = PP.fsep $ map PP.text (words str)

emptyPrinter ::  () ->  PP.Doc
emptyPrinter _ = PP.empty

maybePP :: (a -> PP.Doc) -> Maybe a -> PP.Doc
maybePP _ Nothing = PP.empty
maybePP pp (Just a) = pp a



-- ------------------------------------------------------------
-- * Special for plugins
------------------------------------------------------------

showPluginList :: [Prerequisite] -> String
showPluginList [] = "[]"
showPluginList l = "[" ++ concat (intersperse ", " (map showPlugin l)) ++ "]"
  where
    showPlugin (name,versionBounds) = "(" ++ name ++ "," ++ showVersionBounds versionBounds ++ ")"
    showVersionBounds (a,b) = showBound a ++ "," ++ showBound b
    showBound Nothing = "any"
    showBound (Just v) = showVersion v

parsePluginList :: CharParser () [Prerequisite]
parsePluginList = brackets (sepBy pluginParser comma)

pluginParser :: CharParser () (PluginName, VersionBounds)
pluginParser = trace "pp1" (
    parens $ do
        pluginName <- liftM trim (many (noneOf ","))
        comma
        lowerBound <- boundParser
        comma
        upperBound <- boundParser
        return (pluginName,(lowerBound,upperBound)))
    <?> "pluginParser"

boundParser :: CharParser () (Maybe Version)
boundParser =
    trace "bp1" (try $ do
            v <- versionParser
            return (Just v))
    <|> trace "bp2" (do
            symbol "any"
            return Nothing)
    <?> "boundParser"

