{-# OPTIONS_GHC -XTypeSynonymInstances #-}
--
-- | Module for saving and restoring preferences and settings
--

module Base.PrinterParser (

    Printer
,   Parser
,   FieldDescriptionS(..)
,   mkFieldS

,   applyFieldParsers
,   boolParser
,   intParser
,   lineParser
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
,   Pretty(..)
,   prettyPrint
,   maybePP

,   symbol
,   colon

,   writeFields
,   showFields
,   readFields
,   parseFields
) where

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.PrettyPrint as PP

import Data.Maybe (listToMaybe)
import Data.List (intersperse, foldl')
import qualified Text.ParserCombinators.Parsec as  P
    ((<?>), CharParser(..), parseFromFile)
import Data.Version (showVersion, Version(..))
import Base.PluginTypes (VersionBounds, PluginName, Prerequisite)
import Control.Monad (liftM)
import Base.MyMissing (trim)
import qualified Text.Parsec as P (parse)

-- | A type for printing something
type Printer beta       =   beta -> PP.Doc

-- | A type for parsing something
type Parser beta        =   CharParser () beta

-- |  A type for getting a field of a record
type Getter alpha beta = alpha -> beta

-- |  A type for setting the field of a record
type Setter alpha beta = beta -> alpha -> alpha

-- | A description of a printable and parsable entity
data FieldDescriptionS alpha =  FDS {
        fieldName       ::  String,
        fieldPrinter    ::  alpha -> PP.Doc,
        fieldParser     ::  alpha -> CharParser () alpha,
        fieldSynopsis   ::  Maybe String
    }

-- | A type for making
--type MkFieldDescriptionS alpha beta =
--    String ->
--    Maybe String ->
--    (Printer beta) ->
--    (Parser beta) ->
--    (Getter alpha beta) ->
--    (Setter alpha beta) ->
--    FieldDescriptionS alpha

mkFieldS :: String ->
    Maybe String ->
    (Printer beta) ->
    (Parser beta) ->
    (Getter alpha beta) ->
    (Setter alpha beta) ->
    FieldDescriptionS alpha
mkFieldS name synopsis printer parser getter setter =
    FDS name
        (\ dat -> (PP.text name PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case synopsis of
                                    Nothing -> PP.empty
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> try (do
            symbol name
            colon
            val <- parser
            return (setter val dat)))
        synopsis

applyFieldParsers ::  a ->  [a ->  CharParser () a] ->  CharParser () a
applyFieldParsers prefs parseF = do
    eof
    return (prefs)
    <|> do
    let parsers = map (\a ->  a prefs) parseF
    newprefs <-  choice parsers
    whiteSpace
    applyFieldParsers newprefs parseF
    <?> "field parser"

boolParser ::  CharParser () Bool
boolParser = do
    (symbol "True" <|> symbol "true")
    return True
    <|> do
    (symbol "False"<|> symbol "false")
    return False
    <?> "bool parser"

readParser ::  Read a =>  CharParser () a
readParser = do
    str <- many (noneOf ['\n'])
    if null str
        then unexpected "read parser on empty string"
        else do
            case maybeRead str of
                Nothing -> unexpected $ "read parser no parse " ++ str
                Just r -> return r
    <?> "read parser"
        where maybeRead = listToMaybe . map fst . filter (null . snd) . reads

pairParser ::  CharParser () alpha ->  CharParser () (alpha,alpha)
pairParser p2 = do
    char '('
    v1 <-  p2
    char ','
    v2 <-  p2
    char ')'
    return (v1,v2)
    <?> "pair parser"

stringParser ::  CharParser () String
stringParser = do
    char '"'
    str <- many (noneOf ['"'])
    char '"'
    return (str)
    <?> "string parser"

versionParser :: CharParser () Version
versionParser = do
    branch <-  sepBy1 intParser (char '.')
    return Version{versionBranch=branch, versionTags=[]}

showPluginList :: [Prerequisite] -> String
showPluginList [] = "[]"
showPluginList l = "[" ++ concat (intersperse "," (map showPlugin l)) ++ "]"
  where
    showPlugin (name,versionBounds) = "(" ++ name ++ "," ++ showVersionBounds versionBounds ++ ")"
    showVersionBounds (a,b) = showBound a ++ "," ++ showBound b
    showBound Nothing = "any"
    showBound (Just v) = showVersion v

parsePluginList :: CharParser () [Prerequisite]
parsePluginList = do
    symbol "["
    plugs <- sepBy pluginParser (char ',')
    symbol "]"
    return plugs

pluginParser :: CharParser () (PluginName, VersionBounds)
pluginParser = do
    symbol "("
    pluginName <- liftM trim (many (noneOf ","))
    symbol ","
    lowerBound <- boundParser
    symbol ","
    upperBound <- boundParser
    symbol")"
    return (pluginName,(lowerBound,upperBound))

boundParser :: CharParser () (Maybe Version)
boundParser =
    try $ do
            v <- versionParser
            return (Just v)
    <|> do
            symbol "any"
            return Nothing

lineParser ::  CharParser () String
lineParser = do
    str <- many (noneOf ['\n'])
    return (str)
    <?> "line parser"


intParser ::  CharParser () Int
intParser = do
    i <-  integer
    return (fromIntegral i)

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
colon = P.colon lexer

integer = P.integer lexer

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------
-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> String
prettyPrint a = PP.renderStyle  PP.style (pretty a)

-- | Things that can be pretty-printed
class Pretty a where
	-- | Pretty-print something in isolation.
	pretty :: a -> PP.Doc
	-- | Pretty-print something in a precedence context.
	prettyPrec :: Int -> a -> PP.Doc
	pretty = prettyPrec 0
	prettyPrec _ = pretty
	
emptyPrinter ::  () ->  PP.Doc
emptyPrinter _ = PP.empty

maybePP :: (a -> PP.Doc) -> Maybe a -> PP.Doc
maybePP _ Nothing = PP.empty
maybePP pp (Just a) = pp a

instance Pretty String where
    pretty str = PP.text str

-- ------------------------------------------------------------
-- * Read and write
-- ------------------------------------------------------------

writeFields :: FilePath -> alpha -> [FieldDescriptionS alpha] -> IO ()
writeFields fpath date dateDesc = writeFile fpath (showFields date dateDesc)

showFields ::  alpha  -> [FieldDescriptionS alpha] ->  String
showFields date dateDesc = PP.render $
    foldl' (\ doc FDS{fieldPrinter = printer} ->  doc PP.$+$ printer date) PP.empty dateDesc

readFields :: FilePath -> [FieldDescriptionS alpha] -> alpha -> IO alpha
readFields fn fieldDescrs defaultValue = catch (do
    res <- P.parseFromFile (parseFields' defaultValue fieldDescrs) fn
    case res of
                Left pe -> error $ "Error reading file " ++ show fn ++ " " ++ show pe
                Right r -> return r)
    (\ e -> error $ "Error reading file " ++ show fn ++ " " ++ show e)

parseFields :: String -> [FieldDescriptionS alpha] -> alpha -> alpha
parseFields str fieldDescrs defaultValue =
    case P.parse (parseFields' defaultValue fieldDescrs) "" str of
                Left pe -> error $ "Parse error " ++ show pe
                Right r -> r


parseFields' ::  alpha ->  [FieldDescriptionS alpha] ->  P.CharParser () alpha
parseFields' defaultValue descriptions =
    let parsersF = map fieldParser descriptions in do
        res <-  applyFieldParsers defaultValue parsersF
        return res
        P.<?> "prefs parser"



