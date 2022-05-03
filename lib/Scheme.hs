module Scheme (LispVal (..), readExpr) where

import Data.Char
import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces1)
import System.Environment
import Data.Ratio
import Data.Complex
import Data.Array

oct2dig x = fst $ head (readOct x)
hex2dig x = fst $ head (readHex x)
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                             bin2dig' old xs
toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n


data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Array Int LispVal)
  | Number Integer
  | Float Double
  | Complex (Complex Double)
  | Ratio Rational
  | String String
  | Bool Bool
  | Character Char
  deriving (Eq, Show)

parseExpr :: Parser LispVal
parseExpr = try parseBool
  <|> parseString
  <|> parseVector
  <|> parseAtom
  <|> parseCharacter
  <|> try parseComplex
  <|> try parseFloat
  <|> try parseRatio
  <|> parseNumber
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> parseUnQuote
  <|> parseUnQuoteSplicing
  <|> parseAllTheLists

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right val -> val

escapeChars :: Parser Char
escapeChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
             '\\' -> x
             '"' -> x
             'n' -> '\n'
             'r' -> '\r'
             't' -> '\t'


parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
            <|> do { x <- anyChar ; notFollowedBy alphaNum ; return [x] }
  return $ Character $ case value of
                         "space" -> ' '
                         "newline" -> '\n'
                         _ -> head value

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapeChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom


parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . head $ readFloat (x++"."++y))

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal1
  char '+'
  y <- try parseFloat <|> parseDecimal1
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio (read x % read y)

parseDecimal1 :: Parser LispVal
parseDecimal1 = Number . read <$> many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  x <-  many1 digit
  (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 (oneOf "10")
  return $ Number (bin2dig x)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAllTheLists :: Parser LispVal
parseAllTheLists = do
  char '(' >> spaces
  head <- sepEndBy parseExpr spaces1
  do char '.' >> spaces1
     tail <- parseExpr
     spaces >> char ')'
     return $ DottedList head tail
     <|> (spaces >> char ')' >> return (List head))


parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
  char ','
  char '@'
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do
  string "#("
  elems <- sepBy parseExpr spaces1
  char ')'
  return $ Vector (listArray (0, length elems -1) elems)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces1 :: Parser ()
spaces1 = skipMany1 space

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

