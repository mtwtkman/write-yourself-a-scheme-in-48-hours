module Scheme (LispVal (..), readExpr, eval, extractValue, trapError) where

import Data.Functor
import Data.Char
import Control.Monad
import Control.Monad.Except
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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               deriving (Eq)

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
  deriving (Eq)

instance Show LispVal where show = showVal

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote",val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] ->ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("symbol?", unaryOp symbolp)
             , ("string?", unaryOp stringp)
             , ("number?", unaryOp numberp)
             , ("bool?", unaryOp boolp)
             , ("list?", unaryOp listp)
             , ("symbol->string", unaryOp symbol2string)
             , ("string->symbol", unaryOp string2symbol)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params <&> (Number . foldl1 op)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v

symbolp,numberp,stringp,boolp,listp,string2symbol,symbol2string :: LispVal -> ThrowsError LispVal
symbolp (Atom _) = return $ Bool True
symbolp _ = return $ Bool False

numberp (Number _) = return $ Bool True
numberp _ = return $ Bool False

stringp (String _) = return $ Bool True
stringp _ = return $ Bool False

boolp (Bool _) = return $ Bool True
boolp _ = return $ Bool False

listp (List _) = return $ Bool True
listp (DottedList _ _) = return $ Bool True
listp _ = return $ Bool False

string2symbol (String s) = return $ Atom s
string2symbol s = throwError $ TypeMismatch "string" s

symbol2string (Atom s) = return $ String s
symbol2string s = throwError $ TypeMismatch "symbol" s
