module Main where

{- TODO
Chapter 2 exercise 7, full numeric tower: rational, etc.
-}
--import Lib -- This is my Lib module at src/Lib.hs
import Control.Monad
import qualified Data.Array as Array
import qualified Numeric
import System.Environment
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

data LispVal
  = LispAtom String
  | LispBool Bool
  | LispChar Char
  | LispDottedList [LispVal]
                   LispVal
  | LispFloat Double
  | LispList [LispVal]
  | LispNumber Integer
  | LispString String
  | LispVector (Array.Array Int LispVal)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input =
  case Parsec.parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found Value"

{- Parsers -}
parseAtom :: Parsec.Parser LispVal
parseAtom = do
  first <- Parsec.choice [Parsec.letter, symbol]
  rest <- Parsec.many (Parsec.letter <|> Parsec.digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> LispBool True
      "#f" -> LispBool False
      _ -> LispAtom atom

parseBinary :: Parsec.Parser LispVal
parseBinary = do
  Parsec.try $ Parsec.string "#b"
  b <- Parsec.many1 $ Parsec.oneOf "10"
  return $ LispNumber (binaryToDecimal b)

parseBool :: Parsec.Parser LispVal
parseBool = do
  Parsec.char '#'
  b <- Parsec.oneOf "tf"
  return $
    case b of
      't' -> LispBool True
      'f' -> LispBool False

parseChar :: Parsec.Parser LispVal
parseChar = do
  Parsec.try (Parsec.string "#\\")
  value <-
    Parsec.try (Parsec.string "newline" <|> Parsec.string "space") <|>
    anySingleChar
  return $ LispChar (valueToChar value)
  where
    anySingleChar = do
      x <- Parsec.anyChar
      Parsec.notFollowedBy Parsec.alphaNum
      return [x]
    valueToChar "space" = ' '
    valueToChar "newline" = '\n'
    valueToChar other = other !! 0

parseDecimal1 :: Parsec.Parser LispVal
parseDecimal1 = do
  d <- Parsec.many1 Parsec.digit
  return $ LispNumber $ read d

parseDecimal2 :: Parsec.Parser LispVal
parseDecimal2 = do
  Parsec.try $ Parsec.string "#d"
  d <- Parsec.many1 Parsec.digit
  return $ LispNumber $ read d

parseDottedList :: Parsec.Parser LispVal
parseDottedList = do
  head <- Parsec.endBy parseExpr spaces
  tail <- Parsec.char '.' >> spaces >> parseExpr
  return $ LispDottedList head tail

parseEscapeChars :: Parsec.Parser String
parseEscapeChars = do
  Parsec.char '\\'
  x <- Parsec.oneOf "\"\\rnt"
  return $
    case x of
      '\\' -> [x]
      '"' -> [x]
      't' -> "\t"
      'n' -> "\n"
      'r' -> "r"

parseExpr :: Parsec.Parser LispVal
parseExpr =
  parseAtom <|> --
  parseLists <|>
  parseQuoted <|>
  parseQuasiQuoted <|>
  parseString <|>
  parseUnQuote <|>
  Parsec.try parseFloat <|>
  Parsec.try parseNumber <|>
  Parsec.try parseBool <|>
  Parsec.try parseChar <|>
  Parsec.try parseVectors
  where
    parseLists = do
      Parsec.char '('
      x <- Parsec.try parseList <|> parseDottedList
      Parsec.char ')'
      return x
    parseVectors = do
      Parsec.string "#("
      x <- parseVector
      Parsec.char ')'
      return x

parseFloat :: Parsec.Parser LispVal
parseFloat = do
  d1 <- Parsec.many1 Parsec.digit
  Parsec.char '.'
  d2 <- Parsec.many1 Parsec.digit
  return $ LispFloat (read (d1 ++ "." ++ d2))

parseHex :: Parsec.Parser LispVal
parseHex = do
  Parsec.try $ Parsec.string "#x"
  h <- Parsec.many1 Parsec.hexDigit
  return $ LispNumber $ hexToDecimal h

parseList :: Parsec.Parser LispVal
parseList = do
  liftM LispList $ Parsec.sepBy parseExpr spaces

parseNumber :: Parsec.Parser LispVal
parseNumber =
  parseDecimal1 <|> --
  parseDecimal2 <|>
  parseHex <|>
  parseOct <|>
  parseBinary

parseNumber_ :: Parsec.Parser LispVal
parseNumber_ = do
  str <- Parsec.many1 Parsec.digit
  return $ LispNumber $ read str

parseNumber__ :: Parsec.Parser LispVal
parseNumber__ =
  (Parsec.many1 Parsec.digit) >>= (\str -> return (LispNumber $ read str))

parseOct :: Parsec.Parser LispVal
parseOct = do
  Parsec.try $ Parsec.string "#o"
  o <- Parsec.many1 Parsec.octDigit
  return $ LispNumber $ octalToDecimal o

parseQuasiQuoted :: Parsec.Parser LispVal
parseQuasiQuoted = do
  Parsec.char '`'
  x <- parseExpr
  return $ LispList [LispAtom "quasiquote", x]

parseQuoted :: Parsec.Parser LispVal
parseQuoted = do
  Parsec.char '\''
  x <- parseExpr
  return $ LispList [LispAtom "quote", x]

parseString :: Parsec.Parser LispVal
parseString = do
  Parsec.char '"'
  x <- Parsec.many $ Parsec.many1 (Parsec.noneOf "\"\\") <|> parseEscapeChars
  Parsec.char '"'
  return (LispString (concat x))

parseString_ :: Parsec.Parser LispVal
parseString_ = do
  Parsec.char '"'
  x <- Parsec.many (Parsec.noneOf "\"")
  Parsec.char '"'
  return (LispString x)

spaces :: Parsec.Parser ()
spaces = Parsec.skipMany1 Parsec.space

symbol :: Parsec.Parser Char
symbol = Parsec.oneOf "!$%&|*+-/:<=>?@^_~"

parseUnQuote :: Parsec.Parser LispVal
parseUnQuote = do
  Parsec.char ','
  x <- parseExpr
  return $ LispList [LispAtom "unquote", x]

parseVector :: Parsec.Parser LispVal
parseVector = do
  arrayValues <- Parsec.sepBy parseExpr spaces
  Parsec.spaces
  return $
    LispVector (Array.listArray (0, (length arrayValues - 1)) arrayValues)

{- converters -}
binaryToDecimal :: String -> Integer
binaryToDecimal = binaryToDecimal_ 0

binaryToDecimal_ :: Integer -> String -> Integer
binaryToDecimal_ decimal "" = decimal
binaryToDecimal_ decimal (d:ds) =
  let ones =
        if d == '0'
          then 0
          else 1
      decimal_ = 2 * decimal + ones
  in binaryToDecimal_ decimal_ ds

hexToDecimal :: String -> Integer
hexToDecimal hex = fst $ Numeric.readHex hex !! 0

octalToDecimal :: String -> Integer
octalToDecimal oct = fst $ Numeric.readOct oct !! 0
