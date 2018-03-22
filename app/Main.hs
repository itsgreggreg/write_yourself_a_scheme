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
    Right _val -> "Found Value"

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
  _ <- Parsec.try $ Parsec.string "#b"
  b <- Parsec.many1 $ Parsec.oneOf "10"
  return $ LispNumber (binaryToDecimal b)

parseBool :: Parsec.Parser LispVal
parseBool = do
  _ <- Parsec.char '#'
  b <- Parsec.oneOf "tf"
  return $
    if b == 't'
      then LispBool True
      else LispBool False

parseChar :: Parsec.Parser LispVal
parseChar = do
  _ <- Parsec.try (Parsec.string "#\\")
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
  _ <- Parsec.try $ Parsec.string "#d"
  d <- Parsec.many1 Parsec.digit
  return $ LispNumber $ read d

parseDottedList :: Parsec.Parser LispVal
parseDottedList = do
  left <- Parsec.endBy parseExpr spaces
  right <- Parsec.char '.' >> spaces >> parseExpr
  return $ LispDottedList left right

parseEscapeChars :: Parsec.Parser String
parseEscapeChars = do
  _ <- Parsec.char '\\'
  x <- Parsec.oneOf "\"\\rnt"
  return $
    case x of
      '\\' -> [x]
      '"' -> [x]
      't' -> "\t"
      'n' -> "\n"
      'r' -> "r"
      _ -> error $ "Not a valid escape char: " ++ [x]

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
      _ <- Parsec.char '('
      x <- Parsec.try parseList <|> parseDottedList
      _ <- Parsec.char ')'
      return x
    parseVectors = do
      _ <- Parsec.string "#("
      x <- parseVector
      _ <- Parsec.char ')'
      return x

parseFloat :: Parsec.Parser LispVal
parseFloat = do
  d1 <- Parsec.many1 Parsec.digit
  _ <- Parsec.char '.'
  d2 <- Parsec.many1 Parsec.digit
  return $ LispFloat (read (d1 ++ "." ++ d2))

parseHex :: Parsec.Parser LispVal
parseHex = do
  _ <- Parsec.try $ Parsec.string "#x"
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
  _ <- Parsec.try $ Parsec.string "#o"
  o <- Parsec.many1 Parsec.octDigit
  return $ LispNumber $ octalToDecimal o

parseQuasiQuoted :: Parsec.Parser LispVal
parseQuasiQuoted = do
  _ <- Parsec.char '`'
  x <- parseExpr
  return $ LispList [LispAtom "quasiquote", x]

parseQuoted :: Parsec.Parser LispVal
parseQuoted = do
  _ <- Parsec.char '\''
  x <- parseExpr
  return $ LispList [LispAtom "quote", x]

parseString :: Parsec.Parser LispVal
parseString = do
  _ <- Parsec.char '"'
  x <- Parsec.many $ Parsec.many1 (Parsec.noneOf "\"\\") <|> parseEscapeChars
  _ <- Parsec.char '"'
  return (LispString (concat x))

parseString_ :: Parsec.Parser LispVal
parseString_ = do
  _ <- Parsec.char '"'
  x <- Parsec.many (Parsec.noneOf "\"")
  _ <- Parsec.char '"'
  return (LispString x)

spaces :: Parsec.Parser ()
spaces = Parsec.skipMany1 Parsec.space

symbol :: Parsec.Parser Char
symbol = Parsec.oneOf "!$%&|*+-/:<=>?@^_~"

parseUnQuote :: Parsec.Parser LispVal
parseUnQuote = do
  _ <- Parsec.char ','
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

{- showers -}
showVal :: LispVal -> String
showVal (LispString str) = "\"" ++ str ++ "\""
showVal (LispAtom name) = name
showVal (LispNumber num) = show num
showVal (LispBool True) = "#t"
showVal (LispBool False) = "#f"
showVal _ = error "Show val for this type not implemented"
