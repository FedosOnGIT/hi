module HW3.Parser where

import Data.Void (Void)
import Control.Applicative ( Alternative(..), optional, liftA2)
import           Control.Monad       (MonadPlus, mfilter)
import Text.Megaparsec.Error
import Text.Megaparsec (Parsec, (<|>), choice, some, label, optional, between, sepBy, parse, eof, manyTill, satisfy, try, sepBy1, notFollowedBy)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (numberChar, space1, char, space, hexDigitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific
import GHC.Real hiding (Rational)
import Data.Ratio (Rational)
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.List (intercalate)
import qualified Data.Word as W
import Text.Megaparsec.Debug (dbg)
import Control.Monad.Combinators (manyTill)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Data.ByteString as B
import HW3.Base

type Parser = Parsec Void String

binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL name function = InfixL  (binaryInfix function <$ parseString name)

binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryN name function = InfixN  (binaryInfix function <$ parseString name)

binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryR name function = InfixR  (binaryInfix function <$ parseString name)

binaryInfix:: HiFun -> HiExpr -> HiExpr -> HiExpr
binaryInfix function first second = HiExprApply (HiExprValue (HiValueFunction function)) [first, second]

binaryDiv :: Operator Parser HiExpr
binaryDiv = InfixL (binaryInfix HiFunDiv <$ try (parseString "/" <* notFollowedBy (char '=')))

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = [
  [binaryDiv, binaryL "*" HiFunMul],
  [binaryL "+" HiFunAdd, binaryL "-" HiFunSub],
  [binaryN "<" HiFunLessThan, binaryN ">" HiFunGreaterThan, binaryN "<=" HiFunNotGreaterThan, binaryN ">=" HiFunNotLessThan, binaryN "==" HiFunEquals, binaryN "/=" HiFunNotEquals],
  [binaryR "&&" HiFunAnd],
  [binaryR "||" HiFunOr]]
functions = [
    ("add", HiFunAdd)
  , ("sub", HiFunSub)
  , ("mul", HiFunMul)
  , ("div", HiFunDiv)
  , ("and", HiFunAnd)
  , ("or", HiFunOr)
  , ("equals", HiFunEquals)
  , ("less-than", HiFunLessThan)
  , ("greater-than", HiFunGreaterThan)
  , ("not-equals", HiFunNotEquals)
  , ("not-less-than", HiFunNotLessThan)
  , ("not-greater-than", HiFunNotGreaterThan)
  , ("not", HiFunNot)
  , ("if", HiFunIf)
  , ("length", HiFunLength)
  , ("to-upper", HiFunToUpper)
  , ("to-lower", HiFunToLower)
  , ("reverse", HiFunReverse)
  , ("trim", HiFunTrim)
  , ("list", HiFunList)
  , ("range", HiFunRange)
  , ("fold", HiFunFold)
  , ("pack-bytes", HiFunPackBytes)
  , ("unpack-bytes", HiFunUnpackBytes)
  , ("encode-utf8", HiFunEncodeUtf8)
  , ("decode-utf8", HiFunDecodeUtf8)
  , ("zip", HiFunZip)
  , ("unzip", HiFunUnzip)
  , ("serialise", HiFunSerialise)
  , ("deserialise", HiFunDeserialise)
  , ("read", HiFunRead)
  , ("write", HiFunWrite)
  , ("mkdir", HiFunMkDir)
  , ("cd", HiFunChDir)
  , ("parse-time", HiFunParseTime)
  , ("rand", HiFunRand)
  , ("echo", HiFunEcho)
  , ("count", HiFunCount)
  , ("keys", HiFunKeys)
  , ("values", HiFunValues)
  , ("invert", HiFunInvert)
  ]
boolean = [
    ("true", True)
  , ("false", False)
  ]
actions = [
    ("cwd", HiActionCwd)
  , ("now", HiActionNow) 
  ]

mapFunctions :: (String, HiFun) -> Parser HiValue
mapFunctions (operation, result) = mapStrings (operation, HiValueFunction result)

mapBoolean :: (String, Bool) -> Parser HiValue
mapBoolean (operation, result) = mapStrings (operation, HiValueBool result)

mapActions :: (String, HiAction) -> Parser HiValue
mapActions (operation, result) = mapStrings (operation, HiValueAction result)

mapStrings :: (String, HiValue) -> Parser HiValue
mapStrings (operation, result) = do 
  parseString operation
  return result
             
parseString :: String -> Parser String
parseString input = do
  _ <- space
  result <- string input
  _ <- space
  return result

parseExpression :: Parser HiExpr
parseExpression = makeExprParser parseTerm operatorTable

parseTerm :: Parser HiExpr
parseTerm = do
  expression <- parseTermHelper
  parseRest expression

parseTermHelper :: Parser HiExpr
parseTermHelper =  do
  parenL <- optional $ parseString "("
  case parenL of
    Just value -> do
      expression <- parseExpression
      _ <- parseString ")"
      return expression
    Nothing -> choice [parseFunction, parseNumber, parseBoolean, parseText, parseNull, parseByteString, parseList, parseAction, parseDictionary]

parseRest :: HiExpr -> Parser HiExpr
parseRest expression = do
  running <- optional $ parseString "!"
  case running of
    Just value -> parseRest (HiExprRun expression)
    Nothing -> do
      arguments <- optional $ choice [parseArguments, parseDot]
      case arguments of
        Just args -> parseRest (HiExprApply expression args)
        Nothing -> return expression

parseFunction :: Parser HiExpr
parseFunction = do
  operation <- choice (Prelude.map mapFunctions functions)
  return (HiExprValue operation)

execute :: HiExpr -> Parser HiExpr
execute expression = do
  go <- choice [parseString "!", parseString ""]
  if go == ""
  then return expression
  else return $ HiExprRun expression
  
parseArguments :: Parser [HiExpr]
parseArguments = do
   arguments <- between (parseString "(") (parseString ")") (sepBy parseExpression (parseString ","))
   return arguments

parseDot :: Parser [HiExpr]
parseDot = do
  _ <- parseString "."
  arguments <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return [HiExprValue $ HiValueString $ Data.Text.pack $ Data.List.intercalate "-" arguments]

parseNumber :: Parser HiExpr
parseNumber = do
  number <- L.signed space L.scientific 
  _ <- space
  return (HiExprValue (HiValueNumber (toRational number)))
  
parseBoolean :: Parser HiExpr
parseBoolean = do
  operation <- choice (Prelude.map mapBoolean boolean)
  return (HiExprValue operation)

parseText :: Parser HiExpr
parseText = do
  operation <- Text.Megaparsec.Char.char '"' *> (pack <$> Text.Megaparsec.manyTill L.charLiteral (Text.Megaparsec.Char.char '"'))
  _ <- space
  return (HiExprValue (HiValueString operation))

parseByte :: Parser (Char, Char)
parseByte = do
  first <- hexDigitChar
  second <- hexDigitChar
  _ <- space
  return (first, second)

parseByteString :: Parser HiExpr
parseByteString = do
  arguments <- between (parseString "[#") (parseString "#]") (many parseByte)
  let bytes = B.pack $ Prelude.map (\(first, second)-> Prelude.fromIntegral (digitToInt first * 16 + digitToInt second)) arguments
  return $ HiExprValue $ HiValueBytes bytes

parseList :: Parser HiExpr
parseList = do
  arguments <- between (parseString "[") (parseString "]") (sepBy parseExpression (parseString ","))
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) arguments
  
parseNull :: Parser HiExpr
parseNull = do
  operation <- parseString "null"
  return (HiExprValue HiValueNull)

parseAction :: Parser HiExpr
parseAction = do
  action <- choice (Prelude.map mapActions actions)
  return $ HiExprValue action
  
parseMap :: Parser (HiExpr, HiExpr)
parseMap = do
    first <- parseExpression
    _ <- parseString ":"
    second <- parseExpression
    return (first, second)
    
parseDictionary :: Parser HiExpr
parseDictionary = do
  arguments <- between (parseString "{") (parseString "}") (sepBy parseMap (parseString ","))
  return $ HiExprDict arguments
  
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = Text.Megaparsec.parse (between space space parseExpression <* Text.Megaparsec.eof) ""

