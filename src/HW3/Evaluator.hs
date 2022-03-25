module HW3.Evaluator where
  
import           Control.Applicative (Alternative (..), optional)
import           Control.Monad       (MonadPlus, mfilter)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans (lift)
import Data.Text (Text(..), pack, unpack, length, toUpper, toLower, reverse, strip, drop, take)
import Data.Sequence (Seq(..), fromList, length, reverse, drop, take, singleton)
import Data.Foldable (toList)
import qualified Data.Traversable as TR
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.Ratio (Rational, numerator, denominator)
import Text.Megaparsec.Error
import GHC.IO
import Data.Void (Void)
import qualified Data.Word as W
import Data.Char (digitToInt)
import Codec.Compression.Zlib (compressWith, defaultCompressParams, bestCompression, decompress, compressLevel)
import Codec.Serialise
import qualified Data.ByteString as B (ByteString(..), pack, unpack, drop, take, length, reverse)
import qualified Data.ByteString.Lazy as LazyB
import HW3.Base
import HW3.Parser
import Text.Read (readMaybe)
import Data.Semigroup (stimes)
import Data.Time.Clock (addUTCTime, diffUTCTime)
import Prelude.SafeEnum (fromEnum)
import Data.Map (Map(..), fromList, (!), member, fromListWith, map, keys, elems, toList)

getArgumentsNumber :: HiFun -> Int
getArgumentsNumber function = case function of
  HiFunAdd -> 2
  HiFunSub -> 2
  HiFunMul -> 2
  HiFunDiv -> 2
  HiFunNot -> 1
  HiFunAnd -> 2
  HiFunOr -> 2
  HiFunLessThan -> 2
  HiFunGreaterThan -> 2
  HiFunEquals -> 2
  HiFunNotEquals -> 2
  HiFunNotLessThan -> 2
  HiFunNotGreaterThan -> 2
  HiFunIf -> 3
  HiFunLength -> 1
  HiFunToUpper -> 1
  HiFunToLower -> 1
  HiFunReverse -> 1
  HiFunTrim -> 1
  HiFunList -> 2
  HiFunRange -> 2
  HiFunFold -> 2
  HiFunPackBytes -> 1
  HiFunUnpackBytes -> 1
  HiFunEncodeUtf8 -> 1
  HiFunDecodeUtf8 -> 1
  HiFunZip -> 1
  HiFunUnzip -> 1
  HiFunSerialise -> 1
  HiFunDeserialise -> 1

checkInteger :: HiMonad m => Rational -> ExceptT HiError m Integer
checkInteger number = do
  if denominator number == 1
  then return $ numerator number
  else throwE HiErrorInvalidArgument

checkPositiveInteger :: HiMonad m => Rational -> ExceptT HiError m Integer
checkPositiveInteger number = do
  integer <- checkInteger number
  if integer <= 0
  then throwE HiErrorInvalidArgument
  else return integer
  
counting :: [HiValue] -> HiValue
counting list = HiValueDict $ Data.Map.map (HiValueNumber . toRational) ((Data.Map.fromListWith (+) . flip zip (repeat 1)) list)

executeFunction :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
executeFunction HiFunAdd [HiValueNumber first, HiValueNumber second] = return $ HiValueNumber $ first + second
executeFunction HiFunAdd [HiValueString first, HiValueString second] = return $ HiValueString $ first <> second
executeFunction HiFunAdd [HiValueList first, HiValueList second] = return $ HiValueList (first <> second)
executeFunction HiFunAdd [HiValueBytes first, HiValueBytes second] = return $ HiValueBytes (first <> second)
executeFunction HiFunAdd [HiValueTime first, HiValueNumber second] = do
  plus <- checkInteger second
  return $ HiValueTime $ addUTCTime (fromInteger plus) first
executeFunction HiFunSub [HiValueNumber first, HiValueNumber second] = return $ HiValueNumber $ first - second
executeFunction HiFunSub [HiValueTime first, HiValueTime second] = return $ HiValueNumber $ toRational $ diffUTCTime first second
executeFunction HiFunMul [HiValueNumber first, HiValueNumber second] = return $ HiValueNumber $ first * second
executeFunction HiFunMul [HiValueString first, HiValueNumber second] = do 
  number <- checkPositiveInteger second
  return $ HiValueString $ stimes number first
executeFunction HiFunMul [HiValueList first, HiValueNumber second] =  do 
   number <- checkPositiveInteger second
   return $ HiValueList $ stimes number first
executeFunction HiFunMul [HiValueBytes first, HiValueNumber second] = do 
  number <- checkPositiveInteger second
  return $ HiValueBytes $ stimes number first
executeFunction HiFunDiv [HiValueNumber first, HiValueNumber second] =
  if second == 0
  then throwE HiErrorDivideByZero
  else return $ HiValueNumber $ first / second
executeFunction HiFunDiv [HiValueString first, HiValueString second] = return $ HiValueString $ first <> pack "/" <> second
executeFunction HiFunEquals [first, second] = return $ HiValueBool $ first == second
executeFunction HiFunNotEquals [first, second] = return $ HiValueBool $ first /= second
executeFunction HiFunLessThan [first, second] = return $ HiValueBool $ first < second
executeFunction HiFunGreaterThan [first, second] = return $ HiValueBool $ first > second
executeFunction HiFunNotLessThan [first, second] = return $ HiValueBool $ first >= second
executeFunction HiFunNotGreaterThan [first, second] = return $ HiValueBool $ first <= second
executeFunction HiFunNot [HiValueBool boolean] = return $ HiValueBool $ not boolean
executeFunction HiFunLength [HiValueString text] = return $ HiValueNumber $ toRational $ Data.Text.length text
executeFunction HiFunLength [HiValueList list] = return $ HiValueNumber $ toRational $ Data.Sequence.length list
executeFunction HiFunLength [HiValueBytes bytes] = return $ HiValueNumber $ toRational $ B.length bytes
executeFunction HiFunToUpper [HiValueString text] = return $ HiValueString $ toUpper text
executeFunction HiFunToLower [HiValueString text] = return $ HiValueString $ toLower text
executeFunction HiFunReverse [HiValueString text] = return $ HiValueString $ Data.Text.reverse text
executeFunction HiFunReverse [HiValueList list] = return $ HiValueList $ Data.Sequence.reverse list
executeFunction HiFunReverse [HiValueBytes bytes] = return $ HiValueBytes $ B.reverse bytes
executeFunction HiFunTrim [HiValueString text] = return $ HiValueString $ strip text
executeFunction HiFunList arguments = return $ HiValueList $ Data.Sequence.fromList arguments
executeFunction HiFunRange [HiValueNumber first, HiValueNumber second] = return $ HiValueList $ Data.Sequence.fromList $ Prelude.map HiValueNumber [first .. second]
executeFunction HiFunFold [function, HiValueList Empty] = return HiValueNull 
executeFunction HiFunFold [function, HiValueList (one :<| Empty)] = return one
executeFunction HiFunFold [function, HiValueList (first :<| second :<| arguments)] = do
   begin <- ExceptT $ eval $ HiExprApply (HiExprValue function) [HiExprValue first, HiExprValue second]
   executeFunction HiFunFold [function, HiValueList (begin :<| arguments)]
executeFunction HiFunPackBytes [HiValueList list] = do
  bytes <- TR.traverse mapBytes (Data.Foldable.toList list)
  return $ HiValueBytes $ B.pack bytes
executeFunction HiFunUnpackBytes [HiValueBytes bytes] = return $ HiValueList $ Data.Sequence.fromList $ Prelude.map (HiValueNumber . toRational) (B.unpack bytes)
executeFunction HiFunEncodeUtf8 [HiValueString text] = return $ HiValueBytes $ encodeUtf8 text
executeFunction HiFunDecodeUtf8 [HiValueBytes bytes] = do
  let decoded = decodeUtf8' bytes
  case decoded of
    Right text -> return $ HiValueString text
    _ -> return HiValueNull
executeFunction HiFunZip [HiValueBytes bytes] = return $ HiValueBytes $ LazyB.toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } (LazyB.fromStrict bytes)
executeFunction HiFunUnzip [HiValueBytes bytes] = return $ HiValueBytes $ LazyB.toStrict $ decompress $ LazyB.fromStrict bytes
executeFunction HiFunSerialise [value] = return $ HiValueBytes $ LazyB.toStrict $ serialise value
executeFunction HiFunDeserialise [HiValueBytes bytes] = return $ deserialise $ LazyB.fromStrict bytes
executeFunction HiFunRead [HiValueString path] = return $ HiValueAction $ HiActionRead $ Data.Text.unpack path
executeFunction HiFunWrite [HiValueString path, HiValueString text] = return $ HiValueAction $ HiActionWrite (Data.Text.unpack path) (encodeUtf8 text)
executeFunction HiFunMkDir [HiValueString path] = return $ HiValueAction $ HiActionMkDir $ Data.Text.unpack path
executeFunction HiFunChDir [HiValueString path] = return $ HiValueAction $ HiActionChDir $ Data.Text.unpack path
executeFunction HiFunEcho [HiValueString text] = return $ HiValueAction $ HiActionEcho text
executeFunction HiFunParseTime [HiValueString value] = do 
   let result = readMaybe $ Data.Text.unpack value
   case result of
     Just time -> return $ HiValueTime time
     Nothing -> return HiValueNull
executeFunction HiFunRand [HiValueNumber low, HiValueNumber high] = do
  first <- rationalToInt low
  second <- rationalToInt high
  return $ HiValueAction $ HiActionRand first second
executeFunction HiFunCount [HiValueString text] = return $ counting $ Prelude.map (\character -> HiValueString $ Data.Text.pack [character]) (Data.Text.unpack text)
executeFunction HiFunCount [HiValueList list] = return $ counting $ Data.Foldable.toList list
executeFunction HiFunCount [HiValueBytes bytes] = return $ counting $ Prelude.map (HiValueNumber . toRational) (B.unpack bytes)
executeFunction HiFunKeys [HiValueDict dictionary] = return $ HiValueList $ Data.Sequence.fromList $ keys dictionary
executeFunction HiFunValues [HiValueDict dictionary] = return $ HiValueList $ Data.Sequence.fromList $ elems dictionary 
executeFunction HiFunInvert [HiValueDict dictionary] = return $ HiValueDict $ Data.Map.map HiValueList (Data.Map.fromListWith (<>) (Prelude.map (\(first, second) -> (second, singleton first)) (Data.Map.toList dictionary)))
executeFunction function args =
  if getArgumentsNumber function == Prelude.length args
  then throwE HiErrorInvalidArgument
  else throwE HiErrorArityMismatch

rationalToInt :: HiMonad m => Rational -> ExceptT HiError m Int
rationalToInt rational = do
  integer <- checkInteger rational
  let int = Prelude.SafeEnum.fromEnum integer
  case int of
    Just value -> return value
    Nothing -> throwE HiErrorInvalidArgument

mapBytes :: HiMonad m => HiValue -> ExceptT HiError m W.Word8
mapBytes element =
  case element of
    HiValueNumber number -> return . Prelude.fromInteger . numerator $ number
    _ -> throwE HiErrorInvalidArgument

checkArguments :: HiMonad m => [HiExpr] -> ExceptT HiError m [HiValue]
checkArguments [] = return []
checkArguments (first : rest) = do
  argument <- ExceptT $ eval first
  others <- checkArguments rest
  return (argument : others)

checkPairs :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m [(HiValue, HiValue)]
checkPairs [] = return []
checkPairs ((first, second):rest) = do
  one <- ExceptT $ eval first
  two <- ExceptT $ eval second
  others <- checkPairs rest
  return ((one, two):others)

getIndex :: HiMonad m => HiValue -> Int -> (Int -> Int -> ExceptT HiError m HiValue) -> ExceptT HiError m HiValue
getIndex (HiValueNumber number) length slice = do
  position <- checkInteger number
  let index = fromInteger position
  if index < 0 || index >= length
  then return HiValueNull
  else slice index (index + 1)

getIndex _ _ _ = throwE HiErrorInvalidArgument

getSlice :: HiMonad m => HiValue -> HiValue -> Int -> (Int -> Int -> ExceptT HiError m HiValue) -> ExceptT HiError m HiValue
getSlice start end length slice = do
  from <- processIndex True start length
  to <- processIndex False end length
  slice from to

processIndex :: HiMonad m => Bool -> HiValue -> Int -> ExceptT HiError m Int
processIndex start HiValueNull length
  | start = return 0
  | otherwise = return length
processIndex _ (HiValueNumber number) length = do
  position <- checkInteger number
  let index = fromInteger position
  if index < 0
  then return $ index + length
  else
    if index > length
    then return length
    else return index
processIndex _ _ _ = throwE HiErrorInvalidArgument

sliceString :: HiMonad m => Text -> Int -> Int -> ExceptT HiError m HiValue
sliceString text start end = return $ HiValueString $ Data.Text.take (end - start) (Data.Text.drop start text)

sliceList :: HiMonad m => Seq HiValue -> Int -> Int -> ExceptT HiError m HiValue
sliceList list start end = return $ HiValueList $ Data.Sequence.take (end - start) (Data.Sequence.drop start list)

sliceBytes :: HiMonad m => B.ByteString -> Int -> Int -> ExceptT HiError m HiValue
sliceBytes bytes start end = return $ HiValueBytes $ B.take (end - start) (B.drop start bytes)

slicing :: HiMonad m => Int -> (Int -> Int -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
slicing len slice arguments = do
  converted <- checkArguments arguments
  case converted of
    [number] -> do
      getIndex number len slice
    [first, second] -> do
      getSlice first second len slice
    _ -> do
      throwE HiErrorArityMismatch

secondLazyOr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
secondLazyOr second = do
  two <- ExceptT $ eval second
  case two of
    HiValueBool boolean -> return two
    HiValueNull -> return two
    _ -> throwE HiErrorInvalidArgument

lazyExecute :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
lazyExecute HiFunAnd [first, second] = do
  one <- ExceptT $ eval first
  case one of
    HiValueBool False -> return one
    HiValueNull -> return one
    HiValueBool True -> do
      two <- ExceptT $ eval second
      case two of
        HiValueBool boolean -> return two
        HiValueNull -> return two
        _ -> throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument
lazyExecute HiFunOr [first, second] = do
  one <- ExceptT $ eval first
  case one of
    HiValueBool True -> return one
    HiValueBool False -> secondLazyOr second
    HiValueNull -> secondLazyOr second
    _ -> throwE HiErrorInvalidArgument
lazyExecute HiFunIf [condition, first, second] = do
  boolean <- ExceptT $ eval condition
  case boolean of
    HiValueBool value -> do
      if value
      then ExceptT $ eval first
      else ExceptT $ eval second
    _ -> throwE HiErrorInvalidArgument
lazyExecute _ _ = throwE HiErrorInvalidArgument

checkFunction :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
checkFunction expression arguments = do
  operation <- ExceptT $ eval expression
  case operation of
    HiValueFunction function -> do
      if function `elem` [HiFunOr, HiFunAnd, HiFunIf]
      then lazyExecute function arguments
      else do
        converted <- checkArguments arguments
        executeFunction function converted
    HiValueString text -> do
      let len = Data.Text.length text
      let slice = sliceString text
      slicing len slice arguments
    HiValueList list -> do
      let len = Data.Sequence.length list
      let slice = sliceList list
      slicing len slice arguments
    HiValueBytes bytes -> do
      let len = B.length bytes
      let slice = sliceBytes bytes
      slicing len slice arguments
    HiValueDict dictionary -> do
      converted <- checkArguments arguments
      case converted of
        [key] ->
          if key `member` dictionary
          then return $ dictionary ! key
          else return HiValueNull
        _ -> throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidFunction

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue value) = return (Right value)
eval (HiExprApply expression arguments) = runExceptT $ checkFunction expression arguments
eval (HiExprRun expression) = runExceptT (do
  operation <- ExceptT $ eval expression
  case operation of
    HiValueAction action -> lift $ runAction action
    _ -> throwE HiErrorInvalidArgument
  )
eval (HiExprDict list) = runExceptT $ do
  dictionary <- checkPairs list
  return $ HiValueDict $ Data.Map.fromList dictionary