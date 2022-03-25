module HW3.Pretty where

import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Internal (Doc, pretty, (<+>), viaShow)
import Data.Ratio (Rational, numerator, denominator)
import           Control.Monad.Trans.Except
import Data.Foldable (toList)
import Data.Sequence (Seq(..))
import Data.Scientific
import Data.ByteString (ByteString(..), unpack)
import Numeric (showHex)
import Data.Map (toList)
import HW3.Base
import HW3.Parser
import HW3.Evaluator

prettyNumber :: Rational -> (Scientific, Maybe Int) -> Doc AnsiStyle
prettyNumber rational (number, Nothing) = do
  if denominator rational == 1
  then pretty (numerator rational)
  else pretty (formatScientific Fixed Nothing number)
prettyNumber rational (_, Just _) = do
  let den = denominator rational
  let (quot, rem) = quotRem (numerator rational) den
  let fraction = pretty (abs rem) <> pretty "/" <> pretty den
  if quot == 0
  then
    if numerator rational < 0
    then pretty "-" <> fraction
    else fraction
  else 
    if numerator rational < 0
    then pretty quot <+> pretty "-" <+> fraction
    else pretty quot <+> pretty "+" <+> fraction

prettyByte :: (Integral a, Show a) => a -> String
prettyByte i = case i `showHex` "" of
  [byte] -> ['0', byte]
  bytes   -> bytes
  
prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes bytes = do
  let (first:rest) = fmap prettyByte (Data.ByteString.unpack bytes)
  pretty "[#" <+> foldl (\accumulator next -> accumulator <+> pretty next) (pretty first) rest <+> pretty "#]"

prettyArguments :: [Doc AnsiStyle] -> Doc AnsiStyle
prettyArguments (first:rest) = pretty "(" <> foldl (\accumulator next -> accumulator <> pretty "," <+> next) first rest <> pretty ")"

prettyPair :: (HiValue, HiValue) -> Doc AnsiStyle
prettyPair (first, second) = prettyValue first <> pretty ":" <+> prettyValue second

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber number) = prettyNumber number (fromRationalRepetendUnlimited number)
prettyValue (HiValueFunction function) = case function of
  HiFunDiv -> pretty "div"
  HiFunMul -> pretty "mul"
  HiFunAdd -> pretty "add"
  HiFunSub -> pretty "sub"
  HiFunNot -> pretty "not"
  HiFunAnd -> pretty "and"
  HiFunOr -> pretty "or"
  HiFunLessThan -> pretty "less-than"
  HiFunGreaterThan -> pretty "greater-than"
  HiFunEquals -> pretty "equals"
  HiFunNotLessThan -> pretty "not-less-than"
  HiFunNotGreaterThan -> pretty "not-greater-than"
  HiFunNotEquals -> pretty "not-equals"
  HiFunIf -> pretty "if"
  HiFunLength -> pretty "length"
  HiFunToUpper -> pretty "to-upper"
  HiFunToLower -> pretty "to-lower"
  HiFunReverse -> pretty "reverse"
  HiFunTrim -> pretty "trim"
  HiFunList -> pretty "list"
  HiFunRange -> pretty "range"
  HiFunFold -> pretty "fold"
  HiFunPackBytes -> pretty "pack-bytes"
  HiFunUnpackBytes -> pretty "unpack-bytes"
  HiFunEncodeUtf8 -> pretty "encode-utf8"
  HiFunDecodeUtf8 -> pretty "decode-utf8"
  HiFunZip -> pretty "zip"
  HiFunUnzip -> pretty "unzip"
  HiFunSerialise -> pretty "serialise"
  HiFunDeserialise -> pretty "deserialise"
  HiFunRead -> pretty "read"
  HiFunWrite -> pretty "write"
  HiFunMkDir -> pretty "mkdir"
  HiFunChDir -> pretty "cd"
prettyValue (HiValueBool boolean) = 
  if boolean
  then pretty "true"
  else pretty "false"
prettyValue (HiValueString text) = viaShow text
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList Empty) = pretty "[" <+> pretty "]"
prettyValue (HiValueList (first :<| rest)) = pretty "[" <+> foldl (\accumulator next -> accumulator <> pretty "," <+> prettyValue next) (prettyValue first) (Data.Foldable.toList rest) <+> pretty "]"
prettyValue (HiValueBytes bytes) = prettyBytes bytes
prettyValue (HiValueAction action) = case action of
  HiActionRead path -> pretty "read" <> prettyArguments [pretty path]
  HiActionWrite path bytes -> pretty "write" <> prettyArguments [pretty path, prettyBytes bytes]
  HiActionMkDir path -> pretty "mkdir" <> prettyArguments [pretty path]
  HiActionChDir path -> pretty "cd" <> prettyArguments [pretty path]
  HiActionCwd -> pretty "cwd"
  HiActionNow -> pretty "now"
  HiActionRand low high -> pretty "rand" <> prettyArguments [pretty low, pretty high]
  HiActionEcho text -> pretty "echo" <> prettyArguments [pretty text]
prettyValue (HiValueTime time) = pretty $ show time
prettyValue (HiValueDict dictionary) =
  let pairs = Data.Map.toList dictionary in 
    case pairs of
      [] -> pretty "{" <+> pretty "}"     
      first:rest -> pretty "{" <+> foldl (\accumulator next -> accumulator <> pretty "," <+> prettyPair next) (prettyPair first) rest <+> pretty "}"