{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module HW3.Base where
import           Control.Applicative (Alternative (..), optional)
import           Control.Monad       (MonadPlus, mfilter)
import Control.Monad.Reader
import Data.Text
import Data.Sequence
import Data.ByteString
import Codec.Serialise
import GHC.Generics
import Control.Exception.Base (Exception, throwIO)
import Data.Text.Encoding (decodeUtf8')
import System.Directory
import Data.Set
import Prelude
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import System.Random (randomRIO)
import Data.Map (Map(..))

data HiFun = 
    HiFunDiv 
  | HiFunMul 
  | HiFunAdd 
  | HiFunSub 
  | HiFunNot 
  | HiFunAnd 
  | HiFunOr 
  | HiFunLessThan 
  | HiFunGreaterThan 
  | HiFunEquals 
  | HiFunNotLessThan 
  | HiFunNotGreaterThan 
  | HiFunNotEquals 
  | HiFunIf 
  | HiFunLength 
  | HiFunToUpper 
  | HiFunToLower 
  | HiFunReverse 
  | HiFunTrim 
  | HiFunList 
  | HiFunRange 
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime 
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert deriving (Show, Eq, Ord, Generic)    -- function names (e.g. div, sort, length, ...)
data HiValue = 
    HiValueNull 
  | HiValueFunction HiFun 
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueString Text
  | HiValueAction HiAction
  | HiValueDict (Map HiValue HiValue)
  | HiValueTime UTCTime
  | HiValueBool Bool 
  | HiValueNumber Rational
 deriving (Show, Eq, Ord, Generic)  -- values (numbers, booleans, strings, ...)
data HiExpr = HiExprValue HiValue | HiExprApply HiExpr [HiExpr] | HiExprRun HiExpr | HiExprDict [(HiExpr, HiExpr)] deriving (Show, Eq)   -- expressions (literals, function calls, ...)
data HiError = HiErrorInvalidArgument | HiErrorInvalidFunction | HiErrorArityMismatch | HiErrorDivideByZero deriving (Show, Eq) -- evaluation errors (invalid arguments, ...)

instance Serialise HiFun
instance Serialise HiValue

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow 
  | HiActionRand Int Int 
  | HiActionEcho Text deriving (Show, Eq, Ord, Generic)

instance Serialise HiAction

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime deriving (Show, Eq, Ord, Bounded, Enum)

newtype PermissionException = PermissionRequired HiPermission deriving (Show, Eq, Ord)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving
                                                               (Functor, Applicative, Monad)
                                                               via (ReaderT (Set HiPermission) IO)

checkPermission :: Set HiPermission -> [HiPermission] -> IO a -> IO a
checkPermission permissionsActual permissionsExpected passed = do
  let diff = difference (Data.Set.fromList permissionsExpected) permissionsActual
  if Data.Set.null diff
  then passed
  else throwIO $ PermissionRequired $ findMin diff

instance HiMonad HIO where
  runAction (HiActionWrite path bytes) = HIO $ \permissions -> checkPermission permissions [AllowWrite] $ HiValueNull <$ Data.ByteString.writeFile path bytes
  runAction (HiActionMkDir path) = HIO $ \permissions -> checkPermission permissions [AllowWrite] $ HiValueNull <$ createDirectory path
  runAction (HiActionChDir path) = HIO $ \permissions -> checkPermission permissions [AllowRead] $ HiValueNull <$ setCurrentDirectory path
  runAction HiActionCwd = HIO $ \permissions -> checkPermission permissions [AllowRead] $ HiValueString . Data.Text.pack <$> getCurrentDirectory
  runAction (HiActionRead path) = HIO $ \permissions -> checkPermission permissions [AllowRead] (do
    file <- doesFileExist path
    if file
    then do
      bytes <- Data.ByteString.readFile path
      let result = decodeUtf8' bytes
      case result of
        Right text -> return $ HiValueString text
        Left _ -> return $ HiValueBytes bytes
    else do
      directory <- doesDirectoryExist path
      if directory
      then do
        files <- listDirectory path
        return $ HiValueList $ Data.Sequence.fromList $ Prelude.map (HiValueString . Data.Text.pack) files
      else return HiValueNull)
  runAction HiActionNow = HIO $ \permissions -> checkPermission permissions [AllowTime] $ HiValueTime <$> getCurrentTime
  runAction (HiActionRand low high) = HIO $ \permissions -> checkPermission permissions [] (do
     rand <- randomRIO (low, high)
     return $ HiValueNumber $ toRational rand)
  runAction (HiActionEcho text) = HIO $ \permissions -> checkPermission permissions [AllowWrite] $ HiValueNull <$ Prelude.putStrLn (Data.Text.unpack text)