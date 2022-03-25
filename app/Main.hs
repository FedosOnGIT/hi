module Main where

import System.Console.Haskeline
import Data.Set
import Control.Monad.Cont
import Text.Megaparsec.Error (errorBundlePretty)
import HW3.Base
import HW3.Parser
import HW3.Pretty
import HW3.Evaluator

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case parse input of
            Left err -> outputStrLn $ errorBundlePretty err
            Right expr -> do
              evaluated <- liftIO $ runHIO (eval expr) $ insert AllowRead $ insert AllowWrite $ insert AllowTime empty
              case evaluated of
                Left err -> outputStrLn $ show err
                Right res -> outputStrLn $ show $ prettyValue res
          loop
