module Main where
import Prelude
import System.Console.Haskeline

import Text.Read (readMaybe)
import Data.Either (isLeft, isRight)
import Util (showList)
import Triple
import Expr
import Operation
import qualified Data.Set as S

main :: IO ()
main = runInputT defaultSettings (mainLoop [])

mainLoop :: Mem -> InputT IO ()
mainLoop mem
  = do
    outputStrLn $ show mem
    minput <- getInputLine "% "
    case minput of
      Nothing      -> return ()
      Just "exit"  -> return ()
      Just ""      -> mainLoop mem
      Just cmd -> do
        mem' <- handleCommand cmd mem
        mainLoop mem'

handleCommand :: String -> Mem -> InputT IO Mem
handleCommand cmd mem
  = case readMaybe cmd of
      Just ins -> return $ exec ins mem
      Nothing -> do
        outputStrLn $ "Sorry '"++cmd++"' couldn't be parsed."
        return mem
