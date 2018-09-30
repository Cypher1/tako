module Main where
import Prelude hiding (showList)
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
mainLoop mem = do
    minput <- getInputLine "% "
    case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just command ->handleCommand command mem

handleCommand :: String -> Mem -> InputT IO ()
handleCommand command mem
 = case (readMaybe command)::Maybe Instruction of
     Just instruction -> do
       let mem' = exec instruction mem
       outputStrLn $ show mem'
       mainLoop mem'
     Nothing -> do
       outputStrLn $ "Sorry '"++command++"' couldn't be parsed."
       mainLoop mem
