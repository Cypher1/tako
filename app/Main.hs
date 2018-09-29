module Main where
import Prelude hiding (showList)

import Text.Read (readMaybe)
import Data.Either (isLeft, isRight)
import Util (showList)
import Triple
import Expr
import Operation
import qualified Data.Set as S

main :: IO ()
main = mainLoop []

mainLoop :: Mem -> IO ()
mainLoop mem = do
  print $ mem
  command <- getLine
  case (readMaybe command)::Maybe Instruction of
    Just instruction -> do
      print instruction
      mainLoop $ exec instruction mem
    Nothing -> do
      print $ "Sorry '"++command++"' couldn't be parsed."
      mainLoop mem
