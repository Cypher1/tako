module Main where
import Prelude hiding (not, and, or)

import Triple
import Expr
-- import Util

-- import qualified Data.Map.Lazy as M

main :: IO ()
main = do
  let zero = val "0"
  let ret = var "ret"
  let a = var "a"
  let b = var "b"
  let ne = var "!="
  let div = func "/" (I (Div a b ret)) [exists a, exists b] [creates ret]
  print div
  let minus = func "-" (I (Sub a b ret)) [exists a, exists b] [creates ret]
  print minus
  let frac = add_pre [a, ne, zero] minus
  print frac
