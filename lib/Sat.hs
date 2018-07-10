module Sat where

import Prelude hiding (and, or, not)
import qualified Prelude as P

class Logic a where
  not :: a -> a
  and :: [a] -> a
  or  :: [a] -> a
  true :: a
  false :: a

data Sat
  = Sat -- sat/true/1
  | Unproveable -- unknown/?/0
  | Unsat -- unsat/false/-1
  deriving (Show, Eq)

instance Logic Sat where
  true = Sat
  false = Unsat
  not Sat = Unsat
  not Unproveable = Unproveable
  not Unsat = Sat
  and [] = Sat
  and (Unsat:_) = Unsat
  and (Unproveable:x)
    | and x == Unsat = Unsat
    | otherwise = Unproveable
  and (Sat:x) = and x

  or [] = Unsat
  or (Sat:_) = Sat
  or (Unproveable:x)
    | or x == Sat = Sat
    | otherwise = Unproveable
  or (Unsat:x) = or x

implies :: Logic a => a -> a -> a
implies x y = or [not x, y]
