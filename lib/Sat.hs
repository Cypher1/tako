module Sat where

import Logic
import Prelude hiding (not, and, or)

data Sat
  = Sat -- sat/true/1
  | Unproveable -- unknown/?/0
  | Unsat -- unsat/false/-1
  deriving (Show, Eq)

instance Not Sat where
  not_true = Sat
  not_false = Unsat
  not_not Sat = Unsat
  not_not Unproveable = Unproveable
  not_not Unsat = Sat

instance And Sat where
  and_and [] = Sat
  and_and (Unsat:_) = Unsat
  and_and (Unproveable:x)
    | and x == Unsat = Unsat
    | otherwise = Unproveable
  and_and (Sat:x) = and_and x

instance Or Sat where
  or_or [] = Unsat
  or_or (Sat:_) = Sat
  or_or (Unproveable:x)
    | or_or x == Sat = Sat
    | otherwise = Unproveable
  or_or (Unsat:x) = or_or x

implies :: (Logic a) => a -> a -> a
implies x y = or [not x, y]
