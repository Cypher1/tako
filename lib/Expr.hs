module Expr where

import Prelude hiding (and, not, or)
import Logic
import Util (join)

-- import Data.List (nub)

type Sym = String

data Term
  = T
  | F
  | Var Sym deriving (Eq, Ord)

instance Show Term where
  show T = "T"
  show F = "F"
  show (Var s) = s

var :: String -> Term
var s = Var s

varS :: String -> AbstractExpr String
varS s = Const (var s)

data AbstractExpr a
  = Const a
  | And [AbstractExpr a]
  | Or [AbstractExpr a]
  | Not (AbstractExpr a)
  deriving (Eq, Ord)

instance Show a => Show (AbstractExpr a) where
  show (Const s) = show s
  show (And xs) = "("++(join "^" xs)++")"
  show (Or xs) = "("++(join "v" xs)++")"
  show (Not xs) = "~"++show xs


type Expr = AbstractExpr Term
type Known = AbstractExpr Bool

instance Not (AbstractExpr a) where
  not_not x = Not x
  not_true = And []
  not_false = Or []

instance And (AbstractExpr a) where
  and_and xs = And xs

instance Or (AbstractExpr a) where
  or_or xs = Or xs
