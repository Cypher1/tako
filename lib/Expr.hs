module Expr where

-- import Debug.Trace (trace)

import Prelude hiding (and, not, or)
import Logic
import Util (join)

data Sym = S String -- needs to be more restricted, hashable, etc.
  deriving (Show, Eq, Ord)

data Disjunction a
  = Or [a] deriving (Eq, Ord)

instance Show a => Show (Disjunction a) where
  show (Or xs) = "("++(join "v" xs)++")"

{-
instance (Not a, Eq a) => Or (Disjunction a) where
  or_or ors = Or $ filter (/=not_false) $ concatMap (\(Or xs)->xs) ors

instance (Not a, And a, Eq a) => And (Disjunction a) where
  and_and xs' = foldr and_each (Or [not_false]) xs'
    where
      and_each :: Disjunction a -> Disjunction a -> Disjunction a
      and_each (Or xs) (Or rs)
        = Or $ filter (/=not_true) [and_and [x,and_r]|x<-xs,and_r<-rs]

instance (Not a, And a, Eq a) => Not (Disjunction a) where
  not_not (Or xs) = Or [and_and $ map not_not xs]
  not_true = Or [not_true]
  not_false = Or []
-}

data Conjunction a
  = And [a] deriving (Eq, Ord)

instance Show a => Show (Conjunction a) where
  show (And xs) = "("++(join "^" xs)++")"

{-
instance (Not a, And a, Eq a) => And (Conjunction a) where
  and_and ands = And $ filter (/=not_true) $ concatMap (\(And xs)->xs) ands

instance (Not a, Or a, Eq a) => Or (Conjunction a) where
  or_or xs' = foldr or_each (And [not_true]) xs'
    where
      or_each :: Conjunction a -> Conjunction a -> Conjunction a
      or_each (And xs) (And rs)
        = And $ filter (/=not_true) [or_or [x,or_r]|x<-xs,or_r<-rs]

instance (Not a, Or a) => Not (Conjunction a) where
  not_not (And xs) = And [or_or $ map not_not xs]
  not_true = And [not_true]
  not_false = And [not_false]

  -- show (Apply f x) = show f++" "++show x
-}

data Term
  = T
  | F
  | Var Bool Sym deriving (Eq, Ord)

var :: String -> Term
var s = Var True (S s)

instance Show Term where
  show T = "T"
  show F = "F"
  show (Var f (S x))
    | f = x
    | otherwise = "~"++x

instance Not Term where
  not_true = T
  not_false = F
  not_not T = F
  not_not F = T
  not_not (Var x s) = Var (not_not x) s -- This gives us double negation


type CNF = Conjunction (Disjunction Term)

type DNF = Disjunction (Conjunction Term)
instance Not DNF where
  not_true = Or [And [not_true]]
  not_false = Or []
  not_not (Or xs) = Or $ foldr distribute [And []] $ map (\(And ys)->And (map not_not ys)) xs

instance Or DNF where
  or_or xs = Or $ concatMap (\(Or x)->x) xs -- can sort, uniq and filter this

instance And DNF where
  and_and [] = Or [And []]
  and_and ((Or ys):xs) = Or [And (filter (/=not_true) (y++x))|(And y)<-ys,(And x)<-xs']
    where
      Or xs' = and_and xs

varS :: String -> DNF
varS s = Or [And [var s]]

type Expr = CNF

distribute :: Conjunction Term -> [Conjunction Term] -> [Conjunction Term]
distribute (And ts) ands = concatMap (addTermToAll ands) ts

addTermToAll :: [Conjunction a] -> a -> [Conjunction a]
addTermToAll ands t = map (`addRequirement` t) ands
addRequirement :: Conjunction a -> a -> Conjunction a
addRequirement (And ys) t = And (t:ys)
