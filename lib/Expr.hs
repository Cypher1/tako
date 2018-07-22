module Expr where

import Prelude hiding (and, not, or)
import Logic
import Util (join)

import Data.List (nub)

data Sym = S String -- needs to be more restricted, hashable, etc.
  deriving (Show, Eq, Ord)

data Disjunction a
  = Or [a] deriving (Eq, Ord)

instance Show a => Show (Disjunction a) where
  show (Or xs) = "("++(join "v" xs)++")"

data Conjunction a
  = And [a] deriving (Eq, Ord)

instance Show a => Show (Conjunction a) where
  show (And xs) = "("++(join "^" xs)++")"

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
  not_true = Or [And []]
  not_false = Or []
  not_not (Or xs) = Or $ foldr distribute [And []] $ map (\(And ys)->And (map not_not ys)) xs

instance Or DNF where
  or_or xs = Or $ concatMap (\(Or x)->x) xs -- can sort, uniq and filter this

instance And DNF where
  and_and [] = not_true
  and_and ((Or ys):xs) = simplyDNF $ Or [And (x++y) |(And y)<-ys,(And x)<-xs']
    where
      Or xs' = and_and xs

simplyDNF :: DNF -> DNF
simplyDNF (Or xs')
  | (And []) `elem` xs = Or [And []]
  -- | any (\x->((not_not x)`elem`xs)) xs = Or []
  | otherwise = Or xs
  where
    xs = nub $ filter (/=(And [F])) $ map simplyAnd xs'

simplyAnd :: Conjunction Term -> Conjunction Term
simplyAnd (And xs')
  | F `elem` xs = And [F]
  | any (\x->((not_not x)`elem`xs)) xs = And [F]
  | otherwise = And xs
  where
    xs = nub $ filter (/=not_true) xs'

varS :: String -> DNF
varS s = Or [And [var s]]

type Expr = CNF

distribute :: Conjunction Term -> [Conjunction Term] -> [Conjunction Term]
distribute (And ts) ands = concatMap (addTermToAll ands) ts

addTermToAll :: [Conjunction a] -> a -> [Conjunction a]
addTermToAll ands t = map (`addRequirement` t) ands
addRequirement :: Conjunction a -> a -> Conjunction a
addRequirement (And ys) t = And (t:ys)
