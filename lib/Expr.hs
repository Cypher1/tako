module Expr where

-- import Debug.Trace (trace)

import Prelude hiding (and, not, or)
import Sat
import Util (join)

data Sym = S String -- needs to be more restricted, hashable, etc.
  deriving (Show, Eq, Ord)

data Expr where
  -- Some core concepts need to be built in so that they can be simplified more easy
  -- Any higher level concept should be able to converted to these as they should have full generality... Not sure how to prove that... would be nice to have a full generality proof of them, down to lambda calculus
  T :: Expr
  F :: Expr
  Var :: Bool -> Sym -> Expr
  -- Built in reasoning about boolean relationships
  And :: [Expr] -> Expr
  Or :: [Expr] -> Expr
  -- This allows predicates
  -- Apply :: Expr -> Expr -> Expr
  deriving (Eq, Ord)

sub :: Expr -> Expr -> Expr -> Expr
sub find rep tree | find == tree = rep
sub (Var f_t f_var) rep (Var t_t t_var)
  | f_var == t_var && f_t == t_t = rep
  | f_var == t_var && f_t /= t_t = not rep
sub find rep (And xs) = and $ map (sub find rep) xs
sub find rep (Or xs)  = or  $ map (sub find rep) xs
sub _ _ tree = tree

instance Logic Expr where
  true = T
  false = F
  not T = F
  not F = T
  not (Var True s) = Var False s
  not (Var False s) = Var True s -- This gives us double negation
  -- Possible infinite loop here (De' Morgen's Laws)
  not (And xs) = or $ map not xs
  not (Or xs) = and $ map not xs

  -- Definition of and
  and [] = T
  and (F:_) = F
  and (T:xs) = and xs
  -- Distributive laws
  and ((Or q):p) = or $ map (\x -> and (x:p)) q
  -- Associativity laws / Flattening
  and ((And x):xs) = and (x++xs)
  -- Duplicate variable elimination and check for (a^~a)=F
  and (x:xs)
    | x `elem` xs = and xs
    | (not x) `elem` xs = F
    | otherwise = let and_xs = and xs in case and_xs of
                      F -> F
                      T -> x
                      And xs' -> And (x:xs')
                      _ -> And [x, and_xs]

  -- Definition of or
  or [] = F
  or (T:_) = T
  or (F:xs) = or xs

  -- Distributive laws
  or ((And q):p) = and $ map (\x -> or (x:p)) q

  -- Associativity laws / Flattening
  or ((Or x):xs) = or (x++xs)

  -- Duplicate variable elimination and check for (av~a)=T
  or (x:xs)
    | x `elem` xs = or xs
    | (not x) `elem` xs = T
    | otherwise = let or_xs = or xs in case or_xs of
                      T -> T
                      F -> x
                      Or xs' -> Or (x:xs')
                      _ -> Or [x, or_xs]

var :: String -> Expr
var s = Var True (S s)

instance Show Expr where
  show T = "T"
  show F = "F"
  show (Var f (S x))
    | f = x
    | otherwise = "~"++x
    -- show (Apply f x) = show f++" "++show x
  show (Or xs) = "("++(join "v" xs)++")"
  show (And xs) = "("++(join "^" xs)++")"
