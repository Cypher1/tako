module Pred where

import Util (showMap)
import qualified Data.Map as M
import Data.Map (Map)

import Operation (Sym)

data Var
data Val

data Atom a where
  Value :: Sym -> Atom a  -- a particular symbol
  Variable :: Sym -> Atom Var -- a variable that can match any symbol (in its context)
  Predicate :: Pred a -> Atom a -- either something to be solved or known

-- TODO(jopra): Represent Forall and Exists as:
-- Forall :: Atom Var -> Atom Var -> Atom Val
-- Exists :: Atom Var -> Atom Var -> Atom Val
-- This should allow Htriples to be specified over Atom Val's and enable quantification for the type system.

instance Eq (Atom Val) where
  Value p == Value q = p == q
  Predicate xs == Predicate ys = xs == ys
  _ == _ = False

instance Eq (Atom Var) where
  Variable p == Variable q = p == q
  Value p == Value q = p == q
  Predicate xs == Predicate ys = xs == ys
  _ == _ = False

instance Ord (Atom Val) where
  compare (Value p) (Value q) = compare p q
  compare (Value _) (Predicate _) = GT
  compare (Predicate _) (Value _) = LT
  compare (Predicate p) (Predicate q) = compare p q


instance Ord (Atom Var) where
  compare (Variable p) (Variable q) = compare p q
  compare (Value _) (Variable _) = GT
  compare (Variable _) (Value _) = LT
  compare (Variable _) (Predicate _) = GT
  compare (Predicate _) (Variable _) = LT
  compare (Value p) (Value q) = compare p q
  compare (Value _) (Predicate _) = GT
  compare (Predicate _) (Value _) = LT
  compare (Predicate p) (Predicate q) = compare p q

instance Show (Atom a) where
  show (Value s) = s
  show (Variable s) = s++"?"
  show (Predicate atoms) = show atoms

type Assignment a = Map (Atom Var) (Atom a)
newtype Pred a = Pred (Assignment a)

instance Eq (Pred Val) where
  Pred xs == Pred ys = xs == ys

instance Eq (Pred Var) where
  Pred xs == Pred ys = xs == ys

instance Ord (Pred Val) where
  compare (Pred xs) (Pred ys) = compare xs ys

instance Ord (Pred Var) where
  compare (Pred xs) (Pred ys) = compare xs ys


instance Show (Pred a) where
  show (Pred atoms) = "("++Util.showMap atoms++")"

val :: String -> Atom a
val = Value

var :: String -> Atom Var
var = Variable

isVar :: Atom Var -> Bool
isVar (Variable _) = True
isVar _ = False

getVarsFrom :: Pred Var -> [Atom Var]
getVarsFrom (Pred vs) = filter isVar $ M.elems vs
