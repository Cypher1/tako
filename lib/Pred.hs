module Pred where

import Util (showMap, showSet)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)

import Operation (Sym)

data Var
data Val

data Atom a where
  Value :: Sym -> Atom a  -- a particular symbol
  Variable :: Sym -> Atom Var -- a variable that can match any symbol (in its context)
  Predicate :: Pred a -> Atom a -- either something to be solved or known
  Rule :: Set (Pred Var) -> Set (Pred Var) -> Atom a -- A sequent

-- TODO(jopra): Represent Forall and Exists as:
-- Forall :: Atom Var -> Atom Var -> Atom Val
-- Exists :: Atom Var -> Atom Var -> Atom Val
-- This should allow Htriples to be specified over Atom Val's and enable quantification for the type system.

instance Eq (Atom Val) where
  Value p == Value q = p == q
  Predicate xs == Predicate ys = xs == ys
  Rule conds1 outs1 == Rule conds2 outs2 = (conds1, outs1) == (conds2, outs2)
  _ == _ = False

instance Eq (Atom Var) where
  Variable p == Variable q = p == q
  Value p == Value q = p == q
  Predicate xs == Predicate ys = xs == ys
  Rule conds1 outs1 == Rule conds2 outs2 = (conds1, outs1) == (conds2, outs2)
  _ == _ = False

instance Ord (Atom Val) where
  compare (Value p) (Value q) = compare p q
  compare (Value _) (Predicate _) = GT
  compare (Value _) (Rule _ _) = GT
  compare (Predicate _) (Value _) = LT
  compare (Predicate p) (Predicate q) = compare p q
  compare (Predicate _) (Rule _ _) = GT
  compare (Rule _ _) (Value _) = LT
  compare (Rule _ _) (Predicate _) = LT
  compare (Rule conds1 outs1) (Rule conds2 outs2) = compare (conds1, outs1) (conds2, outs2)


instance Ord (Atom Var) where
  compare (Value p) (Value q) = compare p q
  compare (Value _) (Variable _) = GT
  compare (Value _) (Predicate _) = GT
  compare (Value _) (Rule _ _) = GT
  compare (Variable _) (Value _) = LT
  compare (Variable p) (Variable q) = compare p q
  compare (Variable _) (Predicate _) = GT
  compare (Variable _) (Rule _ _) = GT
  compare (Predicate _) (Variable _) = LT
  compare (Predicate _) (Value _) = LT
  compare (Predicate p) (Predicate q) = compare p q
  compare (Predicate _) (Rule _ _) = GT
  compare (Rule _ _) (Variable _) = LT
  compare (Rule _ _) (Value _) = LT
  compare (Rule _ _) (Predicate _) = LT
  compare (Rule conds1 outs1) (Rule conds2 outs2) = compare (conds1, outs1) (conds2, outs2)

instance Show (Atom a) where
  show (Value s) = s
  show (Variable s) = s++"?"
  show (Predicate pred') = show pred'
  show (Rule conds outs) = showSet conds++" |- "++show outs

type Assignment a = Map (Atom Var) (Atom a)
data Pred a = Pred (Assignment a)

instance Eq (Pred Val) where
  Pred xs == Pred ys = xs == ys

instance Eq (Pred Var) where
  Pred xs == Pred ys = xs == ys

instance Ord (Pred Val) where
  compare (Pred xs) (Pred ys) = compare xs ys

instance Ord (Pred Var) where
  compare (Pred xs) (Pred ys) = compare xs ys

instance Show (Pred a) where
  show (Pred atoms) = case M.lookup (Variable "rel") atoms of
                        Just name -> show name++"("++Util.showMap atoms++")" -- TODO(jopra) hide name from here.
                        Nothing -> "_("++Util.showMap atoms++")"

val :: String -> Atom a
val = Value

var :: String -> Atom Var
var = Variable

isVar :: Atom Var -> Bool
isVar (Variable _) = True
isVar _ = False

getVarsFrom :: Pred Var -> [Atom Var]
getVarsFrom (Pred vs) = filter isVar $ M.elems vs
