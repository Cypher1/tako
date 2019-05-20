{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Pred where

import           Util                           ( prettyMap
                                                , Pretty(pretty)
                                                , prettyMap
                                                )
import qualified Data.Map                      as M
import           Data.Map                       ( Map )

import           PrimOpTypes                    ( Sym )

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
  compare (Value p) (Value q) = compare p q
  compare (Value _) (Variable _) = GT
  compare (Value _) (Predicate _) = GT
  compare (Variable _) (Value _) = LT
  compare (Variable p) (Variable q) = compare p q
  compare (Variable _) (Predicate _) = GT
  compare (Predicate _) (Variable _) = LT
  compare (Predicate _) (Value _) = LT
  compare (Predicate p) (Predicate q) = compare p q

instance Show (Atom a) where
  show (Value s) = "Value "++show s
  show (Variable s) = "Variable "++show s
  show (Predicate pred') = "Predicate " ++ show pred'

instance Pretty (Atom a) where
  pretty (Value s) = s
  pretty (Variable s) = s++"?"
  pretty (Predicate pred') = pretty pred'

type Assignment a = Map (Atom Var) (Atom a)
type Pred a = Assignment a

instance Pretty (Pred a) where
  pretty atoms = name'++"("++atoms'++")"
    where
      rel' = Variable "rel"
      name'= maybe "" pretty $ M.lookup rel' atoms
      atoms'= Util.prettyMap $ M.delete rel' atoms

val :: String -> Atom a
val = Value

var :: String -> Atom Var
var = Variable

isVar :: Atom Var -> Bool
isVar (Variable _) = True
isVar _            = False

getVarsFrom :: Pred Var -> [Atom Var]
getVarsFrom = filter isVar . M.elems
