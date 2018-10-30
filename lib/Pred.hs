module Pred where
import Debug.Trace (trace)

import Util (showList)
import Data.Either (lefts)
import qualified Data.Set as S
import Data.Set (Set)

import Data.List ((\\))
import Operation (Sym (S), Instruction, Op)

data Atom
  = Value Sym -- a particular symbol
  | Variable Sym -- a variable that can match any symbol (in its context)
  | Predicate Pred
  | Pattern Pred -- a variable predicate
  deriving (Eq, Ord)

instance Show Atom where
  show (Value s) = show s
  show (Variable s) = "{"++show s++"}"
  show (Predicate atoms) = Util.showList atoms
  show (Pattern atoms) = "{"++Util.showList atoms++"}"

type Pred = [Atom]

-- TODO(jopra): Consider new typing pre vs post conditions to ensure they aren't mixed up
exists :: Sym -> Pred
exists v = [Value v]

creates :: Sym -> Pred
creates v = [Value v]

type State = Set Pred
emptyState :: State
emptyState = S.empty

type Assignment = [(Sym, Sym)]
-- TODO(jopra): Use hash mapping

emptyAssignment :: Assignment
emptyAssignment = []

-- TODO(jopra): Should return a proper error type, too much work is being done here
-- TODO(jopra): Should check that each value is defined (not just used)
solutions :: State -> State -> [Assignment]
solutions known preds = trace (show sols) sols
  where
    sols = resolution known (S.toList preds) emptyAssignment

-- Finds assignments (that are specialisations of the input assignment) for which the Preds are resolvable.
resolution :: State -> [Pred] -> Assignment -> [Assignment]
resolution known [] ass = [ass]
resolution known (p:ps) ass
  = trace ("SOLS:"++show (p, known, assignments_with_p)) [sol | ass' <- assignments_with_p, sol <- resolution known ps ass']
    where
      assignments_with_p = lefts $ map (restrict ass) $ assignments known p

restrict :: Assignment -> Assignment -> Either Assignment () --TODO(jopra): Report errors?
restrict xs ys
  | trace ("restrict: "++show xs++", "++show ys) True
  = foldr (try restrictOne) (Left xs) ys

restrictOne :: (Sym, Sym) -> Assignment -> Either Assignment ()
-- TODO(jopra): remove repetitions
restrictOne (k, v) xs
  | null v' = Left ((k, v):xs)
  | all (==v) v' = Left xs
  | otherwise = Right ()
  where
    v' = map snd $ filter ((==k).fst) xs

restrictAtoms :: (Atom, Atom) -> Assignment -> Either Assignment ()
restrictAtoms (Value k, Value v) ass
  | k == v = Left ass
restrictAtoms (Variable k, Value v) ass = restrictOne (k, v) ass
restrictAtoms (Pattern vs, Predicate xs) ass = try restrict ass (restrictPred vs xs)
restrictAtoms (k, v) ass = trace ("Unimplemented restrictAtoms for: "++show (k, v, ass)) $ Right ()

try :: (a -> b -> Either c d) -> a -> Either b d -> Either c d
try f a (Left b) = f a b
try f a (Right d) = Right d

restrictPred :: Pred -> Pred -> Either Assignment ()
restrictPred pred poss
  | length pred /= length poss = Right ()
  | otherwise = foldr (try restrictAtoms) (Left []) $ zip pred poss

assignments :: State -> Pred -> [Assignment]
assignments state pred = trace (">>"++show (pred, state, poss)) poss
    where
      poss = lefts $ map (restrictPred pred) $ S.toList state

hasVariable :: Pred -> Bool
hasVariable = isVariable . Predicate

isVariable :: Atom -> Bool
isVariable (Value _) = False
isVariable (Variable _) = True
isVariable (Predicate atoms) = any isVariable atoms
isVariable (Pattern atoms) = any isVariable atoms

-- Checks that the atom is defined in the current state.
contains :: State -> Atom -> Bool
contains state (Predicate p) = containsAll state p
contains state (Value s) = [Value s] `elem` state
contains state (Variable s) = False -- TODO(jopra): Don't require assignment?
-- TODO(jopra) Patterns

-- Checks that all atoms in a predicate are defined in the current state.
containsAll :: State -> Pred -> Bool
containsAll = all.contains
