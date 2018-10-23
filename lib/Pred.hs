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
  deriving (Eq, Ord)

instance Show Atom where
  show (Value s) = show s
  show (Variable s) = "{"++show s++"}"
  show (Predicate atoms) = Util.showList atoms

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
    sols = resolution' known (S.toList preds) emptyAssignment

-- Finds assignments (that are specialisations of the input assignment) for which the Preds are resolvable.
resolution' :: State -> [Pred] -> Assignment -> [Assignment]
resolution' known [] ass = [ass]
resolution' known (p:ps) ass
  = trace ("SOLS:"++show (p, known, assignments_with_p)) [sol | ass' <- assignments_with_p, sol <- resolution' known ps ass']
    where
      assignments_with_p = lefts $ map (restrict ass) $ assignments known p

restrict :: Assignment -> Assignment -> Either Assignment () --TODO(jopra): Report errors?
restrict xs= foldr restrictOne (Left xs)

restrictOne :: (Sym, Sym) -> Either Assignment () -> Either Assignment ()
-- TODO(jopra): remove repetitions
restrictOne (k, v) (Left xs)
  | null vals_for_k = Left ((k, v):xs)
  | all (==v) vals_for_k = Left xs
  where
    vals_for_k = map snd $ filter ((==k).fst) xs
restrictOne _ _ = Right ()

assignments :: State -> Pred -> [Assignment]
assignments state pred
  | not(hasVariable pred) && pred `elem` state && containsAll state pred = [emptyAssignment]-- Check existence
  | otherwise = [] -- TODO(jopra): Perform substitution

hasVariable :: Pred -> Bool
hasVariable = isVariable . Predicate

isVariable :: Atom -> Bool
isVariable (Variable _) = True
isVariable (Value _) = False
isVariable (Predicate atoms) = any isVariable atoms

-- Checks that the atom is defined in the current state.
contains :: State -> Atom -> Bool
contains state (Predicate p) = containsAll state p
contains state (Value s) = [Value s] `elem` state
contains state (Variable s) = False -- TODO(jopra): Don't require assignment?

-- Checks that all atoms in a predicate are defined in the current state.
containsAll :: State -> Pred -> Bool
containsAll = all.contains
