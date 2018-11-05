module Pred where
import Debug.Trace (trace)

import Util (showList, try)
import Data.Either (rights)
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
  show (Predicate atoms) = "("++Util.showList atoms++")"

type Pred = [Atom]

-- TODO(jopra): Consider new typing pre vs post conditions to ensure they aren't mixed up
exists :: Sym -> Pred
exists v = [Value v]

creates :: Sym -> Pred
creates v = [Value v]

type State = Set Pred
emptyState :: State
emptyState = S.empty

type Assignment = [(Sym, Atom)]
-- TODO(jopra): Use hash mapping

emptyAssignment :: Assignment
emptyAssignment = []

-- TODO(jopra): Should return a proper error type, too much work is being done here
-- TODO(jopra): Should check that each value is defined (not just used)
solutions :: State -> State -> [Assignment]
solutions known preds
   = resolution known (S.toList preds) emptyAssignment

-- Finds assignments (that are specialisations of the input assignment) for which the Preds are resolvable.
resolution :: State -> [Pred] -> Assignment -> [Assignment]
resolution known [] ass = [ass]
resolution known (p:ps) ass
  = [sol | ass' <- assignments_with_p, sol <- resolution known ps ass']
    where
      assignments_with_p = rights $ map (restrict ass) $ assignments known p

restrict :: Assignment -> Assignment -> Either () Assignment --TODO(jopra): Report errors?
restrict xs
  = foldr (try restrictOne) (Right xs)

restrictOne :: (Sym, Atom) -> Assignment -> Either () Assignment
-- TODO(jopra): remove repetitions
restrictOne (k, v) xs
  | null v' = Right ((k, v):xs)
  | all (==v) v' = Right xs
  | otherwise = Left ()
  where
    v' = map snd $ filter ((==k).fst) xs

restrictAtoms :: (Atom, Atom) -> Assignment -> Either () Assignment
restrictAtoms (Value k, Value v) ass
  | k == v = Right ass
  | otherwise = Left ()
restrictAtoms (Variable k, Value v) ass = restrictOne (k, Value v) ass --TODO(handle this for Preds
restrictAtoms (Predicate vs, Predicate xs) ass = try restrict ass (restrictPred vs xs)
restrictAtoms (Variable k, Predicate xs) ass = restrictAtoms (Predicate (ks' k), Predicate xs) ass
  where
    ks' (S k')= [Variable $ S(k'++show i)|i<-[0..length xs-1]]
restrictAtoms (k, v) ass = trace ("Unimplemented restrictAtoms for: k:"++show k ++" v:"++ show v) $ Left ()

restrictPred :: Pred -> Pred -> Either () Assignment
restrictPred pred poss
  | length pred /= length poss = Left ()
  | otherwise = foldr (try restrictAtoms) (Right []) $ zip pred poss

assignments :: State -> Pred -> [Assignment]
assignments state pred
  = rights $ map (restrictPred pred) $ S.toList state
