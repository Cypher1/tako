module Pred where
import Debug.Trace (trace)

import Util (showList, showMap)
import Data.Either (rights)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)

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
  show (Predicate atoms) = show atoms

newtype Pred = Pred (Map Sym Atom) deriving (Eq, Ord)

instance Show Pred where
  show (Pred atoms) = "("++Util.showMap atoms++")"

type State = Set Pred
type Assignment = Map Sym Atom
data ResolutionFailure
  = VariableAssignmentContradiction
    { key :: Sym
    , value :: Atom
    , in_ :: Assignment
    }
  | ConcreteMismatch
    { key :: Sym
    , value :: Atom
    , in_ :: Assignment
    }
  | PredicatesOfDifferentShapes
    { requirement :: Pred
    , possible_solution :: Pred
    }
type Resolution = Either ResolutionFailure Assignment

val :: String -> Atom
val = Value . S

var :: String -> Atom
var = Variable . S

toMap :: Pred -> Map Sym Atom
toMap (Pred atoms) = atoms

toPred :: [(String, Atom)] -> Pred
toPred xs = Pred $ M.fromList $ map (\(n, a) -> (S n, a)) xs

-- TODO(jopra): Consider new typing pre vs post conditions to ensure they aren't mixed up
exists :: Atom -> Pred
exists v = toPred [("exists", v)]

emptyState :: State
emptyState = S.empty

emptyAssignment :: Assignment
emptyAssignment = M.empty

-- TODO(jopra): Should return a proper error type, too much work is being done here
-- TODO(jopra): Should check that each value is defined (not just used)
solutions :: State -> State -> [Assignment]
solutions known preds
   = rights $ resolution known (S.toList preds) (Right emptyAssignment)

-- Finds assignments (that are specialisations of the input assignment) for which the Preds are resolvable.
resolution :: State -> [Pred] -> Resolution -> [Resolution]
-- resolution known ps ass | trace ("(K,P,A): "++show (known,ps,ass)) False = undefined
resolution known [] ass = [ass]
resolution known (p:ps) ass
  = [sol | ass' <- assignments_with_p, sol <- resolution known ps ass']
    where
      assignments_with_p = map (restrict ass) $ assignments known p

restrict :: Resolution -> Resolution -> Resolution
restrict xs ys
  = xs >>= M.foldrWithKey restrictOne ys

restrictOne :: Sym -> Atom -> Resolution -> Resolution
restrictOne k v (Right xs)
  = case M.lookup k xs of
      Nothing -> Right $ M.insert k v xs
      Just v' -> case v == v' of
                   True -> Right xs
                   False -> Left $ VariableAssignmentContradiction {key = k, value = v, in_ = xs}
restrictOne _ _ err = err

restrictAtoms :: (Atom, Atom) -> Resolution -> Resolution
restrictAtoms _ err@(Left _) = err
restrictAtoms (Value k, Value v) ass
  = if k == v
       then ass
       else do
         ass' <- ass
         Left $ ConcreteMismatch { key = k, value = Value v, in_ = ass' }
restrictAtoms (Variable k, Value v) ass = restrictOne k (Value v) ass
restrictAtoms (Predicate vs, Predicate xs) ass = restrict (restrictPred vs xs) ass
restrictAtoms (Variable (S k), Predicate xs) ass = do
  let ks = M.mapWithKey (\(S k') ps -> (S(k++"."++k'), ps)) (toMap xs)
  M.foldr (\(var, v)->restrictOne var v) ass ks
restrictAtoms (k, v) ass = error $ "Unimplemented restrictAtoms for: k:"++show k ++" v:"++ show v

restrictPred :: Pred -> Pred -> Resolution
restrictPred pred poss
  | M.keysSet (toMap pred) /= M.keysSet (toMap poss) = Left $
    PredicatesOfDifferentShapes { requirement = pred, possible_solution = poss}
  | otherwise = ass''
  where
    ass'' = foldr (\(k, v) -> restrictAtoms (k, v)) (Right emptyAssignment) ass'
    ass' = M.intersectionWithKey (\k pr po -> (pr, po)) (toMap pred) (toMap poss)

assignments :: State -> Pred -> [Resolution]
assignments state pred
  = map (restrictPred pred) $ S.toList state
