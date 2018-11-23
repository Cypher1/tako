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


val :: String -> Atom
val = Value . S

var :: String -> Atom
var = Variable . S


newtype Pred = Pred (Map Sym Atom) deriving (Eq, Ord)

toMap :: Pred -> Map Sym Atom
toMap (Pred atoms) = atoms

instance Show Pred where
  show (Pred atoms) = "("++Util.showMap atoms++")"

toPred :: [(String, Atom)] -> Pred
toPred xs = Pred $ M.fromList $ map (\(n, a) -> (S n, a)) xs

-- TODO(jopra): Consider new typing pre vs post conditions to ensure they aren't mixed up
exists :: Atom -> Pred
exists v = toPred [("exists", v)]

type State = Set Pred
emptyState :: State
emptyState = S.empty

type Assignment = Map Sym Atom
-- TODO(jopra): Use hash mapping

emptyAssignment :: Assignment
emptyAssignment = M.empty

-- TODO(jopra): Should return a proper error type, too much work is being done here
-- TODO(jopra): Should check that each value is defined (not just used)
solutions :: State -> State -> [Assignment]
solutions known preds
   = rights $ resolution known (S.toList preds) (Right emptyAssignment)

-- Finds assignments (that are specialisations of the input assignment) for which the Preds are resolvable.
resolution :: State -> [Pred] -> Either () Assignment -> [Either () Assignment]
-- resolution known ps ass | trace ("(K,P,A): "++show (known,ps,ass)) False = undefined
resolution known [] ass = [ass]
resolution known (p:ps) ass
  = [sol | ass' <- assignments_with_p, sol <- resolution known ps ass']
    where
      assignments_with_p = map (restrict ass) $ assignments known p

restrict :: Either () Assignment -> Either () Assignment -> Either () Assignment
restrict xs ys
  = xs >>= M.foldrWithKey restrictOne ys

restrictOne :: Sym -> Atom -> Either () Assignment -> Either () Assignment
restrictOne k v (Right xs)
  = case M.lookup k xs of
      Nothing -> Right $ M.insert k v xs
      Just v' -> case v == v' of
                   True -> Right xs
                   False -> Left ()
restrictOne _ _ err = err

restrictAtoms :: (Atom, Atom) -> Either () Assignment -> Either () Assignment
restrictAtoms _ err@(Left _) = err
restrictAtoms (Value k, Value v) ass
  | k == v = ass
-- | otherwise = trace ("Non-match: "++show (k, v)) $ Left ()
restrictAtoms (Variable k, Value v) ass = restrictOne k (Value v) ass --TODO(handle this for Preds
restrictAtoms (Predicate vs, Predicate xs) ass = restrict (restrictPred vs xs) ass
restrictAtoms (Variable k, Predicate xs) ass = ass''
  where
    ass'' = M.foldrWithKey (\k (var, v)->restrictOne var v) ass (ks' k)
    ks' :: Sym -> Map Sym (Sym, Atom)
    ks' (S k') = M.mapWithKey (\(S k'') ps -> (S(k'++"."++k''), ps)) (toMap xs)
restrictAtoms (k, v) ass = trace ("Unimplemented restrictAtoms for: k:"++show k ++" v:"++ show v) $ Left ()

restrictPred :: Pred -> Pred -> Either () Assignment
restrictPred pred poss
  | M.keysSet (toMap pred) /= M.keysSet (toMap poss) = trace ("Non-matching keys"++show (pred, poss)) $ Left ()
  | otherwise = ass''
  where
    ass'' = foldr (\(k, v) -> restrictAtoms (k, v)) (Right emptyAssignment) $ trace ("unrestricted: "++show ass') ass'
    ass' = M.intersectionWithKey (\k pr po -> (pr, po)) (toMap pred) (toMap poss)

assignments :: State -> Pred -> [Either () Assignment]
assignments state pred
  = map (restrictPred pred) $ S.toList state
