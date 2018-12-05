module Pred where

import Util (showMap, onPair)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)

import Operation (Sym)

data Var
data Val

data Atom a where
  Value :: Sym -> Atom a  -- a particular symbol
  Variable :: Sym -> Atom Var -- a variable that can match any symbol (in its context)
  Predicate :: Pred a -> Atom a -- either something to be solved or known

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

type State = Set (Pred Val)
type Requirements = Set (Pred Var)

data ResolutionFailure
  = VariableNotResolved
    { variable :: Atom Var
    , in_ :: Assignment Val
    }
  | VariableAssignmentContradiction
    { variable :: Atom Var
    , value :: Atom Val
    , in_ :: Assignment Val
    }
  | ConcreteMismatch
    { variable :: Atom Var
    , value :: Atom Val
    , in_ :: Assignment Val
    }
  | VariableVsPredicateMismatch
    { variable :: Atom Var
    , predicate :: Pred Val
    , in_ :: Assignment Val
    }
  | ValueVsPredicateMismatch
    { predicate_match :: Pred Var
    , value :: Atom Val
    , in_ :: Assignment Val
    }
  | PredicatesOfDifferentShapes
    { requirement :: Pred Var
    , possible_solution :: Pred Val
    }
    deriving (Eq, Ord, Show)

data System a
  = Partial a
  | Error ResolutionFailure
  deriving (Eq, Ord, Show)

instance Functor System where
  fmap f (Partial a) = Partial $ f a
  fmap _ (Error e) = Error e

instance Applicative System where
  pure = Partial
  (Partial fs) <*> (Partial xs) = Partial $ fs xs
  (Error fs) <*> _ = Error fs
  _ <*> (Error xs) = Error xs

instance Monad System where
  (Partial xs) >>= f = f xs
  (Error xs) >>= _ = Error xs

type Resolution = System (Assignment Val)

instance Semigroup Resolution where
  xs <> ys = xs >>= M.foldrWithKey restrictOne ys

instance Monoid Resolution where
  mempty :: Resolution
  mempty = pure M.empty


filterErrors :: [Resolution] -> [ResolutionFailure]
filterErrors = foldr next' []
  where
    next' (Partial _) xs' = xs'
    next' (Error err) xs' = err:xs'

ignoreErrors :: [System (Assignment a)] -> [Assignment a]
ignoreErrors = foldr next' []
  where
    next' (Partial ass) xs' = ass:xs'
    next' (Error _) xs' = xs'

val :: String -> Atom a
val = Value

var :: String -> Atom Var
var = Variable

toMap :: Pred a -> Map Sym (Atom a)
toMap (Pred atoms) = M.mapKeysMonotonic show atoms

toPred :: [(String, Atom a)] -> Pred a
toPred xs = Pred $ M.fromList $ map (onPair Variable id) xs

isVar :: Atom Var -> Bool
isVar (Variable _) = True
isVar _ = False

getVarsFrom :: Pred Var -> [Atom Var]
getVarsFrom (Pred vs) = filter isVar $ M.elems vs

requireDefined :: Atom Var -> Resolution -> Resolution
requireDefined _var (Error err) = Error err
requireDefined var' (Partial par)
  | M.lookup var' par == Nothing = Error $ VariableNotResolved var' par
  | otherwise = Partial par

exists :: Atom a -> Pred a
exists v = toPred [("exists", v)]

emptyState :: State
emptyState = S.empty

solutions :: State -> Requirements -> [Assignment Val]
solutions known preds = ignoreErrors $ solutionsAndErrors known preds

-- TODO(jopra): Should check that each value is defined (not just used)
solutionsAndErrors :: State -> Requirements -> [Resolution]
solutionsAndErrors known preds
   = map (\res -> S.foldr requireDefined res vars) $ resolution known (S.toList preds) mempty
     where
       vars = S.fromList $ concatMap getVarsFrom $ S.toList preds

-- Finds assignments (that are specialisations of the input assignment) for which the Preds are resolvable.
resolution :: State -> [Pred Var] -> Resolution -> [Resolution]
resolution known ps ass = foldr (concatMap.partialResolution known) [ass] ps

partialResolution :: State -> Pred Var -> Resolution -> [Resolution]
partialResolution known pred' ass
  = do
  poss <- S.toList known
  return $ ass <> assignmentFromPred pred' poss

restrictOne :: Atom Var -> Atom Val -> Resolution -> Resolution
restrictOne k v (Partial xs)
  = case M.lookup k xs of
      Nothing -> return $ M.insert k v xs
      Just v' -> if v == v'
                    then return xs
                    else Error $ VariableAssignmentContradiction {variable = k, value = v, in_ = xs}
restrictOne _ _ err = err

restrictAtoms :: Atom Var -> Atom Val -> Resolution -> Resolution
restrictAtoms _ _ err@(Error _) = err
restrictAtoms k@(Value k') v@(Value v') ass
  = if k' == v'
       then ass
       else (\ass' -> Error $ ConcreteMismatch { variable = k, value = v, in_ = ass' }) =<< ass
restrictAtoms (Predicate vs) (Predicate xs) ass = assignmentFromPred vs xs <> ass
restrictAtoms k@(Variable _) v ass = restrictOne k v ass
restrictAtoms v@(Value _) (Predicate p) ass
  = (\ass' -> Error $ VariableVsPredicateMismatch {predicate = p, variable = v, in_ = ass'}) =<< ass
restrictAtoms (Predicate p) v@(Value _) ass
  = (\ass' -> Error $ ValueVsPredicateMismatch {predicate_match = p, value = v, in_ = ass'}) =<< ass

assignmentFromPred :: Pred Var -> Pred Val -> Resolution
assignmentFromPred pred' poss
  | M.keysSet (toMap pred') /= M.keysSet (toMap poss) = Error $
    PredicatesOfDifferentShapes { requirement = pred', possible_solution = poss}
  | otherwise = ass''
  where
    ass'' = foldr (uncurry restrictAtoms) mempty ass'
    ass' = M.intersectionWithKey (\_ pr po -> (pr, po)) (toMap pred') (toMap poss)
