module Resolution where

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M

import Pred (Pred (Pred), Val, Var, Atom(Value, Variable, Predicate), Assignment, getVarsFrom)


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

requireDefined :: Atom Var -> Resolution -> Resolution
requireDefined _var (Error err) = Error err
requireDefined var' (Partial par)
  | M.lookup var' par == Nothing = Error $ VariableNotResolved var' par
  | otherwise = Partial par

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
assignmentFromPred (Pred pred') (Pred poss)
  | M.keysSet pred' /= M.keysSet poss = Error $
    PredicatesOfDifferentShapes { requirement = Pred pred', possible_solution = Pred poss}
  | otherwise = ass''
  where
    ass'' = foldr (uncurry restrictAtoms) mempty ass'
    ass' = M.intersectionWithKey (\_ pr po -> (pr, po)) pred' poss
