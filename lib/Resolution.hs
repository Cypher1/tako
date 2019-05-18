{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Resolution where

import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isNothing )

import           Pred                           ( Pred
                                                , Assignment
                                                , Val
                                                , Var
                                                , Atom
                                                  ( Value
                                                  , Variable
                                                  , Predicate
                                                  )
                                                , getVarsFrom
                                                )


type State = Set (Pred Val)
type Requirements = Set (Pred Var)
type Resolution = System (Assignment Val)

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
    , in_ :: Assignment Val
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

instance Semigroup Resolution where
  xs <> ys = xs >>= M.foldrWithKey (\k v -> (=<<) (k `mgu` v)) ys

instance Monoid Resolution where
  mempty = pure M.empty


filterErrors :: [Resolution] -> [ResolutionFailure]
filterErrors = foldr next' []
 where
  next' (Partial _  ) xs' = xs'
  next' (Error   err) xs' = err : xs'

ignoreErrors :: [System (Assignment a)] -> [Assignment a]
ignoreErrors = foldr next' []
 where
  next' (Partial ass) xs' = ass : xs'
  next' (Error   _  ) xs' = xs'

requireDefined :: Atom Var -> Resolution -> Resolution
requireDefined var' (Partial par)
  | isNothing (M.lookup var' par) = Error $ VariableNotResolved var' par
  | otherwise                     = Partial par
requireDefined _ err = err

solutions :: State -> Requirements -> [Assignment Val]
solutions known preds = ignoreErrors $ solutionsAndErrors known preds

solutionsAndErrors :: State -> Requirements -> [Resolution]
solutionsAndErrors known preds =
  map (\res -> S.foldr requireDefined res vars)
    $ resolution known (S.toList preds) mempty
  where vars = S.fromList $ concatMap getVarsFrom $ S.toList preds

-- Finds assignments (that are specialisations of the input assignment) for which the Preds are resolvable.
resolution :: State -> [Pred Var] -> Resolution -> [Resolution]
resolution state ps res = foldr (fanout $ S.toList state) [res] ps
 where
  fanout :: [Pred Val] -> Pred Var -> [Resolution] -> [Resolution]
  fanout state' pred' res' =
    concatMap (\poss -> (predToAssignment pred' poss =<<) <$> res') state'

mgu :: Atom Var -> Atom Val -> Assignment Val -> Resolution
mgu k@(Value k') v@(Value v') =
  if k' == v' then return else Error <$> ConcreteMismatch k v
mgu (  Predicate vs) (  Predicate xs) = predToAssignment vs xs
mgu v@(Value _) (Predicate p) = Error <$> VariableVsPredicateMismatch v p
mgu (  Predicate p ) v@(Value     _ ) = Error <$> ValueVsPredicateMismatch p v
mgu k@(Variable  _ ) v                = resolve
 where
  resolve ass = case M.lookup k ass of
    Nothing -> return $ M.insert k v ass
    Just v' -> if v == v'
      then return ass
      else Error $ VariableAssignmentContradiction k v ass

predToAssignment :: Pred Var -> Pred Val -> Assignment Val -> Resolution
predToAssignment pred' poss ass
  | M.keysSet pred' == M.keysSet poss
  = foldr (=<<) (Partial ass) $ M.intersectionWith mgu pred' poss
  | otherwise
  = Error $ PredicatesOfDifferentShapes pred' poss ass
