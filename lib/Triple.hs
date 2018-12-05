module Triple where

import Prelude hiding (showList)
import Util (showList)
import Pred (Pred, State, Requirements, solutionsAndErrors, filterErrors, ignoreErrors, Assignment, Val, Var, ResolutionFailure)
import Operation (Op)
import qualified Data.Set as S

data Triple a b c= Tri
  { pre :: a  -- requirements of the calling environment
  , op :: b   -- operations to be executed
  , post :: c -- outcomes of the operations
  } deriving (Eq, Ord)

type HTriple = Triple Requirements Op State -- HTriples are triples over operations, with states/checks
-- TODO(jopra): Consider new typing pre vs post conditions to ensure they aren't mixed up

instance Show HTriple where
  show t = "{"++pre'++"}"++showList op'++"{"++post'++"}"
    where
      pre' = show' $ pre t
      op' = op t
      post' = show' $ post t
      show' x = showList $ S.toList x

-- TODO(jopra): Replace with new repr:
-- - should be restricted to the syntax of the language
-- - should be printable
-- - should be interpretable
-- - should have representation information or some way of storing it.

emp :: HTriple
emp =
  Tri { pre = S.empty
      , op = []
      , post = S.empty
      }

data Failure = Failure FailureMode [ResolutionFailure] deriving (Show, Eq, Ord)

data FailureMode
  = Contradiction State
  | Unsolved { state_::State, requirements_::Requirements}
  | Many [Failure]
  | Underspecified [Assignment Val] HTriple HTriple
  deriving (Show, Ord, Eq)

func :: Op -> Requirements -> State -> HTriple
func algo pre' post' =
  Tri { pre = pre'
      , op = algo
      , post = post'
      }

-- TODO(jopra): Use lenses for these patterns
addPre :: Pred Var -> HTriple -> HTriple
addPre p h = h {pre = S.union (S.singleton p) (pre h)}

addPost :: Pred Val -> HTriple -> HTriple
addPost p h = h {post = S.union (S.singleton p) (post h)}

update :: HTriple -> HTriple -> Either Failure HTriple
update accepted extension
  = case solutions' of
      [] -> Left $ Failure (Unsolved state requirements) errors'
      [_sol] -> Right $ mergeTriples accepted extension -- TODO(jopra): This discards the solution's requirements
      sols -> Left $ Failure (Underspecified sols accepted extension) errors'
  where
    solutions' = ignoreErrors possibles
    errors' = filterErrors possibles
    possibles = solutionsAndErrors state requirements
    state = post accepted
    requirements = pre extension

-- Unchecked 'merge' of two triples, running one after the other.
mergeTriples :: HTriple -> HTriple -> HTriple
mergeTriples accepted extension
  = Tri { pre = pre accepted
        -- TODO(jopra): Add anything needed to make function calls etc.
        , op = op accepted ++ op extension
        , post = post extension
        }

assume :: [Pred Val] -> HTriple
assume ps = promise (S.fromList ps) emp

promise :: State -> HTriple -> HTriple
promise ps' sh = sh {post=S.union ps' (post sh)}
