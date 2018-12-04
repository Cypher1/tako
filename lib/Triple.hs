module Triple where

import Prelude hiding (showList)
import Util (showList)
import Pred (Pred, State, solutionsAndErrors, filterErrors, ignoreErrors, Assignment, ResolutionFailure)
import Operation (Sym, Op)
import qualified Data.Set as S

data Triple a b = Tri
  { pre :: b -- things it consumes
  , op :: a
  , post :: b -- things it produces
  } deriving (Eq, Ord)

type HTriple = Triple Op State -- HTriples are triples over operations, with states/checks
-- TODO(jopra): Consider new typing pre vs post conditions to ensure they aren't mixed up

instance Show HTriple where
  show t = "{"++pre'++"}"++showList op'++"{"++post'++"}"
    where
      pre' = show' $ pre t
      op' = op t
      post' = show' $ post t
      show' :: State -> String
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
  | Unsolved { state_::State, requirements_::State}
  | Undefined { missing_::[Sym], in_::HTriple}
  | Many [Failure]
  | Underspecified [Assignment] HTriple HTriple
  deriving (Show, Ord, Eq)

func :: Op -> State -> State -> HTriple
func algo pre' post' =
  Tri { pre = pre'
      , op = algo
      , post = post'
      }

-- TODO(jopra): Use lenses for these patterns
addPre :: Pred -> HTriple -> HTriple
addPre p h = h {pre = S.union (S.singleton p) (pre h)}

addPost :: Pred -> HTriple -> HTriple
addPost p h = h {post = S.union (S.singleton p) (post h)}

update :: HTriple -> HTriple -> Either Failure HTriple
update accepted extension
  = case solutions' of
      [] -> Left $ Failure (Unsolved state requirements) errors'
      [_sol] -> Right $ mergeTriples accepted extension -- TODO(jopra): This discards the solution's requirements
      sols -> Left $ Failure (Underspecified sols accepted extension) errors'
  where
    -- TODO(jopra): Also return some debug info
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

assume :: [Pred] -> HTriple
assume ps = promise (S.fromList ps) emp

promise :: State -> HTriple -> HTriple
promise ps' sh = sh {post=S.union ps' (post sh)}
