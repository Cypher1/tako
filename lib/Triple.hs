module Triple where

import Prelude hiding (showList)
import Data.List(nub, (\\))
import Debug.Trace
import Util (line, showList, passes, fails)
import Pred (Pred, State, resolutions, Assignment)
import Operation (Sym (S), Op)
import qualified Data.Set as S
import Data.Either (partitionEithers)

data Triple a b = Tri
  { pre :: b -- things it consumes
  , op :: a
  , post :: b -- things it produces
  } deriving (Eq, Ord)

type HTriple = Triple Op State -- HTriples are triples over operations, with states/checks

instance Show HTriple where
  show t = "{"++pre'++"}"++showList op'++"{"++post'++"}"
    where
      pre' = show' $ pre t
      op' = op t
      post' = show' $ post t
      show' :: State -> String
      show' x = showList $ S.toList x
      show'' :: [Sym] -> String
      show'' x = drop 1 $ concatMap (\s->" "++show s) x

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

data Failure
  = Contradiction State
  | Unproven State HTriple
  | Undefined [Sym] HTriple
  | Many [Failure]
  | Underspecified [Assignment] HTriple HTriple
  deriving (Show, Ord, Eq)

getAllErrors :: [Failure] -> Failure
getAllErrors xs = case getErrors (Many xs) of
                    [x] -> x
                    xs' -> Many xs'

getErrors :: Failure -> [Failure]
getErrors (Many xs) = nub $ concatMap getErrors xs
getErrors x = [x]

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

update :: HTriple -> HTriple -> Either HTriple Failure
update accepted extension
  = case solutions of
      [] -> Right $ Unproven state extension
      [sol] -> trace ("Specialising with "++show sol) $ Left $ mergeTriples accepted extension
      sols -> Right $ Underspecified sols accepted extension
  where
    solutions = resolutions state requirements
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
