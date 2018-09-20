module Triple where

import Data.List(nub)
import Debug.Trace
import Prelude hiding (showList)
import Util (line)
import qualified Data.Set as S
import Data.Set (Set)

data Triple a b = Tri
  { pre :: b -- things it consumes
  , op :: a
  , post :: b -- things it produces
  } deriving (Eq, Ord)

showList :: Show a => [a] -> String
showList xs = drop (length joiner) $ concatMap (\x->joiner++show x) xs
  where
    joiner = ", "

instance Show (HTriple) where
  show t = "{"++pre'++"}"++showList op'++"{"++post'++"}"
    where
      pre' = show' $ pre t
      op' = op t
      post' = show' $ post t
      show' :: State -> String
      show' x = drop 2 $ concatMap (", "++) $ map show'' $ S.toList x
      show'' :: [Sym] -> String
      show'' x = drop 1 $ concatMap (\s->" "++show s) x

-- TODO(jopra): Replace with new repr:
-- - should be restricted to the syntax of the language
-- - should be printable
-- - should be interpretable
-- - should have representation information or some way of storing it.
data Sym = S String -- deriving (Show, Eq, Ord)
  deriving (Eq, Ord)
instance Show Sym where
  show (S s) = s

type Pred = [Sym]
type State = Set Pred
emptyState :: State
emptyState = S.empty

emp :: HTriple
emp =
  Tri { pre = S.empty
      , op = []
      , post = S.empty
      }

type HTriple = Triple Op State -- HTriples are triples over operations, with states/checks

data Instruction
  = And Sym Sym Sym
  | Or  Sym Sym Sym
  | Not Sym Sym
  | Add Sym Sym Sym
  | Sub Sym Sym Sym
  | Div Sym Sym Sym
  | New Sym Sym
  | Free Sym
  deriving (Show, Eq, Ord)

type Op = [Instruction]

data Failure
  = Contradiction State
  | Unresolved State State
  | Many [Failure]
  deriving (Show, Ord, Eq)

getAllErrors :: [Failure] -> Failure
getAllErrors xs = case (getErrors (Many xs)) of
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

val :: String -> Sym
val = S

var :: String -> Sym
var = S

add_pre :: Pred -> HTriple -> HTriple
add_pre p h = h {pre = S.union (S.singleton p) (pre h)}

add_post :: Pred -> HTriple -> HTriple
add_post p h = h {post = S.union (S.singleton p) (post h)}

exists :: Sym -> Pred
exists v = [v]

creates :: Sym -> Pred
creates v = [v]


update :: HTriple -> HTriple -> Either HTriple Failure
update ht sh
  | not(null unresolved_pre)= Right $ Unresolved unresolved_pre (post ht)
  | otherwise = Left sh
  where
    unresolved_pre = S.filter (not.(resolved (post ht))) (pre sh)
    -- unconsumed_post = S.filter (not.(resolved (post ht))) (pre sh) -- the left overs {R}
    pre' = pre ht
    post' = post ht
    op' = op ht

resolved :: State -> Pred -> Bool
resolved sh pred
  | null sh = False -- can't prove without axioms / a world
  | any(\x-> [x]`notElem` sh) pred = False -- can't prove things without their parts existing
  | pred `elem` sh = True -- we know what we know
  | otherwise = trace ("TODO(jopra): Find "++show pred++" in "++show sh) True -- proofs... are hard...

assume :: [Pred] -> HTriple
assume ps = promise (S.fromList ps) emp

promise :: State -> HTriple -> HTriple
promise ps' sh = sh {post=S.union elems $ S.union ps' (post sh)}
  where
    elems :: Set Pred
    elems = S.fromList $ map(:[]) $ concat ps' -- TODO(jopra): use sets
