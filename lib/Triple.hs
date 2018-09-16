module Triple where

import Data.List(nub)
-- import Debug.Trace
import Prelude hiding (not, and, or)
import Util (line)

data Triple a b = Tri
  { pre :: b -- things it consumes
  , op :: a
  , post :: b -- things it produces
  } deriving (Eq, Ord)

instance Show (HTriple) where
  show t = "{"++pre'++"} "++show op'++" {"++post'++"}"
    where
      pre' = show' $ pre t
      op' = op t
      post' = show' $ post t
      show' :: [[Sym]] -> String
      show' x = drop 2 $ concatMap (", "++) $ map show'' x
      show'' :: [Sym] -> String
      show'' x = drop 1 $ concatMap (" "++) x

-- TODO(jopra): Replace with new repr:
-- - should be restricted to the syntax of the language
-- - should be printable
-- - should be interpretable
-- - should have representation information or some way of storing it.
type Sym = String -- deriving (Show, Eq, Ord)

type Scope = [Sym] -- TODO(jopra): Use set

type Statement = [Sym]

type Pred = [Sym] -- Predicates are triples over statements in a scope, producing a scope
type Check = [Pred]

type HTriple = Triple Op Check -- HTriples are triples over operations, with checks, in a scope

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

data Op = I Instruction
        | Algo [HTriple]
        deriving (Show, Eq, Ord)

data Expr = E String deriving (Show, Eq, Ord)

data Failure
  = Contradiction Expr Expr
  | Unspecified Expr
  | Many [Failure]
  deriving (Ord, Eq)

getAllErrors :: [Failure] -> Failure
getAllErrors xs = case (getErrors (Many xs)) of
                    [x] -> x
                    xs' -> Many xs'

getErrors :: Failure -> [Failure]
getErrors (Many xs) = nub $ concatMap getErrors xs
getErrors x = [x]

func :: Sym -> Op -> [Pred] -> [Pred] -> HTriple
func n algo pre' post' =
  Tri { pre = pre'
      , op = algo
      , post = post'
      }

val :: String -> Sym
val = id

var :: String -> Sym
var = id

add_pre :: Pred -> HTriple -> HTriple
add_pre p h = h {pre = p:(pre h)}

add_post :: Pred -> HTriple -> HTriple
add_post p h = h {post = p:(post h)}

exists :: Sym -> Pred
exists v = [v]

creates :: Sym -> Pred
creates v = [v]
