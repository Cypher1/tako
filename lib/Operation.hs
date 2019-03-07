module Operation where

import Util (boundedAll, Pretty(..))

import Data.Bits

type Sym = String

data Instruction
  = T TriOp Sym Sym Sym
  | B BiOp Sym Sym
  | U UnOp Sym
  | L Val Sym
  deriving (Show, Eq, Ord)

instance Pretty Instruction where
  pretty = show

only :: a -> [(a, String)]
only a = [(a, "")]

-- TODO(jopra): Ensure array access safety.
-- TODO(jopra): Rename this, read and show are reserved.
instance Read Instruction where
  readsPrec _p s
    | null w = []
    | n == 3 && c == "L" = only $ L (read opa) opb
    | n == 2 && c`elem`map show unops = only $ U (read c) opa
    | n == 3 && c`elem`map show biops = only $ B (read c) opa opb
    | n == 4 && c`elem`map show triops = only $ T (read c) opa opb opr
    | otherwise = []
    where
      n = length w
      c = head w
      opa = w!!1
      opb = w!!2
      opr = w!!3
      w = words s

type Val = Int

data TriOp = And | Or | Add | Sub | Div | Mul deriving (Show, Read, Eq, Ord, Enum, Bounded)
data BiOp = Not | New deriving (Show, Read, Eq, Ord, Enum, Bounded)
data UnOp = Free deriving (Show, Read, Eq, Ord, Enum, Bounded)

unops :: [UnOp]
unops = boundedAll

biops :: [BiOp]
biops = boundedAll

triops :: [TriOp]
triops = boundedAll

type Op = [Instruction]

type Mem = [(Sym, Val)]

getV :: Sym -> Mem -> Val
getV k h
  = case v of
    [x] -> x
    [] -> error $ show k++" not defined in "++show h
    v' -> error $ show k++" multiply defined in "++show h++" as "++show v'
  where
    v = map snd $ filter ((==k).fst) h


removeV :: Sym -> Mem -> Mem
removeV k = filter ((/=k).fst)

setV :: Sym -> Val -> Mem -> Mem
setV k v m = (k, v):removeV k m

interpreter :: Op -> Mem -> Mem
interpreter is m = foldr exec m is

exec :: Instruction -> Mem -> Mem
exec (L r' r) m = setV  r r' m

exec (U o a) m = m'
  where
    m' = case o of
           Free -> removeV a m

exec (B o a r) m = setV r r' m
  where
    a' = getV a m
    r' = case o of
           Not -> complement a'
           New -> a'

exec (T o a b r) m = setV r r' m
  where
    a' = getV a m
    b' = getV b m
    r' = case o of
           And -> a' .&. b'
           Or -> a' .|. b'
           Add -> a' + b'
           Sub -> a' - b'
           Div -> a'`div`b'
           Mul -> a' * b'
