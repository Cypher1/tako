module Operation where

import Data.Bits

data Sym = S String -- deriving (Show, Eq, Ord)
  deriving (Eq, Ord)
instance Show Sym where
  show (S s) = s

data Instruction
  = T TriOp Sym Sym Sym
  | B BiOp Sym Sym
  | U UnOp Sym
  | L Val Sym
  deriving (Show, Eq, Ord)

type Val = Int

data TriOp = And | Or | Add | Sub | Div | Mul deriving (Show, Eq, Ord)
data BiOp = Not | New deriving (Show, Eq, Ord)
data UnOp = Free deriving (Show, Eq, Ord)

type Op = [Instruction] -- deriving (Show, Eq, Ord)

type Mem = [(Sym, Val)] -- deriving (Show, Eq, Ord)

getV :: Sym -> Mem -> Val
getV k h = case v of
               [x] -> x
               [] -> error $ show k++" not defined in "++show h
               v' -> error $ show k++" multiply defined in "++show h++" as "++show v'
  where
    v = map snd $ filter ((==k).fst) h


removeV :: Sym -> Mem -> Mem
removeV k m = filter ((/=k).fst) m

setV :: Sym -> Val -> Mem -> Mem
setV k v m = (k, v):removeV k m

interpreter :: Op -> Mem -> Mem
interpreter is m = foldr run m is

run :: Instruction -> Mem -> Mem


run (L r' r) m = setV  r r' m

run (U o a) m = m'
  where
    m' = case o of
           Free -> removeV a m

run (B o a r) m = setV r r' m
  where
    a' = getV a m
    r' = case o of
           Not -> complement a'
           New -> a'
run (T o a b r) m = setV r r' m
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
