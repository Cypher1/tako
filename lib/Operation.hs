module Operation where

import           Data.Bits

import           Language                       ( PrimValOpType(..)
                                                , PrimUnOpType(..)
                                                , PrimBiOpType(..)
                                                , PrimTriOpType(..)
                                                )
import Ops (Sym, Mem, Val, PrimOp(..), Op)

getV :: Sym -> Mem -> Val
getV k h = case v of
  [x] -> x
  []  -> error $ show k ++ " not defined in " ++ show h
  v' ->
    error $ show k ++ " multiply defined in " ++ show h ++ " as " ++ show v'
  where v = map snd $ filter ((== k) . fst) h

removeV :: Sym -> Mem -> Mem
removeV k = filter ((/= k) . fst)

setV :: Sym -> Val -> Mem -> Mem
setV k v m = (k, v) : removeV k m

interpreter :: Op -> Mem -> Mem
interpreter is m = foldr exec m is

exec :: PrimOp -> Mem -> Mem
exec (L o r' r) m
  = case o of
      PrimLoad -> setV r r' m

exec (U o  a) m
  = case o of
      PrimFree -> removeV a m

exec (B o a r) m = setV r r' m
 where
  a' = getV a m
  r' = case o of
    PrimNot -> complement a'
    PrimNew -> a'

exec (T o a b r) m = setV r r' m
 where
  a' = getV a m
  b' = getV b m
  r' = case o of
    PrimAnd -> a' .&. b'
    PrimOr  -> a' .|. b'
    PrimAdd -> a' + b'
    PrimSub -> a' - b'
    PrimDiv -> a' `div` b'
    PrimMul -> a' * b'
