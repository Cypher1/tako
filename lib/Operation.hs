module Operation where

-- import           Data.Bits

import           Language                       ( PrimValOpType(..)
                                                , PrimUnOpType(..)
                                                , PrimBiOpType(..)
                                                , PrimTriOpType(..)
                                                )
import           PrimType                       ( Sym )
import           PrimOpType                     ( Mem
                                                , Val
                                                , PrimOp(..)
                                                , Op
                                                )

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
exec (L o r' r) m = case o of
  PrimLoad -> setV r r' m

exec (U o a) m = case o of
  PrimFree -> removeV a m

exec (B o a r) m = setV r r' m
 where
  a' = getV a m
  r' = case o of
    PrimNot -> error "complement a'"
    PrimNew -> a'

exec (T o a b r) m = setV r r' m
 where
  _a' = getV a m
  _b' = getV b m
  r' = case o of
    PrimAnd -> error "a' .&. b'"
    PrimOr  -> error "a' .|. b'"
    PrimAdd -> error "a' + b'"
    PrimSub -> error "a' - b'"
    PrimDiv -> error "a' `div` b'"
    PrimMul -> error "a' * b'"
