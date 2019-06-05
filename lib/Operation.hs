module Operation where

-- import           Data.Bits

import           Language                       ( PrimValOpType(..)
                                                , PrimUnOpType(..)
                                                , PrimBiOpType(..)
                                                , PrimTriOpType(..)
                                                )
import           PrimType                       ( Sym
                                                , Value(..)
                                                )
import           PrimOpType                     ( Mem
                                                , Val
                                                , PrimOp(..)
                                                , Op
                                                )
import           Util                           ( pretty )

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
    PrimNot -> complement' a'
    PrimNew -> a'

exec (T o a b r) m = setV r r' m
 where
  a' = getV a m
  b' = getV b m
  r' = case o of
    PrimAnd -> and' a' b'
    PrimOr  -> or' a' b'
    PrimAdd -> error "a' + b'"       -- Assumes an unsigned integer
    PrimSub -> error "a' - b'"       -- Assumes an unsigned integer
    PrimDiv -> error "a' `div` b'"   -- Assumes an unsigned integer
    PrimMul -> error "a' * b'"       -- Assumes an unsigned integer

complement' :: Value -> Value
complement' (Values as          ) = Values $ complement' <$> as
complement' (Value 0 (Values [])) = Value 1 (Values [])
complement' (Value 1 (Values [])) = Value 0 (Values [])
complement' s                     = error $ "Cannot complement' " ++ pretty s

and' :: Value -> Value -> Value
and' (Values as) (Values bs) | length as == length bs =
  Values $ zipWith and' as bs
and' (Value s (Values [])) (Value t (Values []))
  | s == 0 || t == 0 = Value 0 (Values [])
  | otherwise        = Value t (Values [])
and' s t = error $ "Cannot and' " ++ pretty s ++ " with " ++ pretty t

or' :: Value -> Value -> Value
or' (Values as) (Values bs) | length as == length bs =
  Values $ zipWith or' as bs
or' (Value s (Values [])) (Value t (Values []))
  | s /= 0    = Value s (Values [])
  | t /= 0    = Value t (Values [])
  | otherwise = Value 0 (Values [])
or' s t = error $ "Cannot or' " ++ pretty s ++ " with " ++ pretty t
