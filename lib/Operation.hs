module Operation where

-- import           Data.Bits

import           Language                       ( PrimValOpType(..)
                                                , PrimUnOpType(..)
                                                , PrimBiOpType(..)
                                                , PrimTriOpType(..)
                                                )
import           PrimType                       ( Sym
                                                , Value(..)
                                                , iToV
                                                , vToI
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
    PrimAdd -> add' a' b'
    PrimSub -> sub' a' b'
    PrimDiv -> div' a' b'
    PrimMul -> mul' a' b'

tag :: Int -> Value
tag t = Union t $ Struct []

-- Ops -- Needs review, should probably be actual functions, with
-- optimisations.

add' :: Value -> Value -> Value
add' (Struct as) (Struct bs) | length as == length bs =
  Struct $ snd $ foldr carry' (0, []) $ zip as bs
 where
  carry' :: (Value, Value) -> (Int, [Value]) -> (Int, [Value])
  carry' (Union a (Struct []), Union b (Struct [])) (c, xs) = (c', tag r : xs)
   where
    (c', r) = case (c, a, b) of
      (0, 0, 0) -> (0, 0)
      (0, 0, 1) -> (0, 1)
      (0, 1, 0) -> (0, 1)
      (1, 0, 0) -> (0, 1)
      (0, 1, 1) -> (1, 0)
      (1, 0, 1) -> (1, 0)
      (1, 1, 0) -> (1, 0)
      (1, 1, 1) -> (1, 1)
      _         -> error $ "carry' " ++ show (c, a, b)
  carry' a b = error $ "carry' " ++ show a ++ " : " ++ show b
add' a b = error $ "add' " ++ show a ++ " : " ++ show b

complement' :: Value -> Value
complement' (Struct as          ) = Struct $ complement' <$> as
complement' (Union 0 (Struct [])) = tag 1
complement' (Union 1 (Struct [])) = tag 0
complement' s                     = error $ "Cannot complement' " ++ pretty s

and' :: Value -> Value -> Value
and' (Struct as) (Struct bs) | length as == length bs =
  Struct $ zipWith and' as bs
and' (Union s (Struct [])) (Union t (Struct [])) | s == 0 || t == 0 = tag 0
                                                 | otherwise        = tag t
and' s t = error $ "Cannot and' " ++ pretty s ++ " with " ++ pretty t

sub' :: Value -> Value -> Value
sub' a b = add' a (neg' b)

neg' :: Value -> Value
neg' x = add' (complement' x) (iToV 1)

mul' :: Value -> Value -> Value
mul' a b = iToV $ a' * b'
 where
  a' = vToI a
  b' = vToI b

div' :: Value -> Value -> Value
div' a b = iToV $ if b' == 0 then 0 else a' `div` b'
 where
  a' = vToI a
  b' = vToI b

or' :: Value -> Value -> Value
or' (Struct as) (Struct bs) | length as == length bs =
  Struct $ zipWith or' as bs
or' (Union s (Struct [])) (Union t (Struct [])) | s /= 0    = tag s
                                                | t /= 0    = tag t
                                                | otherwise = tag 0
or' s t = error $ "Cannot or' " ++ pretty s ++ " with " ++ pretty t
