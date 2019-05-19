module Operation where

import           Util                           ( boundedAll
                                                , Pretty(..)
                                                )

import           Data.Bits

import           Language                       ( PrimUnOpType(..)
                                                , PrimBiOpType(..)
                                                , PrimTriOpType(..)
                                                )

type Sym = String

type Val = Int

data PrimOp
  = L Val Sym
  | U PrimUnOpType Sym
  | B PrimBiOpType Sym Sym
  | T PrimTriOpType Sym Sym Sym
  deriving (Show, Eq, Ord)

instance Pretty PrimOp where
  pretty (L v x) = "L "++show v++" "++x
  pretty (U op x) = pretty op++" "++x
  pretty (B op x y) = pretty op++" "++x++" "++y
  pretty (T op x y z) = pretty op++" "++x++" "++y++" "++z

-- TODO(jopra): Ensure array access safety.
convert :: String -> Either String PrimOp
convert s
  | c `elem` unops = if n == 2
    then Right $ U (read c) opa
    else Left $ "Expected(1) " ++ c ++ " <reg>"
  | c == "L" = if n == 3
    then Right $ L (read opa) opb
    else Left $ "Expected(2) " ++ c ++ " <val> <reg>"
  | c `elem` biops = if n == 3
    then Right $ B (read c) opa opb
    else Left $ "Expected(3) " ++ c ++ " <reg> <reg>"
  | c `elem` triops = if n == 4
    then Right $ T (read c) opa opb opr
    else Left $ "Expected(4) " ++ c ++ " <reg> <reg> <reg>"
  | otherwise = Left $ "Unknown Op: '" ++ c ++ "'."
 where
  n                         = length w
  (c : opa : opb : opr : _) = w ++ [ "" | _ <- [0 :: Int ..] ]
  w                         = words s


unops :: [String]
unops = show <$> (boundedAll :: [PrimUnOpType])

biops :: [String]
biops = show <$> (boundedAll :: [PrimBiOpType])

triops :: [String]
triops = show <$> (boundedAll :: [PrimTriOpType])

type Op = [PrimOp]

type Mem = [(Sym, Val)]

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
exec (L r' r) m = setV r r' m

exec (U o  a) m = m'
 where
  m' = case o of
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
