{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module PrimType where

import           Util                           ( Pretty(..) )

import           Data.List                      ( (\\) )
import           Control.Monad                  ( foldM )

type Sym = String


-- TODO(cypher1): Use named indexes to better support labelling and dependent types.

data Ty = Unit | Sum Ty Ty | Product Ty Ty | Var Sym
  deriving (Show, Eq, Ord)

data ADT = Z | F ADT | S ADT | Prod ADT ADT
  deriving (Show, Eq, Ord)

minsize :: Ty -> Integer
minsize ty = l2 $ cardinality ty

l2 :: Integer -> Integer
l2 x = head [ i | i <- [0 ..], 2 ^ i >= x ]

cardinality :: Ty -> Integer
cardinality Unit          = 1
cardinality (Sum     f s) = cardinality f + cardinality s
cardinality (Product f s) = cardinality f * cardinality s
cardinality (Var name'  ) = error $ "Var '" ++ name' ++ "' does not have a size"

instance Pretty ADT where
  pretty Z = ""
  pretty (F Z) = "0"
  pretty (S Z) = "1"
  pretty (F f) = "1("++pretty f++")"
  pretty (S s) = "1("++pretty s++")"
  pretty (Prod f s) = "("++pretty f++"*"++pretty s++")"

instance Pretty Ty where
  pretty Unit = "1"
  pretty (Sum f s) = "("++pretty f++"+"++pretty s++")"
  pretty (Product f s) = "("++pretty f++"*"++pretty s++")"
  pretty (Var name') = name'++"?"

nbits :: Integer -> Ty
nbits n = case n `divMod` 2 of
  (0, 0) -> Unit
  (0, 1) -> Sum Unit Unit
  (d, 0) -> Product mb mb where mb = nbits d
  (_, 1) -> Product (nbits (n - 1)) (nbits 1)
  (d, r) -> error $ "Divided " ++ show n ++ " by 2 and got " ++ show (d, r)

data Failure
  = RanOutOfVariableNames
  | CouldNotUnify Ty Ty
  | CouldNotUnifyVar Sym Ty Ty
  | AssignmentFailedToUnify Ty Ty Assignment
  | InfiniteUnification Sym Ty
  deriving (Show, Eq)

instance Pretty Failure where
  pretty RanOutOfVariableNames = "Ran out of (an assumedly infinite) list of variables..."
  pretty (CouldNotUnify t1 t2) = "Could not unify types "++pretty t1++" and "++pretty t2
  pretty (CouldNotUnifyVar v t1 t2) = "Could not find an assignment for "++v++" that unifies types "++pretty t1++" and "++pretty t2
  pretty (AssignmentFailedToUnify t1 t2 ass) = "Assignment "++show [(n, pretty v)|(n, v)<-ass]++" failed to unify "++show t1++" and "++show t2
  pretty (InfiniteUnification v t) = "Cannot unify "++v++" and "++pretty t++" as "++pretty t++" would become infinite."

newtype WithVars a = WithVars { unWithVars :: [Ty] -> Try (a, [Ty])}

data Try a = Failed Failure | Success a
  deriving (Show, Eq)

instance Functor Try where
  f`fmap`(Success a) = Success $ f a
  _`fmap`(Failed er) = Failed er

failed :: WithVars a -> Bool
failed x = case runWithVars x of
  (Success _) -> False
  (Failed  _) -> True

instance Pretty a => Pretty (Try a) where
  pretty (Success a) = pretty a
  pretty (Failed f) = pretty f

failDueTo :: Failure -> WithVars a
failDueTo f = WithVars $ const $ Failed f

varsFrom :: Ty -> [Sym]
varsFrom Unit          = []
varsFrom (Sum     f s) = varsFrom f ++ varsFrom s
varsFrom (Product f s) = varsFrom f ++ varsFrom s
varsFrom (Var n      ) = [n]

alphaRename :: (String -> String) -> Ty -> Ty
alphaRename _ Unit          = Unit
alphaRename r (Var n      ) = Var (r n)
alphaRename r (Sum     f s) = Sum (alphaRename r f) (alphaRename r s)
alphaRename r (Product f s) = Product (alphaRename r f) (alphaRename r s)

hideVar :: Sym -> WithVars ()
hideVar used = WithVars $ \vars -> Success ((), vars \\ [Var used])

getVar :: WithVars Ty
getVar = WithVars $ \case
  (n : vars') -> Success (n, vars')
  []          -> Failed RanOutOfVariableNames

runWithVars :: WithVars a -> Try a
runWithVars ma = case unWithVars ma vars of
  Success (res, _) -> Success res
  Failed  f        -> Failed f
 where
  vars  = Var <$> names
  names = ltrs ++ [ l ++ n | l <- ltrs, n <- names ]
  ltrs  = (: []) <$> "abcdefghijklmnopqrstuvwxyz"

instance Functor WithVars where
  f`fmap`ma = f <$> ma

instance Applicative WithVars where
  pure = return
  mf <*> ma = do
    f <- mf
    f <$> ma

instance Monad WithVars where
  return a = WithVars $ \vars -> Success (a, vars)
  (WithVars x) >>= f
    = WithVars $ \vars ->
      case x vars of
        Success (s', vars') -> unWithVars (f s') vars'

        Failed f' -> Failed f'

merge :: Assignment -> Assignment -> WithVars Assignment
merge []            ys = return ys
merge ((x, v) : xs) ys = do
  mg <- merge xs ys
  case find x ys of
    Nothing -> return $ (x, v) : mg
    Just v2 ->
      if v2 == v then return mg else failDueTo $ CouldNotUnifyVar x v v2


getType :: ADT -> WithVars Ty
getType Z          = return Unit
getType (F f     ) = Sum <$> getType f <*> getVar
getType (S s     ) = Sum <$> getVar <*> getType s
getType (Prod f s) = Product <$> getType f <*> getType s

getZero :: Ty -> ADT
getZero Unit          = Z
getZero (Sum     f _) = F $ getZero f
getZero (Product f s) = Prod (getZero f) (getZero s)
getZero (Var name'  ) = error $ "Var '" ++ name' ++ "' does not have a zero"

extract :: ADT -> ADT -> ADT
extract Z          x          = x
extract (F f     ) (F x     ) = extract f x
extract (S s     ) (S x     ) = extract s x
extract (F f     ) (Prod x _) = extract f x
extract (S s     ) (Prod _ x) = extract s x
extract (Prod f s) x          = Prod (extract f x) (extract s x)

extract f x =
  error
    $  "Pattern match failed "
    ++ show x
    ++ " doesn't match pattern "
    ++ show f

type Assignment = [(Sym, Ty)]

find :: Sym -> Assignment -> Maybe Ty
find n xs = case [ v | (x, v) <- xs, x == n ] of
  []  -> Nothing
  [v] -> Just v
  vs  -> error $ "Conflicting definitions for " ++ n ++ ": " ++ show vs

mgu :: WithVars Ty -> WithVars Ty -> WithVars Assignment
mgu x y = do
  x' <- x
  y' <- y
  mgu' x' y'

merge2 :: (Ty, Ty) -> (Ty, Ty) -> WithVars Assignment
merge2 (fx, sx) (fy, sy) = do
  h <- mgu' fx fy
  t <- mgu' sx sy
  return $ h ++ t

mgu' :: Ty -> Ty -> WithVars Assignment
mgu' Unit            Unit                         = return []
mgu' (Sum     fx sx) (Sum     fy sy)              = merge2 (fx, sx) (fy, sy)
mgu' (Product fx sx) (Product fy sy)              = merge2 (fx, sx) (fy, sy)
mgu' (Var namex') (Var namey') | namex' == namey' = do
  hideVar namex'
  hideVar namey'
  return []
mgu' v (Var m) | m `elem` varsFrom v = failDueTo $ InfiniteUnification m v
               | otherwise           = hideVar m >> return [(m, v)]
mgu' (Var m) v | m `elem` varsFrom v = failDueTo $ InfiniteUnification m v
               | otherwise           = hideVar m >> return [(m, v)]
mgu' t v = failDueTo $ CouldNotUnify v t

applyType :: WithVars Ty -> WithVars Ty -> WithVars Ty
applyType x y = do
  x' <- x
  y' <- y
  applyType' x' y' =<< mgu' x' y'

applyType' :: Ty -> Ty -> Assignment -> WithVars Ty
applyType' x y ass = do
  x' <- assign x ass
  y' <- assign y ass
  if x' == y' then return x else failDueTo $ AssignmentFailedToUnify x' y' ass

assign :: Ty -> Assignment -> WithVars Ty
assign = foldM assign1

assign1 :: Ty -> (Sym, Ty) -> WithVars Ty
assign1 _ (m, v) | m `elem` varsFrom v = failDueTo $ InfiniteUnification m v
assign1 (Var n) (m, v) | n == m    = return v
                       | otherwise = return $ Var n
assign1 Unit          _ = return Unit
assign1 (Sum     f s) k = Sum <$> assign1 f k <*> assign1 s k
assign1 (Product f s) k = Product <$> assign1 f k <*> assign1 s k


-- TODO(cypher1): Write up an inc with types.
-- inc :: ADT -> (ADT, Bool)
-- inc Z = (Z, True)
-- inc (N t f s)
  -- = case t of
      -- Sum Fst -> (N (Sum (if carry' then Snd else Fst)) f' s', False)
      -- Sum Snd -> (N (Sum (if carry' then Fst else Snd)) f' s', carry')
      -- Prod -> (N Product f' s', carry')
  -- where
    -- (s', carry) = inc s
    -- (f', carry') = if carry then inc f else (f, False)

boolean :: Ty
boolean = Sum Unit Unit

uint32 :: Ty
uint32 = nbits 32
