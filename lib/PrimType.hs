{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module PrimType where

import           Util                           ( Pretty(..) )

import           Data.List                      ( (\\) )
import           Control.Monad                  ( foldM
                                                , zipWithM
                                                )

type Sym = String


-- TODO(cypher1): Use named indexes to better support labelling and dependent types.

-- TODO(cypher1): Use maps or sets
data Ty = Sum [(Sym, Ty)] | Product [(Sym, Ty)] | Var Sym
  deriving (Show, Eq, Ord)

type UnionTag = Sym

-- Note: Ordering is important for zeros and products.
data Value = Value UnionTag Value | Values [Value]
  deriving (Show, Eq, Ord)

minsize :: Ty -> Integer
minsize ty = l2 $ cardinality ty

l2 :: Integer -> Integer
l2 x = head [ i | i <- [0 ..], 2 ^ i >= x ]

cardinality :: Ty -> Integer
cardinality (Sum tys) = sum [ cardinality $ snd t | t <- tys ]
cardinality (Product tys) = product [ cardinality $ snd t | t <- tys ]
cardinality (Var name') = error $ "Var '" ++ name' ++ "' does not have a size"

instance Pretty (Sym, Ty) where
  pretty (n, t) = n++":"++pretty t

instance Pretty Value where
  pretty (Value s v) = s++":("++pretty v++")"
  pretty (Values []) = "()"
  pretty (Values (v:vs)) = "("++pretty v++concat[" "++pretty x|x<-vs]++")"

instance Pretty Ty where
  pretty (Sum []) = "0"
  pretty (Sum ((n, t):tys)) = "("++pretty (n, t)++concat["+"++pretty t'|t'<-tys]++")"
  pretty (Product []) = "1"
  pretty (Product (t:tys)) = "("++pretty t++concat["*"++pretty t'|t'<-tys]++")"
  pretty (Var name') = name'++"?"

nbits :: Integer -> Ty
nbits n = case n `divMod` 2 of
  (0, 0) -> Product []
  (0, 1) -> Sum [("0", Product []), ("1", Product [])]
  (d, 0) -> Product [("high", mb), ("low", mb)] where mb = nbits d
  (_, 1) -> Product [("high", nbits (n - 1)), ("low", nbits 1)]
  (d, r) -> error $ "Divided " ++ show n ++ " by 2 and got " ++ show (d, r)

data Failure
  = RanOutOfVariableNames
  | CouldNotUnify Ty Ty
  | CouldNotUnifyVar Sym Ty Ty
  | CouldNotUnifySymbols Sym Ty Sym Ty
  | AssignmentFailedToUnify Ty Ty Assignment
  | InfiniteUnification Sym Ty
  | PatternMismatch Sym Ty Value
  | InvalidValueForType Ty Value
  deriving (Show, Eq)

instance Pretty Failure where
  pretty RanOutOfVariableNames = "Ran out of (an assumedly infinite) list of variables.."
  pretty (CouldNotUnify t1 t2) = "Could not unify types "++pretty t1++" and "++pretty t2
  pretty (CouldNotUnifyVar v t1 t2) = "Could not find an assignment for "++v++" that unifies types "++pretty t1++" and "++pretty t2
  pretty (CouldNotUnifySymbols s1 t1 s2 t2) = "Could not find an assignment as tags "++s1++" and "++s2++" associated with types "++pretty t1++" and "++pretty t2++" do not match"
  pretty (AssignmentFailedToUnify t1 t2 ass) = "Assignment "++show [(n, pretty v)|(n, v)<-ass]++" failed to unify "++show t1++" and "++show t2
  pretty (InfiniteUnification var t) = "Cannot unify "++var++" and "++pretty t++" as "++pretty t++" would become infinite"
  pretty (PatternMismatch s t v) = "Cannot find symbol "++s++" in "++pretty t++" for value "++pretty v
  pretty (InvalidValueForType t v) = "Matched on value "++pretty v++" does not match type "++pretty t

newtype WithVars a = WithVars { unWithVars :: [Sym] -> Try (a, [Sym])}

data Try a = Failed Failure | Success a
  deriving (Show, Eq)

instance Functor Try where
  f`fmap`(Success a) = Success $ f a
  _`fmap`(Failed er) = Failed er

instance Applicative Try where
  pure = Success
  (Success f) <*> (Success x) = Success $ f x
  (Failed err) <*> _ = Failed err
  _ <*> (Failed err) = Failed err

instance Monad Try where
  return = pure
  (Success x) >>= f = f x
  (Failed err ) >>= _ = Failed err

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
varsFrom (Sum     tys) = concat [ varsFrom $ snd t | t <- tys ]
varsFrom (Product tys) = concat [ varsFrom $ snd t | t <- tys ]
varsFrom (Var     n  ) = [n]

alphaRename :: (String -> String) -> Ty -> Ty
alphaRename r (Var     n  ) = Var (r n)
alphaRename r (Sum     tys) = Sum [ (n, alphaRename r t) | (n, t) <- tys ]
alphaRename r (Product tys) = Product [ (n, alphaRename r t) | (n, t) <- tys ]

hideVar :: Sym -> WithVars ()
hideVar used = WithVars $ \vars -> Success ((), vars \\ [used])

getVar :: WithVars Ty
getVar = Var <$> getVarName

getVarName :: WithVars Sym
getVarName = WithVars $ \case
  (n : vars') -> Success (n, vars')
  []          -> Failed RanOutOfVariableNames

runWithVars :: WithVars a -> Try a
runWithVars ma = case unWithVars ma names of
  Success (res, _) -> Success res
  Failed  f        -> Failed f
 where
  names = ltrs ++ [ l ++ n | l <- ltrs, n <- names ]
  ltrs  = (: []) <$> "abcdefghijklmnopqrstuvwxyz"

instance Functor WithVars where
  f`fmap`(WithVars a) = WithVars $ \vars ->
    case a vars of
      Success (x, v) -> Success (f x, v)
      Failed err -> Failed err

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

getType :: Value -> WithVars Ty
getType (Value n v) = (\t -> Sum [(n, t)]) <$> getType v
getType (Values vs) = Product <$> mapM gt' vs
 where
  gt' :: Value -> WithVars (String, Ty)
  gt' v = do
    t <- getType v
    s <- getVarName
    return (s, t)

getZero :: Ty -> Value
getZero (Sum     []   ) = error "Tried to get Sum of []"
getZero (Sum     tys  ) = (\(s, t) -> Value s (getZero t)) $ head tys
getZero (Product tys  ) = Values [ getZero t | (_, t) <- tys ]
getZero (Var     name') = error $ "Var '" ++ name' ++ "' does not have a zero"


-- data Value = Value Sym Value | Values [(Sym, Value)]
unpackFrom :: Sym -> (Ty, Value) -> Try (Ty, Value)
unpackFrom s (t@(Sum ops), v@(Value tag val)) =
  case [ (ty, val) | (name, ty) <- ops, s == name, name == tag ] of
    []   -> Failed $ PatternMismatch s t v
    [tv] -> return tv
    _    -> Failed $ InvalidValueForType t v
unpackFrom s (t@(Product vals), v@(Values vs)) =
  case [ (ty, val) | ((name, ty), val) <- zip vals vs, s == name ] of
    []   -> Failed $ PatternMismatch s t v
    [tv] -> return tv
    _    -> Failed $ InvalidValueForType t v

unpackFrom s (t, v) = Failed $ PatternMismatch s t v

extractFrom :: [Sym] -> (Ty, Value) -> Try (Ty, Value)
extractFrom path tv = foldM (flip unpackFrom) tv path

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

assignVar :: Sym -> Ty -> WithVars Assignment
assignVar m (Var v) = do
  hideVar m
  hideVar v
  if m == v then return [] else return [(m, Var v), (v, Var m)]
assignVar m v | m `elem` varsFrom v = failDueTo $ InfiniteUnification m v
              | otherwise           = hideVar m >> return [(m, v)]

apply :: Ty -> Assignment -> WithVars Ty
apply = foldM apply1

apply1 :: Ty -> (Sym, Ty) -> WithVars Ty
apply1 _ (m, v) | m `elem` varsFrom v = failDueTo $ InfiniteUnification m v
apply1 (Var n) (m, v) | n == m    = return v
                      | otherwise = return $ Var n
apply1 (Sum     tys) k = Sum <$> apply1Map tys k
apply1 (Product tys) k = Product <$> apply1Map tys k

apply1Map :: [(Sym, Ty)] -> (Sym, Ty) -> WithVars [(Sym, Ty)]
apply1Map tys k = mapM (\(s, t) -> (s, ) <$> t `apply1` k) tys

merge :: Assignment -> Assignment -> WithVars Assignment
merge []            ys = return ys
merge ((x, v) : xs) ys = do
  mg <- merge xs ys
  case find x ys of
    Nothing -> return $ (x, v) : mg
    Just v2 ->
      if v2 == v then return mg else failDueTo $ CouldNotUnifyVar x v v2

merge' :: Assignment -> WithVars Assignment -> WithVars Assignment
merge' a ma = do
  ma' <- ma
  merge a ma'

mgu'' :: (Sym, Ty) -> (Sym, Ty) -> WithVars Assignment
mgu'' (s1, t1) (s2, t2)
  | s1 /= s2  = failDueTo $ CouldNotUnifySymbols s1 t1 s2 t2
  | otherwise = mgu' t1 t2

merge2
  :: ([(Sym, Ty)] -> Ty) -> [(Sym, Ty)] -> [(Sym, Ty)] -> WithVars Assignment
merge2 cons xs ys
  | length xs /= length ys = failDueTo $ CouldNotUnify (cons xs) (cons ys)
  | otherwise = do
    asses <- zipWithM mgu'' xs ys
    foldr merge' (return []) asses

mgu' :: Ty -> Ty -> WithVars Assignment
mgu' v            (Var m)      = assignVar m v
mgu' (Var     m ) v            = assignVar m v
mgu' (Sum     xs) (Sum     ys) = merge2 Sum xs ys
mgu' (Product xs) (Product ys) = merge2 Product xs ys
mgu' t            v            = failDueTo $ CouldNotUnify v t

applyType :: WithVars Ty -> WithVars Ty -> WithVars Ty
applyType x y = do
  x' <- x
  y' <- y
  applyType' x' y' =<< mgu' x' y'

applyType' :: Ty -> Ty -> Assignment -> WithVars Ty
applyType' x y ass = do
  x' <- apply x ass
  y' <- apply y ass
  if x' == y' then return x else failDueTo $ AssignmentFailedToUnify x' y' ass

-- TODO(cypher1): Write up an inc with types.
-- inc :: Value -> (Value, Bool)
-- inc Z = (Z, True)
-- inc (N t f s)
  -- = case t of
      -- Sum Fst -> (N (Sum (if carry' then Snd else Fst)) f' s', False)
      -- Sum Snd -> (N (Sum (if carry' then Fst else Snd)) f' s', carry')
      -- Prod -> (N Product f' s', carry')
  -- where
    -- (s', carry) = inc s
    -- (f', carry') = if carry then inc f else (f, False)

unit :: Ty
unit = Product []

void :: Ty
void = Sum []

boolean :: Ty
boolean = Sum [("0", unit), ("1", unit)]

uint32 :: Ty
uint32 = nbits 32
