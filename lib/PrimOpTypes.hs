module PrimOpTypes where

import           Util                           ( boundedAll
                                                , Pretty(..)
                                                )

import           Language                       ( PrimValOpType(..)
                                                , PrimUnOpType(..)
                                                , PrimBiOpType(..)
                                                , PrimTriOpType(..)
                                                )

type Sym = String

type Val = Integer

data PrimOp
  = L PrimValOpType Val Sym
  | U PrimUnOpType Sym
  | B PrimBiOpType Sym Sym
  | T PrimTriOpType Sym Sym Sym
  deriving (Show, Eq, Ord)

instance Pretty PrimOp where
  pretty (L op v x) = pretty op++show v++" "++x
  pretty (U op x) = pretty op++" "++x
  pretty (B op x y) = pretty op++" "++x++" "++y
  pretty (T op x y z) = pretty op++" "++x++" "++y++" "++z

unops :: [String]
unops = show <$> (boundedAll :: [PrimUnOpType])

biops :: [String]
biops = show <$> (boundedAll :: [PrimBiOpType])

triops :: [String]
triops = show <$> (boundedAll :: [PrimTriOpType])

type Op = [PrimOp]

type Mem = [(Sym, Val)]
