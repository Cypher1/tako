module Operation where

data Sym = S String -- deriving (Show, Eq, Ord)
  deriving (Eq, Ord)
instance Show Sym where
  show (S s) = s

data Instruction
  = And Sym Sym Sym
  | Or  Sym Sym Sym
  | Not Sym Sym
  | Add Sym Sym Sym
  | Sub Sym Sym Sym
  | Div Sym Sym Sym
  | New Sym Sym
  | Free Sym
  deriving (Show, Eq, Ord)

type Op = [Instruction]
