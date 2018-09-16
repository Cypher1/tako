module Pred where

data Sym = String -- TODO(jopra): Use something more ... useful?

type Scope = [Sym]

data Pred = Pred { ins :: Scope, requires :: Scope, outs :: Scope, rels :: [Sym]}

type Check = [Pred]
