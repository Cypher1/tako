module Pred where

import qualified Data.Set as S
import Data.Set (Set)

import Operation (Sym (S), Instruction, Op)

-- TODO(jopra): Use something more ... useful?
type Pred = [Sym]

type Check = [Pred]

type State = Set Pred
emptyState :: State
emptyState = S.empty
