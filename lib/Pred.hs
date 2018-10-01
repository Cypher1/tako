module Pred where

import qualified Data.Set as S
import Data.Set (Set)

import Data.List ((\\))
import Operation (Sym (S), Instruction, Op)

-- TODO(jopra): Use something more ... useful?
type Pred = [Sym]

-- TODO(jopra): Consider new typing pre vs post conditions to ensure they aren't mixed up
exists :: Sym -> Pred
exists v = [v]

creates :: Sym -> Pred
creates v = [v]

type Check = [Pred]

type State = Set Pred
emptyState :: State
emptyState = S.empty

findMissing :: State -> Pred -> [Sym]
findMissing sh pred = map head $ atoms_needed \\ S.toList sh
  where
    atoms_needed = map (:[]) pred

-- TODO(jopra): Should return a proper error type, too much work is being done here
resolved :: State -> Pred -> Bool
resolved sh pred
-- can't prove things without their parts existing
  | findMissing sh pred /= [] = False -- trace ("TODO(jopra): Missing "++show(findMissing sh pred)) False
  | pred `elem` sh = True -- we know what we know
-- proofs... are hard...
  | otherwise = True -- trace ("TODO(jopra): Find "++show pred++" in "++show sh) True
