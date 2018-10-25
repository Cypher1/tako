module TestUtil where

import Pred (Assignment)

-- Test types
prints :: Show a => a -> Bool
prints = (/= "").show

debug :: a -> Bool
debug = const False

hasNoSolution :: [Assignment] -> Bool
hasNoSolution = (==[])

hasEmptySolution :: [Assignment] -> Bool
hasEmptySolution = (==[[]])
