module TestUtil where

import Data.Either (isLeft, isRight)
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

fails :: Either a b -> Bool
fails = isRight

passes :: Either a b -> Bool
passes = isLeft
