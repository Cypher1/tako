module TestUtil where
import Distribution.TestSuite
  ( Test(Test)
  , TestInstance (TestInstance, run, name, tags, options, setOption)
  , Progress(Finished, Progress)
  , Result(Fail, Pass))

import Debug.Trace
import Data.Either (isLeft, isRight)
import qualified Data.Map as M

import Pred (Assignment, emptyAssignment, Atom)
import Operation (Sym)

-- Test types
prints :: Show a => a -> Bool
prints = (/= "").show

debug :: a -> Bool
debug = const False

hasNoSolution :: [Assignment] -> Bool
hasNoSolution = (==[])

hasEmptySolution :: [Assignment] -> Bool
hasEmptySolution = (==[emptyAssignment])

hasSingleSolution :: [(Sym, Atom)] -> [Assignment] -> Bool
hasSingleSolution req = hasOnlySolutions [req]

hasOnlySolutions :: [[(Sym, Atom)]] -> [Assignment] -> Bool
hasOnlySolutions reqs = (==)(map M.fromList reqs)

mkTest :: Show a => String -> (a -> Bool) -> a -> [String]-> TestInstance
mkTest name' check' val' tags'
  = trace ("---"++name'++"---") $ TestInstance
    { run = return $ if check' val'
                        then Finished Pass
                        else Finished $ Fail $ show val'
    , name = name'
    , tags = tags'
    , options = []
    , setOption = \opN op -> Right $ mkTest name' check' val' tags'
    }

addTags :: [String] -> ([String] -> TestInstance) -> TestInstance
addTags t i = i t

fails :: Either a b -> Bool
fails = isLeft

passes :: Either a b -> Bool
passes = isRight
