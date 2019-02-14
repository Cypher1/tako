module Main where

import Test.Tasty

import ParserTests (parserTests)
import OperationTests (operationTests)
import TripleTests (tripleTests)
import ResolutionTests (resolutionTests)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ parserTests
  , operationTests
  , tripleTests
  , resolutionTests
  ]
