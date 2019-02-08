module Main where

import Test.Tasty

import ParserTests (parserTests)
-- import OperationTests (operationTests)
-- import ResolutionTests (resolutionTests)
-- import TripleTests (tripleTests)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ parserTests
  ]
