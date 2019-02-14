module ParserTests where

import Test.Tasty
import Test.Tasty.HUnit

import PredParser (parseFile)

import Data.Either (isRight)

files :: [String]
files = [ "examples/t1.htr"
        , "examples/t2.htr"
        , "examples/t3.htr"
        ]

parserTests :: TestTree
parserTests = testGroup "Parser tests"
  [ parsesExamples
  -- TODO(jopra): Test more properties
  -- TODO(jopra): Test whitespace flexibility
  ]

parsesExamples :: TestTree
parsesExamples = testGroup "Parsing example files succeeds" $
  map parsesExample files

parsesExample :: String -> TestTree
parsesExample file = testCase ("Can parse example file("++file++")") $ do
  parsed <- parseFile file
  isRight parsed @?= True
