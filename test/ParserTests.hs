module ParserTests where

import           Test.Tasty
import           Test.Tasty.HUnit

import           HtripleParser                  ( parseFile )

files :: [String]
files =
  ["examples/t1.htr", "examples/t2.htr", "examples/t3.htr", "examples/t4.htr"]

parserTests :: TestTree
parserTests =
  testGroup "Parser tests" [parsesExamples
  -- TODO(jopra): Test more properties
  -- TODO(jopra): Test whitespace flexibility
                                          ]

parsesExamples :: TestTree
parsesExamples =
  testGroup "Parsing example files succeeds" $ map parsesExample files

parsesExample :: String -> TestTree
parsesExample file = testCase ("Can parse example file(" ++ file ++ ")") $ do
  parsed <- parseFile file
  assertBool (show parsed) (and $ zipWith (==) "Scope [" (show parsed))
