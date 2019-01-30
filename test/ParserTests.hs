module ParserTests where

import Distribution.TestSuite (Test(Test))

import PredParser (parseFile)
import TestUtil

files :: [String]
files = [ "examples/t1.htr"
        , "examples/t2.htr"
        , "examples/t3.htr"
        ]

parseExample :: String -> UntaggedIOTestInstance
parseExample file = mkTestIO name passes $ parseFile file
  where
    name = ("Can parse example file("++file++")")

tests :: IO [Test]
tests = mapM runTest files
  where
    runTest :: String -> IO Test
    runTest file = Test <$> parseExample file ["parser", "IO"]
