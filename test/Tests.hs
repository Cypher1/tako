module Tests where

import OperationTests (operationTests)
import TripleTests (tripleTests)
import ResolutionTests (resolutionTests)
import Distribution.TestSuite
  ( Test(Test)
  , TestInstance (TestInstance)
  )

tests :: IO [Test]
tests = return $ map Test testList

-- Tests
testList :: [TestInstance]
testList
  = concat
  [ tripleTests
  , operationTests
  , resolutionTests
  ]
  -- TODO(jopra): Execute a triple
  -- TODO(jopra): Compile a triple
  -- TODO(jopra): Check separation for triples
  -- TODO(jopra): Check parallelisation for triples
  -- TODO(jopra): Enforce the KB contains no Variables without implication
