module Core where

import Prelude hiding (showList)
import Distribution.TestSuite
  ( Test(Test)
  , TestInstance (TestInstance, run, name, tags, options, setOption)
  , Progress(Finished, Progress)
  , Result(Fail, Pass))
import Test.QuickCheck
import TestUtil

import Util (showList, labelL, printL, fails, passes)
import Pred (exists, creates, Atom(Value, Predicate), solutions)
import Triple
import Operation
import qualified Data.Set as S

mkTest :: Show a => String -> (a -> Bool) -> a -> [String]-> TestInstance
mkTest name' check' val' tags'
  = TestInstance
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

tests :: IO [Test]
tests = return $ map Test testList

-- Constants
zero = val "0"
ret = var "ret"
a = var "a"
b = var "b"
ne = var "!="
aNeZero = map Value [a, ne, zero]
bNeZero = map Value [b, ne, zero]
fdiv = func [T Div a b ret] (S.fromList [exists a, exists b]) (S.fromList [creates ret])
frac = addPre bNeZero fdiv
minus = func [T Sub a b ret] (S.fromList [exists a, exists b]) $ S.fromList [creates ret]
needsRet = addPre (exists ret) emp

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

tripleTests :: [TestInstance]
tripleTests
  = map (addTags ["triples"])
  [ mkTest "constantsExist" (not.null) $ showList [zero, ret, a, b, ne]
  , mkTest "a-b" prints minus
  , mkTest "unsafe a/b" prints fdiv
  , mkTest "safe a/b" prints frac
  , mkTest "updateFrac with emp fails" fails $ update emp frac
  , mkTest "updateFrac with b!=0 fails" fails
    $ update (assume [bNeZero]) frac
  , mkTest "updateFrac with b!=0 and a should fail" fails
    $ update (assume [bNeZero, exists a]) frac
  , mkTest "updateFrac with b!=0, b and a should fail" fails
    $ update (assume [bNeZero, exists a, exists b]) frac
  , mkTest "updateFrac with b!=0, !=, b and a should fail" fails
    $ update (assume [bNeZero, exists a, exists b, exists ne]) frac
  , mkTest "updateFrac with b!=0, !=, 0, b and a should succeed" passes
    $ update (assume [bNeZero, exists a, exists b, exists ne, exists zero]) frac
  , mkTest "require ret" prints needsRet
  , mkTest "neets ret <*> frac is unsat" fails $ update needsRet frac
  , mkTest "frac <*> neets ret is sat" passes $ update frac needsRet
  , mkTest "post.assume == Set.fromList"
      (==S.fromList [aNeZero, exists a, bNeZero])
      (post$assume [aNeZero, exists a, bNeZero, bNeZero])
  ]

operationTests :: [TestInstance]
operationTests
  = map (addTags ["vm"])
  [ mkTest "Introducing a literal gives the same value" (==[(a, 3)])
    $ exec (L 3 a) []
  , mkTest "Introducing a literal overwrites the old value" (==[(a, 3)])
    $ exec (L 3 a) [(a, 100)]
  , mkTest "Free empties memory" (==[])
    $ exec (U Free a) [(a, 3)]
  , mkTest "Free doesn't remove un-freed vars" (==[(b, 4)])
    $ exec (U Free a) [(a, 3), (b, 4)]
  , mkTest "Complement 10 = -11" ((ret, -11)`elem`)
    $ exec (B Not a ret) [(a, 10)]
  , mkTest "Complement -11 = 10" ((ret, 10)`elem`)
    $ exec (B Not a ret) [(a, -11)]
  , mkTest "New a b copies a into b" ((b, 2)`elem`)
    $ exec (B New a b) [(a, 2)]
  , mkTest "And 10&3 =" ((ret, 2)`elem`)
    $ exec (T And a b ret) [(a, 10), (b, 6)]
  , mkTest "Or 10|3 =" ((ret, 14)`elem`)
    $ exec (T Or a b ret) [(a, 10), (b, 6)]
  , mkTest "Add 10+3 =" ((ret, 13)`elem`)
    $ exec (T Add a b ret) [(a, 10), (b, 3)]
  , mkTest "Sub 10-3 =" ((ret, 7)`elem`)
    $ exec (T Sub a b ret) [(a, 10), (b, 3)]
  ]

resolutionTests :: [TestInstance]
resolutionTests
  = map (addTags ["resolution", "predicate", "variable", "assignment"])
  [ mkTest "Value resolution fails if there is no heap" (==[])
    $ solutions (post (assume [])) $ S.fromList [exists a]
  , mkTest "Value resolution passes if the state contains the value" (==[[]])
    $ solutions (post (assume [exists a])) $ S.fromList [exists a]
  , mkTest "Predicate resolution fails if state doesn't contain the predicate" (==[])
    $ solutions (post (assume [[Value a, Value ne, Value zero]])) $ S.fromList [[Value a, Value ne, Value zero]]
  , mkTest "Predicate resolution fails if state only contains the predicate" (==[])
    $ solutions (post (assume [[Value a, Value ne, Value zero]])) $ S.fromList [[Value a, Value ne, Value zero]]
  , mkTest "Predicate resolution fails if state contains the predicate and its contents" (==[[]])
    $ solutions (post (assume [exists a, exists ne, exists zero, [Value a, Value ne, Value zero]])) $ S.fromList [[Value a, Value ne, Value zero]]
  ]
