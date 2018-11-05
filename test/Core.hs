module Core where

import Prelude hiding (showList)
import Distribution.TestSuite
  ( Test(Test)
  , TestInstance (TestInstance, run, name, tags, options, setOption)
  , Progress(Finished, Progress)
  , Result(Fail, Pass))
import Test.QuickCheck
import TestUtil
import Debug.Trace (trace)

import Util (showList)
import Pred (exists, creates, Atom(Value, Variable, Predicate), solutions, Assignment)
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
c = var "c"
ne = var "!="
cons = var "cons"
nil = var "nil"
list = var "list"
aNeZero = map Value [a, ne, zero]
bNeZero = map Value [b, ne, zero]
fdiv = func [T Div a b ret] (S.fromList [exists a, exists b]) (S.fromList [creates ret])
frac = addPre bNeZero fdiv
minus = func [T Sub a b ret] (S.fromList [exists a, exists b]) $ S.fromList [creates ret]
needsRet = addPre (exists ret) emp

x = var "x"
y = var "y"
z = var "z"
isa = var "isa"
varXNeZero = [Variable x, Value ne, Value zero]

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
  , mkTest "updateFrac with emp should fail" fails $ update emp frac
  , mkTest "updateFrac with b!=0 should fail " fails
    $ update (assume [bNeZero]) frac
  , mkTest "updateFrac with b!=0 and a should fail" fails
    $ update (assume [bNeZero, exists a]) frac
  , mkTest "updateFrac with b!=0 and b should fail" fails
    $ update (assume [bNeZero, exists b]) frac
  , mkTest "updateFrac with a, b should fail" fails
    $ update (assume [exists a, exists b]) frac
  , mkTest "updateFrac with b!=0, a, b, should pass" passes
    $ update (assume [exists a, exists b, bNeZero]) frac
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
  [ mkTest "Value resolution fails if there is no heap"
      hasNoSolution
    $ solutions (S.fromList []) $ S.fromList [exists a]
  , mkTest "Value resolution passes if the state contains the value"
      hasEmptySolution
    $ solutions (S.fromList [exists a]) $ S.fromList [exists a]
  , mkTest "Resolution succeeds if state contains the predicate"
      hasEmptySolution
      $ solutions (S.fromList [aNeZero]) $ S.fromList [aNeZero]
  , mkTest "Resolution fails on 1-pred with variable (no matches)"
      hasNoSolution
      $ solutions (S.fromList [exists a, exists ne, exists zero])
        $ S.fromList [varXNeZero]
  , mkTest "Resolution succeeds on 1-pred with variable (with matches)"
      (==[[(x, Value a)]])
      $ solutions (S.fromList [exists a, aNeZero])
        $ S.fromList [varXNeZero]
  , mkTest "Resolution correct on 1-pred with variable (with matches)"
      (==[[(x, Value a)]])
      $ solutions (S.fromList [exists a, exists b, aNeZero])
        $ S.fromList [varXNeZero]
  , mkTest "Resolution fails on 1-pred with variable (with matches)"
      hasNoSolution
      $ solutions (S.fromList [exists a, exists b, [Value a, Value ne, Value b]])
        $ S.fromList [exists y, [Variable x, Value ne, Variable y]]
  , mkTest "Resolution fails on 1-pred with variable (with matches)"
      hasNoSolution
      $ solutions (S.fromList [exists a, exists b, [Value a, Value ne, Value b]])
        $ S.fromList [exists x, [Variable x, Value ne, Variable y]]
  , mkTest "Resolution correct on 1-pred with variable (with matches)"
      (==[[(x, Value a), (y, Value b)]])
      $ solutions (S.fromList [exists a, exists b, [Value a, Value ne, Value b]])
        $ S.fromList [[Variable x, Value ne, Variable y]]
  , mkTest "Resolution fails on 2-pred with variable (with matches)"
      hasNoSolution
      $ solutions (S.fromList [[Value b, Value isa, Value c]])
        $ S.fromList [[Variable x, Value isa, Variable y], [Variable y, Value isa, Variable z]]
  , mkTest "Resolution fails on 2-pred with variable (with matches)"
      hasNoSolution
      $ solutions (S.fromList [[Value a, Value isa, Value b]])
        $ S.fromList [[Variable x, Value isa, Variable y], [Variable y, Value isa, Variable z]]
  , mkTest "Resolution correct on 2-pred with variable (with matches)"
      (==[[(z, Value c), (x, Value a), (y, Value b)]])
      $ solutions (S.fromList [[Value a, Value isa, Value b], [Value b, Value isa, Value c]])
        $ S.fromList [[Variable x, Value isa, Variable y], [Variable y, Value isa, Variable z]]
  , mkTest "Resolution correct on nested 1-pred"
      (==[[(S "x0", Value a), (S "x1", Value cons), (S "x2", Value b)]])
      -- (==[[(x, Predicate [Value a, Value cons, Value b])]])
      $ solutions (S.fromList [[Predicate [Value a, Value cons, Value b], Value isa, Value list]])
        $ S.fromList [[Variable x, Value isa, Value list]]
  , mkTest "Resolution correct on pattern matched nested 1-pred"
      (==[[(x, Value a), (y, Value b)]])
      $ solutions (S.fromList [[Predicate [Value a, Value cons, Value b], Value isa, Value list]])
        $ S.fromList [[Predicate [Variable x, Value cons, Variable y], Value isa, Value list]]
  --TODO(jopra): Add tests for pattern matching (nested predicates)
  --TODO(jopra): Add tests for implication
  --TODO(jopra): Add tests for skolemisation (convert forall to exists)
  ]
