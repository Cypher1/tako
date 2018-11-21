module Core where

import Prelude hiding (showList)
import Distribution.TestSuite
  ( Test(Test)
  , TestInstance (TestInstance, run, name, tags, options, setOption)
  , Progress(Finished, Progress)
  , Result(Fail, Pass))
import Test.QuickCheck
import TestUtil

import Util (showList)
import Pred (exists, val, var, toPred, Pred, Atom(Predicate), solutions, Assignment)
import Triple
import Operation
import qualified Data.Set as S

tests :: IO [Test]
tests = return $ map Test testList

pred3 r x y = toPred [("#0", x), ("rel", r), ("#1", y)]

-- Constants
zero = val "0"
a = val "a"
b = val "b"
c = val "c"
ret = val "ret"
pa = S "a"
pb = S "b"
pRet = S "ret"
ne = val "!="
ne' :: Atom -> Atom -> Pred
ne' = pred3 ne
cons = val "cons"
nil = val "nil"
list = val "list"
aNeZero = ne' a zero
bNeZero = ne' b zero
aNeb = ne' a b
fdiv = func [T Div pa pb pRet] (S.fromList [exists a, exists b]) (S.fromList [exists ret])
frac = addPre bNeZero fdiv
minus = func [T Sub pa pb pRet] (S.fromList [exists a, exists b]) $ S.fromList [exists ret]
needsRet = addPre (exists ret) emp

x = var "x"
px = S "x"
y = var "y"
py = S "y"
z = var "z"
pz = S "z"
isa = val "isa"
isa' = pred3 isa

varXNeZero = ne' x zero
xNeY = ne' x y

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

tripleTests :: [TestInstance]
tripleTests
  = map (addTags ["triples"])
  [ mkTest "constantsExist" (not.null) $ showList [zero, ret, a, b, ne]
  , mkTest "a-b" prints minus
  , mkTest "unsafe a/b" prints fdiv
  , mkTest "safe a/b" prints frac
  , mkTest "updateFrac with emp should fail" fails $ update emp frac
  , mkTest "updateFrac with b!=0 should fail" fails
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
  , mkTest "needs ret <*> frac is unsat" fails $ update needsRet frac
  , mkTest "frac <*> needs ret is sat" passes $ update frac needsRet
  , mkTest "post.assume == Set.fromList"
      (==S.fromList [aNeZero, exists a, bNeZero])
      (post$assume [aNeZero, exists a, bNeZero, bNeZero])
  ]

operationTests :: [TestInstance]
operationTests
  = map (addTags ["vm"])
  [ mkTest "Introducing a literal gives the same value" (==[(pa, 3)])
    $ exec (L 3 pa) []
  , mkTest "Introducing a literal overwrites the old value" (==[(pa, 3)])
    $ exec (L 3 pa) [(pa, 100)]
  , mkTest "Free empties memory" (==[])
    $ exec (U Free pa) [(pa, 3)]
  , mkTest "Free doesn't remove un-freed vars" (==[(pb, 4)])
    $ exec (U Free pa) [(pa, 3), (pb, 4)]
  , mkTest "Complement 10 = -11" ((pRet, -11)`elem`)
    $ exec (B Not pa pRet) [(pa, 10)]
  , mkTest "Complement -11 = 10" ((pRet, 10)`elem`)
    $ exec (B Not pa pRet) [(pa, -11)]
  , mkTest "New a b copies a into b" ((pb, 2)`elem`)
    $ exec (B New pa pb) [(pa, 2)]
  , mkTest "And 10&3 =" ((pRet, 2)`elem`)
    $ exec (T And pa pb pRet) [(pa, 10), (pb, 6)]
  , mkTest "Or 10|3 =" ((pRet, 14)`elem`)
    $ exec (T Or pa pb pRet) [(pa, 10), (pb, 6)]
  , mkTest "Add 10+3 =" ((pRet, 13)`elem`)
    $ exec (T Add pa pb pRet) [(pa, 10), (pb, 3)]
  , mkTest "Sub 10-3 =" ((pRet, 7)`elem`)
    $ exec (T Sub pa pb pRet) [(pa, 10), (pb, 3)]
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
      (hasSingleSolution [(px, a)])
      $ solutions (S.fromList [exists a, aNeZero])
        $ S.fromList [varXNeZero]
  , mkTest "Resolution correct on 1-pred with variable (with alternate matches)"
      (hasSingleSolution [(px, a)])
      $ solutions (S.fromList [exists a, exists b, aNeZero])
        $ S.fromList [varXNeZero]
  , mkTest "Resolution correct on 1-pred with variable"
      (hasSingleSolution [(px, a), (py, b)])
      $ solutions (S.fromList [exists a, exists b, aNeb])
        $ S.fromList [xNeY]
  , mkTest "Resolution fails on 1-pred with variable"
      hasNoSolution
      $ solutions (S.fromList [exists b, aNeb])
        $ S.fromList [exists x, exists y, xNeY]
  , mkTest "Resolution fails on 1-pred with variable"
      hasNoSolution
      $ solutions (S.fromList [exists a, aNeb])
        $ S.fromList [exists x, exists y, xNeY]
  , mkTest "Resolution fails on nested 1-pred without match"
      hasNoSolution
      -- (==[[(x, Predicate [a, cons, b])]])
      $ solutions (S.fromList [pred3 ne (Predicate $pred3 cons a b) list])
        $ S.fromList [pred3 isa x list]
  , mkTest "Resolution fails on 2-pred with variable"
      hasNoSolution
      $ solutions (S.fromList [isa' b c])
        $ S.fromList [pred3 isa x y, pred3 isa y z]
  , mkTest "Resolution fails on 2-pred with variable"
      hasNoSolution
      $ solutions (S.fromList [pred3 isa a b])
        $ S.fromList [pred3 isa x y, pred3 isa y z]
  , mkTest "Resolution correct on 2-pred with variable"
      (hasSingleSolution [(pz, c), (px, a), (py, b)])
      $ solutions (S.fromList [pred3 isa a b, pred3 isa b c])
        $ S.fromList [pred3 isa x y, pred3 isa y z]
  , mkTest "Resolution correct on pattern matched nested 1-pred"
      (hasSingleSolution [(px, a), (py, b)])
      $ solutions (S.fromList [pred3 isa (Predicate $ pred3 cons a b) list])
        $ S.fromList [pred3 isa (Predicate $ pred3 cons x y) list]
  , mkTest "Resolution correct on nested 1-pred"
      (hasSingleSolution [(S "x.#0", a), (S "x.rel", cons), (S "x.#1", b)])
      -- (==[[(x, Predicate [a, cons, b])]])
      $ solutions (S.fromList [pred3 isa (Predicate $pred3 cons a b) list])
        $ S.fromList [pred3 isa x list]
  --TODO(jopra): Add tests for pattern matching (nested predicates)
  --TODO(jopra): Add tests for implication
  --TODO(jopra): Add tests for skolemisation (convert forall to exists)
  ]
