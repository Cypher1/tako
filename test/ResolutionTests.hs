module ResolutionTests where

import Test.Tasty
import Test.Tasty.HUnit
import TestUtil

import Pred (val, var, Atom(Predicate))
import Resolution (solutions)
import Triple (addPre, emp, func)
import Operation (Instruction(T), TriOp(Div, Sub))
import qualified Data.Set as S

-- Constants
zero = val "0"
a = val "a"
b = val "b"
c = val "c"
ret = val "ret"
pa = "a"
pb = "b"
pRet = "ret"
ne = val "!="
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
px = "x"
y = var "y"
py = "y"
z = var "z"
pz = "z"
isa = val "isa"
isa' = pred3 isa

varXNeZero = ne' x zero
xNeY = ne' x y

resolutionTests :: TestTree
resolutionTests = testGroup "Resolution tests"
  [ emptyHeapletTests
  , singlePredicateResolution
  , nestedPredicateTests
  , joinResolutionTests
  --TODO(jopra): Add tests for implication
  --TODO(jopra): Add tests for empty heaplets
  --TODO(jopra): Add tests for regions
  --TODO(jopra): Add tests for skolemisation (convert forall to exists)
  --TODO(jopra): Add tests for joins over multiple predicates
  ]

emptyHeapletTests :: TestTree
emptyHeapletTests = testGroup "Empty heaplet tests"
  [ testCase "Value resolution fails if there is no heap" $
      hasNoSolution $ solutions S.empty $ S.fromList [exists a]
  ]

singlePredicateResolution :: TestTree
singlePredicateResolution = testGroup "Single Predicate Resolution"
  [ testCase "Value resolution passes if the state contains the value" $
      hasEmptySolution
    $ solutions (S.fromList [exists a]) $ S.fromList [exists a]
  , testCase "Resolution succeeds if state contains the predicate" $
      hasEmptySolution
      $ solutions (S.fromList [aNeZero]) $ S.fromList [aNeZero]
  , testCase "Resolution fails on 1-pred with variable (no matches)" $
      hasNoSolution
      $ solutions (S.fromList [exists a, exists ne, exists zero])
        $ S.fromList [varXNeZero]
  , testCase "Resolution succeeds on 1-pred (with matches)" $
      hasSingleSolution [(x, a)]
      $ solutions (S.fromList [exists a, aNeZero])
        $ S.fromList [varXNeZero]
  , testCase "Resolution correct on 1-pred (with alternate matches)" $
      hasSingleSolution [(x, a)]
      $ solutions (S.fromList [exists a, exists b, aNeZero])
        $ S.fromList [varXNeZero]
  , testCase "Resolution correct on 1-pred with variable" $
      hasSingleSolution [(x, a), (y, b)]
      $ solutions (S.fromList [exists a, exists b, aNeb])
        $ S.fromList [xNeY]
  , testCase "Resolution fails on 1-pred with variable (a)" $
      hasNoSolution
      $ solutions (S.fromList [exists a, aNeb])
        $ S.fromList [exists x, exists y, xNeY]
  , testCase "Resolution fails on 1-pred with variable (b)" $
      hasNoSolution
      $ solutions (S.fromList [exists b, aNeb])
        $ S.fromList [exists x, exists y, xNeY]
  ]

nestedPredicateTests :: TestTree
nestedPredicateTests = testGroup "Nested predicate matching tests"
  --TODO(jopra): More tests for pattern matching (nested predicates)
  --TODO(jopra): Add tests for joins inside deeply nested predicates
  [ testCase "Resolution fails on nested 1-pred without match" $
      hasNoSolution
      $ solutions (S.fromList [pred3 ne (Predicate $pred3 cons a b) list])
        $ S.fromList [pred3 isa x list]
  , testCase "Resolution correct on pattern matched nested 1-pred" $
      hasSingleSolution [(x, a), (y, b)]
      $ solutions (S.fromList [pred3 isa (Predicate $ pred3 cons a b) list])
        $ S.fromList [pred3 isa (Predicate $ pred3 cons x y) list]
  , testCase "Resolution correct on nested 1-pred" $
      hasSingleSolution
      [(var "x", Predicate $ toPred [("#0", a), ("rel", cons), ("#1", b)])]
      $ solutions (S.fromList [pred3 isa (Predicate $pred3 cons a b) list])
        $ S.fromList [pred3 isa x list]
  ]

joinResolutionTests :: TestTree
joinResolutionTests = testGroup "Join resolution tests"
  [ testCase "Resolution fails on 2-pred with variable" $
      hasNoSolution
      $ solutions (S.fromList [isa' b c])
        $ S.fromList [pred3 isa x y, pred3 isa y z]
  , testCase "Resolution fails on 2-pred with variable" $
      hasNoSolution
      $ solutions (S.fromList [pred3 isa a b])
        $ S.fromList [pred3 isa x y, pred3 isa y z]
  , testCase "Resolution correct on 2-pred with variable" $
      hasSingleSolution [(z, c), (x, a), (y, b)]
      $ solutions (S.fromList [pred3 isa a b, pred3 isa b c])
        $ S.fromList [pred3 isa x y, pred3 isa y z]
  ]
