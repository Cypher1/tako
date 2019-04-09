module ResolutionTests where

import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtil

import           Pred                           ( val
                                                , var
                                                , Var
                                                , Pred
                                                , Atom(Predicate)
                                                )
import           Resolution                     ( solutions )
import qualified Data.Set                      as S

-- Constants
zero :: Atom a
zero = val "0"

a :: Atom a
a = val "a"

b :: Atom a
b = val "b"

c :: Atom a
c = val "c"

ret :: Atom a
ret = val "ret"

ne :: Atom a
ne = val "!="

cons :: Atom a
cons = val "cons"

list :: Atom a
list = val "list"

bNeZero :: Pred a
bNeZero = pred3 ne b zero

aNeb :: Pred a
aNeb = pred3 ne a b

x :: Atom Var
x = var "x"

y :: Atom Var
y = var "y"

z :: Atom Var
z = var "z"

isa :: Atom a
isa = val "isa"

resolutionTests :: TestTree
resolutionTests = testGroup
  "Resolution tests"
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
emptyHeapletTests = testGroup
  "Empty heaplet tests"
  [ testCase "Value resolution fails if there is no heap"
    $ hasNoSolution
    $ solutions S.empty
    $ S.fromList [exists a]
  ]

singlePredicateResolution :: TestTree
singlePredicateResolution = testGroup
  "Single Predicate Resolution"
  [ testCase "Value resolution passes if the state contains the value"
  $ hasEmptySolution
  $ solutions (S.fromList [exists a])
  $ S.fromList [exists a]
  , testCase "Resolution succeeds if state contains the predicate"
  $ hasEmptySolution
  $ solutions (S.fromList [bNeZero])
  $ S.fromList [bNeZero]
  , testCase "Resolution fails on 1-pred with variable (no matches)"
  $ hasNoSolution
  $ solutions (S.fromList [exists a, exists ne, exists zero])
  $ S.fromList [pred3 ne x zero]
  , testCase "Resolution succeeds on 1-pred (with matches)"
  $ hasSingleSolution [(x, b)]
  $ solutions (S.fromList [exists b, bNeZero])
  $ S.fromList [pred3 ne x zero]
  , testCase "Resolution correct on 1-pred (with alternate matches)"
  $ hasSingleSolution [(x, b)]
  $ solutions (S.fromList [exists a, exists b, bNeZero])
  $ S.fromList [pred3 ne x zero]
  , testCase "Resolution correct on 1-pred with variable"
  $ hasSingleSolution [(x, a), (y, b)]
  $ solutions (S.fromList [exists a, exists b, aNeb])
  $ S.fromList [pred3 ne x y]
  , testCase "Resolution fails on 1-pred with variable (a)"
  $ hasNoSolution
  $ solutions (S.fromList [exists a, aNeb])
  $ S.fromList [exists x, exists y, pred3 ne x y]
  , testCase "Resolution fails on 1-pred with variable (b)"
  $ hasNoSolution
  $ solutions (S.fromList [exists b, aNeb])
  $ S.fromList [exists x, exists y, pred3 ne x y]
  ]

nestedPredicateTests :: TestTree
nestedPredicateTests = testGroup
  "Nested predicate matching tests"
  --TODO(jopra): More tests for pattern matching (nested predicates)
  --TODO(jopra): Add tests for joins inside deeply nested predicates
  [ testCase "Resolution fails on nested 1-pred without match"
  $ hasNoSolution
  $ solutions (S.fromList [pred3 ne (Predicate $ pred3 cons a b) list])
  $ S.fromList [pred3 isa x list]
  , testCase "Resolution correct on pattern matched nested 1-pred"
  $ hasSingleSolution [(x, a), (y, b)]
  $ solutions (S.fromList [pred3 isa (Predicate $ pred3 cons a b) list])
  $ S.fromList [pred3 isa (Predicate $ pred3 cons x y) list]
  , testCase "Resolution correct on nested 1-pred"
  $ hasSingleSolution
      [(var "x", Predicate $ toPred [("#0", a), ("rel", cons), ("#1", b)])]
  $ solutions (S.fromList [pred3 isa (Predicate $ pred3 cons a b) list])
  $ S.fromList [pred3 isa x list]
  ]

joinResolutionTests :: TestTree
joinResolutionTests = testGroup
  "Join resolution tests"
  [ testCase "Resolution fails on 2-pred with variable"
  $ hasNoSolution
  $ solutions (S.fromList [pred3 isa b c])
  $ S.fromList [pred3 isa x y, pred3 isa y z]
  , testCase "Resolution fails on 2-pred with variable"
  $ hasNoSolution
  $ solutions (S.fromList [pred3 isa a b])
  $ S.fromList [pred3 isa x y, pred3 isa y z]
  , testCase "Resolution correct on 2-pred with variable"
  $ hasSingleSolution [(z, c), (x, a), (y, b)]
  $ solutions (S.fromList [pred3 isa a b, pred3 isa b c])
  $ S.fromList [pred3 isa x y, pred3 isa y z]
  ]
