module ResolutionTests where

import Distribution.TestSuite
  ( Test(Test)
  , TestInstance
  )
import TestUtil

import Pred (val, var, Atom(Predicate), solutions)
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

tests :: IO [Test]
tests = return $ map Test resolutionTests

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
      (hasSingleSolution [(x, a)])
      $ solutions (S.fromList [exists a, aNeZero])
        $ S.fromList [varXNeZero]
  , mkTest "Resolution correct on 1-pred with variable (with alternate matches)"
      (hasSingleSolution [(x, a)])
      $ solutions (S.fromList [exists a, exists b, aNeZero])
        $ S.fromList [varXNeZero]
  , mkTest "Resolution correct on 1-pred with variable"
      (hasSingleSolution [(x, a), (y, b)])
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
      (hasSingleSolution [(z, c), (x, a), (y, b)])
      $ solutions (S.fromList [pred3 isa a b, pred3 isa b c])
        $ S.fromList [pred3 isa x y, pred3 isa y z]
  , mkTest "Resolution correct on pattern matched nested 1-pred"
      (hasSingleSolution [(x, a), (y, b)])
      $ solutions (S.fromList [pred3 isa (Predicate $ pred3 cons a b) list])
        $ S.fromList [pred3 isa (Predicate $ pred3 cons x y) list]
  , mkTest "Resolution correct on nested 1-pred"
      (hasSingleSolution [(var "x", Predicate $ toPred [("#0", a), ("rel", cons), ("#1", b)])])
      $ solutions (S.fromList [pred3 isa (Predicate $pred3 cons a b) list])
        $ S.fromList [pred3 isa x list]
  --TODO(jopra): Add tests for pattern matching (nested predicates)
  --TODO(jopra): Add tests for implication
  --TODO(jopra): Add tests for skolemisation (convert forall to exists)
  ]
