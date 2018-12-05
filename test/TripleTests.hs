module TripleTests where

import Prelude hiding (showList)
import Distribution.TestSuite (TestInstance)
import TestUtil

import Util (showList)
import Pred (exists, val, var, toPred, Pred (Pred), Atom(Predicate), solutions, solutionsAndErrors, Assignment, System(Partial))
import Triple
import Operation (Instruction(T), TriOp(Div, Sub))
import qualified Data.Set as S

pred3 :: Atom a -> Atom a -> Atom a -> Pred a
pred3 r x y = toPred [("#0", x), ("rel", r), ("#1", y)]

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
