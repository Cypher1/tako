module TripleTests where

import Prelude hiding (showList)
import Test.Tasty
import Test.Tasty.HUnit

import TestUtil (pred3, passes, fails, exists, showList)

import Pred (val, var)
import Triple
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

tripleTests :: TestTree
tripleTests
  = testGroup "Triple tests"
    [ expressionConstructionTests
    , updateExpressionTests
    ]

expressionConstructionTests :: TestTree
expressionConstructionTests = testGroup "Expression Construction tests"
  [ testCase "Constants Exist" $
    null(showList [zero, ret, a, b, ne]) @?= False
  , testCase "Operation for a-b contains a single instruction" $
    op minus @?= [T Sub "a" "b" "ret"]
  , testCase "Printing unsafe a/b" $
    show fdiv == "" @?= False
  , testCase "Printing safe a/b" $
    show frac == "" @?= False
  , testCase "require ret" $
    show needsRet == "" @?= False
  , testCase "post.assume == Set.fromList" $
    S.fromList [aNeZero, exists a, bNeZero] @?=
      post $ assume [aNeZero, exists a, bNeZero, bNeZero]
  ]

updateExpressionTests :: TestTree
updateExpressionTests = testGroup "Upgrade Operation tests"
  [ testCase "updateFrac with emp should fail" $
    fails $ update emp frac
  , testCase "updateFrac with b!=0 should fail" $
    fails $ update (assume [bNeZero]) frac
  , testCase "updateFrac with b!=0 and a should fail" $
    fails $ update (assume [bNeZero, exists a]) frac
  , testCase "updateFrac with b!=0 and b should fail" $
    fails $ update (assume [bNeZero, exists b]) frac
  , testCase "updateFrac with a, b should fail" $
    fails $ update (assume [exists a, exists b]) frac
  , testCase "updateFrac with b!=0, a, b, should pass" $
    passes $ update (assume [exists a, exists b, bNeZero]) frac
  , testCase "needs ret <*> frac is unsat" $
    fails $ update needsRet frac
  , testCase "frac <*> needs ret is sat" $
    passes $ update frac needsRet
  ]
