module TripleTests where

import           Prelude                 hiding ( showList )
import           Test.Tasty
import           Test.Tasty.HUnit

import           TestUtil                       ( pred3
                                                , passes
                                                , fails
                                                , exists
                                                , showList
                                                )

import           Pred                           ( val
                                                , Pred
                                                , Atom
                                                )
import           Triple
import           Language ( PrimTriOpType (PrimDiv, PrimSub))
import Operation (PrimOp(T))
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

aNeZero :: Pred a
aNeZero = pred3 ne a zero

bNeZero :: Pred a
bNeZero = pred3 ne b zero

fdiv :: HTriple
fdiv = func [T PrimDiv "a" "b" "ret"]
            (S.fromList [exists a, exists b])
            (S.fromList [exists ret])

frac :: HTriple
frac = addPre bNeZero fdiv

minus :: HTriple
minus = func [T PrimSub "a" "b" "ret"] (S.fromList [exists a, exists b])
  $ S.fromList [exists ret]

needsRet :: HTriple
needsRet = addPre (exists ret) emp

tripleTests :: TestTree
tripleTests =
  testGroup "Triple tests" [expressionConstructionTests, updateExpressionTests]

expressionConstructionTests :: TestTree
expressionConstructionTests = testGroup
  "Expression Construction tests"
  [ testCase "Constants Exist" $ null (showList [zero, ret, a, b, ne]) @?= False
  , testCase "Operation for a-b contains a single instruction"
  $   op minus
  @?= [T PrimSub "a" "b" "ret"]
  , testCase "Printing unsafe a/b" $ show fdiv == "" @?= False
  , testCase "Printing safe a/b" $ show frac == "" @?= False
  , testCase "require ret" $ show needsRet == "" @?= False
  , testCase "post.assume == Set.fromList"
  $   S.fromList [aNeZero, exists a, bNeZero]
  @?= post (assume [aNeZero, exists a, bNeZero, bNeZero])
  ]

updateExpressionTests :: TestTree
updateExpressionTests = testGroup
  "Upgrade Operation tests"
  [ testCase "updateFrac with emp should fail" $ fails $ update emp frac
  , testCase "updateFrac with b!=0 should fail" $ fails $ update
    (assume [bNeZero])
    frac
  , testCase "updateFrac with b!=0 and a should fail" $ fails $ update
    (assume [bNeZero, exists a])
    frac
  , testCase "updateFrac with b!=0 and b should fail" $ fails $ update
    (assume [bNeZero, exists b])
    frac
  , testCase "updateFrac with a, b should fail" $ fails $ update
    (assume [exists a, exists b])
    frac
  , testCase "updateFrac with b!=0, a, b, should pass" $ passes $ update
    (assume [exists a, exists b, bNeZero])
    frac
  , testCase "needs ret <*> frac is unsat" $ fails $ update needsRet frac
  , testCase "frac <*> needs ret is sat" $ passes $ update frac needsRet
  ]
