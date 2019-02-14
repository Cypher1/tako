module OperationTests where

import Test.Tasty
import Test.Tasty.HUnit

import Pred (val, var)
import Operation (Instruction(T, B, U, L), UnOp(Free), BiOp(New, Not), TriOp(Or, And, Add, Sub), exec)

-- Constants
zero = val "0"
a = val "a"
b = val "b"
c = val "c"
ret = val "ret"
pa = "a"
pb = "b"
pRet = "ret"
cons = val "cons"
nil = val "nil"
list = val "list"

x = var "x"
px = "x"
y = var "y"
py = "y"
z = var "z"
pz = "z"

operationTests :: TestTree
operationTests
  = testGroup "Operation tests"
    [ declarationTests
    , freeTests
    , complementTests
    , copyAssignmentTests
    ]

declarationTests :: TestTree
declarationTests = testGroup "Declaration tests"
  [ testCase "Introducing a literal gives the same value" $
    exec (L 3 pa) [] @?= [(pa, 3)]
  , testCase "Introducing a literal overwrites the old value" $
    exec (L 3 pa) [(pa, 100)] @?= [(pa, 3)]
  ]

freeTests :: TestTree
freeTests = testGroup "Free tests"
  [ testCase "Free empties memory" $
    exec (U Free pa) [(pa, 3)] @?= []
  , testCase "Free doesn't remove un-freed vars" $
    exec (U Free pa) [(pa, 3), (pb, 4)] @?= [(pb, 4)]
  ]

complementTests :: TestTree
complementTests = testGroup "Complement tests"
  [ testCase "Complement 10 = -11" $
    (pRet, -11)`elem`exec (B Not pa pRet) [(pa, 10)] @?= True
  , testCase "Complement -11 = 10" $
    (pRet, 10)`elem`exec (B Not pa pRet) [(pa, -11)] @?= True
  ]

copyAssignmentTests :: TestTree
copyAssignmentTests = testGroup "Copy Assignment tests"
  [ testCase "New a b copies a into b" $
    (pb, 2)`elem`exec (B New pa pb) [(pa, 2)] @?= True
  ]

andTests :: TestTree
andTests = testGroup "And tests"
  [ testCase "And 10&3 =" $
    (pRet, 2)`elem`exec (T And pa pb pRet) [(pa, 10), (pb, 6)] @?= True
  ]

orTests :: TestTree
orTests = testGroup "Or tests"
  [ testCase "Or 10|3 =" $
    (pRet, 14)`elem`exec (T Or pa pb pRet) [(pa, 10), (pb, 6)] @?= True
  ]

addTests :: TestTree
addTests = testGroup "Addition tests"
  [ testCase "Add 10+3 =" $
    (pRet, 13)`elem`exec (T Add pa pb pRet) [(pa, 10), (pb, 3)] @?= True
  , testCase "Sub 10-3 =" $
    (pRet, 7)`elem`exec (T Sub pa pb pRet) [(pa, 10), (pb, 3)] @?= True
  ]
