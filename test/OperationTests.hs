module OperationTests where

import Test.Tasty
import Test.Tasty.HUnit

import Pred (val, Atom)
import Operation (Instruction(T, B, U, L), UnOp(Free), BiOp(New, Not), TriOp(Or, And, Add, Sub), exec)

-- Constants
a :: Atom a
a = val "a"
b :: Atom a
b = val "b"
c :: Atom a
c = val "c"

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
    exec (L 3 "a") [] @?= [("a", 3)]
  , testCase "Introducing a literal overwrites the old value" $
    exec (L 3 "a") [("a", 100)] @?= [("a", 3)]
  ]

freeTests :: TestTree
freeTests = testGroup "Free tests"
  [ testCase "Free empties memory" $
    exec (U Free "a") [("a", 3)] @?= []
  , testCase "Free doesn't remove un-freed vars" $
    exec (U Free "a") [("a", 3), ("b", 4)] @?= [("b", 4)]
  ]

complementTests :: TestTree
complementTests = testGroup "Complement tests"
  [ testCase "Complement 10 = -11" $
    ("ret", -11)`elem`exec (B Not "a" "ret") [("a", 10)] @?= True
  , testCase "Complement -11 = 10" $
    ("ret", 10)`elem`exec (B Not "a" "ret") [("a", -11)] @?= True
  ]

copyAssignmentTests :: TestTree
copyAssignmentTests = testGroup "Copy Assignment tests"
  [ testCase "New a b copies a into b" $
    ("b", 2)`elem`exec (B New "a" "b") [("a", 2)] @?= True
  ]

andTests :: TestTree
andTests = testGroup "And tests"
  [ testCase "And 10&3 =" $
    ("ret", 2)`elem`exec (T And "a" "b" "ret") [("a", 10), ("b", 6)] @?= True
  ]

orTests :: TestTree
orTests = testGroup "Or tests"
  [ testCase "Or 10|3 =" $
    ("ret", 14)`elem`exec (T Or "a" "b" "ret") [("a", 10), ("b", 6)] @?= True
  ]

addTests :: TestTree
addTests = testGroup "Addition tests"
  [ testCase "Add 10+3 =" $
    ("ret", 13)`elem`exec (T Add "a" "b" "ret") [("a", 10), ("b", 3)] @?= True
  , testCase "Sub 10-3 =" $
    ("ret", 7)`elem`exec (T Sub "a" "b" "ret") [("a", 10), ("b", 3)] @?= True
  ]
