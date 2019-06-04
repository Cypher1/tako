module OperationTests where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Pred                           ( val
                                                , Atom
                                                )
import           Language                       ( PrimValOpType(..)
                                                , PrimUnOpType(..)
                                                , PrimBiOpType(..)
                                                , PrimTriOpType(..)
                                                )
import           PrimOpType                     ( PrimOp(..) )
import           PrimType                       ( iToV )
import           Operation                      ( exec )

-- Constants
a :: Atom a
a = val "a"
b :: Atom a
b = val "b"
c :: Atom a
c = val "c"

operationTests :: TestTree
operationTests = testGroup
  "Operation tests"
  [declarationTests, freeTests, complementTests, copyAssignmentTests]

declarationTests :: TestTree
declarationTests = testGroup
  "Declaration tests"
  [ testCase "Introducing a literal gives the same value"
  $   exec (L PrimLoad (iToV 3) "a") []
  @?= [("a", iToV 3)]
  , testCase "Introducing a literal overwrites the old value"
  $   exec (L PrimLoad (iToV 3) "a") [("a", iToV 100)]
  @?= [("a", iToV 3)]
  ]

freeTests :: TestTree
freeTests = testGroup
  "Free tests"
  [ testCase "Free empties memory"
  $   exec (U PrimFree "a") [("a", iToV 3)]
  @?= []
  , testCase "Free doesn't remove un-freed vars"
  $   exec (U PrimFree "a") [("a", iToV 3), ("b", iToV 4)]
  @?= [("b", iToV 4)]
  ]

complementTests :: TestTree
complementTests = testGroup
  "Complement tests"
  [ testCase "Complement 10 = -11"
  $      ("ret", iToV (-11))
  `elem` exec (B PrimNot "a" "ret") [("a", iToV 10)]
  @?=    True
  , testCase "Complement -11 = 10"
  $      ("ret", iToV 10)
  `elem` exec (B PrimNot "a" "ret") [("a", iToV (-11))]
  @?=    True
  ]

copyAssignmentTests :: TestTree
copyAssignmentTests = testGroup
  "Copy Assignment tests"
  [ testCase "New a b copies a into b"
    $      ("b", iToV 2)
    `elem` exec (B PrimNew "a" "b") [("a", iToV 2)]
    @?=    True
  ]

andTests :: TestTree
andTests = testGroup
  "And tests"
  [ testCase "And 10&3 ="
    $      ("ret", iToV 2)
    `elem` exec (T PrimAnd "a" "b" "ret") [("a", iToV 10), ("b", iToV 6)]
    @?=    True
  ]

orTests :: TestTree
orTests = testGroup
  "Or tests"
  [ testCase "Or 10|3 ="
    $      ("ret", iToV 14)
    `elem` exec (T PrimOr "a" "b" "ret") [("a", iToV 10), ("b", iToV 6)]
    @?=    True
  ]

addTests :: TestTree
addTests = testGroup
  "Addition tests"
  [ testCase "Add 10+3 ="
  $      ("ret", iToV 13)
  `elem` exec (T PrimAdd "a" "b" "ret") [("a", iToV 10), ("b", iToV 3)]
  @?=    True
  , testCase "Sub 10-3 ="
  $      ("ret", iToV 7)
  `elem` exec (T PrimSub "a" "b" "ret") [("a", iToV 10), ("b", iToV 3)]
  @?=    True
  ]
-- TODO(cypher1): Write value ops!
