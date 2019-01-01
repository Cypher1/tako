module OperationTests where

import Distribution.TestSuite
  ( Test(Test)
  , TestInstance
  )
import TestUtil

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

tests :: IO [Test]
tests = return $ map Test operationTests

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
