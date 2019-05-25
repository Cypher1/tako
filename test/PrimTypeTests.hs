module PrimTypeTests where

import           Test.Tasty
-- import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck         as QC
import           PrimType

instance QC.Arbitrary Ty where
  arbitrary = QC.frequency [ (6, return Unit)
    , (1, Sum <$> QC.arbitrary <*> QC.arbitrary)
    , (10, Var <$> QC.vectorOf 1 (QC.elements "abcdefghijklmnopqrstuvwxyz"))
    , (1, Product <$> QC.arbitrary <*> QC.arbitrary)
                    ]
  shrink Unit = []
  shrink (Var _) = []
  shrink (Sum f s) = [f, s]
  shrink (Product f s) = [f, s]

primTypeTests :: TestTree
primTypeTests = testGroup "Primitive Type tests" [unificationTests]

unificationTests :: TestTree
unificationTests = testGroup
  "Unification tests"
  [ propSelfUnify
  , QC.testProperty "n bits have minsize n"
                    (\n' -> let n = n' `mod` 200 in n == minsize (nbits n))
  , QC.testProperty
    "n bits unify with m bits iff n == m"
    (\m' n' ->
      let (n, m) = (m' `mod` 200, n' `mod` 200)
      in  (n /= m) == failed (mgu' (nbits n) (nbits m))
    )
  , propSimpleUnification
  ]

propSelfUnify :: TestTree
propSelfUnify = QC.testProperty "type unifies with itself" p
 where
  p :: Ty -> Bool
  p ty = case runWithVars m of
    Success map' -> all (\(v, Var v2) -> rename v == v2) map'
    _            -> False
   where
    rename s = s ++ "2"
    m = mgu' (alphaRename rename ty) ty

propSimpleUnification :: TestTree
propSimpleUnification = QC.testProperty
  "variable unified with type is assigned that type"
  p
 where
  p :: String -> Ty -> Bool
  p v t = case runWithVars $ mgu (return t) (return $ Var v) of
    Success map' -> map' == [(v, t)]
    _            -> False
