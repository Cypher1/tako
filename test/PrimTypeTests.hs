module PrimTypeTests where

import           Debug.Trace                    ( trace )
import           Data.List                      ( sort
                                                , nub
                                                )

import           Test.Tasty
-- import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck         as QC
import           PrimType

instance QC.Arbitrary Ty where
  arbitrary = QC.frequency [ (1, Sum <$> arbitraryList)
    , (100, Var <$> QC.vectorOf 1 (QC.elements "abcdefghijklmnopqrstuvwxyz"))
    , (1, Product <$> arbitraryList)
    ]
  shrink (Var _) = []
  shrink (Sum ts) = snd <$> ts -- TODO sublists should also be checked.
  shrink (Product ts) = snd <$> ts

arbitraryList :: QC.Arbitrary a => QC.Gen [a]
arbitraryList = QC.sized $ \n -> do
  k <- QC.choose (0, min 10 n)
  QC.vectorOf k QC.arbitrary

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
  p ty = case runWithVars (mgu' ty ty) of
    Success map' -> null map'
    _            -> False

propAlphaRenameUnify :: TestTree
propAlphaRenameUnify = QC.testProperty
  "type unifies with an alpha rename of itself"
  p
 where
  p :: Ty -> Bool
  p ty = case runWithVars m of
    Success map' -> null map' || trace (show map') False
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
  p v t = check' $ runWithVars $ mgu (return t) (return $ Var v)
   where
    check' (Success map') = sort map' == sort (nub exp')
     where
      exp' = (v, t) : case t of
        Var t' -> [(t', Var v)]
        _      -> []
    check' err = trace (show err) False
