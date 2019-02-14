module TestUtil where

import qualified Data.Map as M

import Pred (Assignment, Var, Val, Pred(Pred), Atom(Variable))
import Test.Tasty ()
import Test.Tasty.HUnit (assertFailure, Assertion, (@?=))

showList :: Show a => [a] -> String
showList xs = drop (length joiner) $ concatMap (\x->joiner++show x) xs
  where
    joiner = ", "

toPred :: [(String, Atom a)] -> Pred a
toPred xs = Pred $ M.fromList $ map (\(x,y) -> (Variable x, y)) xs

pred3 :: Atom a -> Atom a -> Atom a -> Pred a
pred3 r x y = toPred [("#0", x), ("rel", r), ("#1", y)]

exists :: Atom a -> Pred a
exists v = toPred [("exists", v)]

hasNoSolution :: Eq (Atom a) => [Assignment a] -> Assertion
hasNoSolution sols= sols @?=[]

hasEmptySolution :: Eq (Atom a) => [Assignment a] -> Assertion
hasEmptySolution sols
  = sols @?= [mempty]

hasSingleSolution :: [(Atom Var, Atom Val)] -> [Assignment Val] -> Assertion
hasSingleSolution req = hasOnlySolutions [req]

hasOnlySolutions :: [[(Atom Var, Atom Val)]] -> [Assignment Val] -> Assertion
hasOnlySolutions reqs sols = sols @?= map M.fromList reqs

passes :: Show a => Either a b -> IO ()
passes (Left err) = assertFailure $ "Function returned an error: "++show err
passes (Right _) = return ()

fails :: Either a b -> IO ()
fails (Left _) = return ()
fails (Right _) = assertFailure "Function was expected to return an error"
