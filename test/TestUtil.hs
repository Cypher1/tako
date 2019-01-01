module TestUtil where
import Distribution.TestSuite
  ( TestInstance (TestInstance, run, name, tags, options, setOption)
  , Progress(Finished)
  , Result(Fail, Pass))

import Debug.Trace
import Data.Either (isLeft, isRight)
import qualified Data.Map as M

import Util (onPair)
import Pred (Assignment, Var, Val, Pred(Pred), Atom(Variable))

toPred :: [(String, Atom a)] -> Pred a
toPred xs = Pred $ M.fromList $ map (onPair Variable id) xs

pred3 :: Atom a -> Atom a -> Atom a -> Pred a
pred3 r x y = toPred [("#0", x), ("rel", r), ("#1", y)]

-- Test types
prints :: Show a => a -> Bool
prints = (/= "").show

debug :: a -> Bool
debug = const False

hasNoSolution :: Eq (Assignment a) => [Assignment a] -> Bool
hasNoSolution = (==[])

hasEmptySolution :: Eq (Assignment a) => [Assignment a] -> Bool
hasEmptySolution = (==[mempty])

hasSingleSolution :: [(Atom Var, Atom Val)] -> [Assignment Val] -> Bool
hasSingleSolution req = hasOnlySolutions [req]

hasOnlySolutions :: [[(Atom Var, Atom Val)]] -> [Assignment Val] -> Bool
hasOnlySolutions reqs = (==)(map M.fromList reqs)

mkTest :: Show a => String -> (a -> Bool) -> a -> [String]-> TestInstance
mkTest name' check' val' tags'
  = trace ("---"++name'++"---") $ TestInstance
    { run = return $ if check' val'
                        then Finished Pass
                        else Finished $ Fail $ show val'
    , name = name'
    , tags = tags'
    , options = []
    , setOption = \opN op -> Right $ mkTest name' check' val' tags'
    }

addTags :: [String] -> ([String] -> TestInstance) -> TestInstance
addTags t i = i t

fails :: Either a b -> Bool
fails = isLeft

passes :: Either a b -> Bool
passes = isRight

exists :: Atom a -> Pred a
exists v = toPred [("exists", v)]
