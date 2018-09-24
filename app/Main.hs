module Main where
import Prelude hiding (showList)

import Data.Either (isLeft, isRight)
import Triple
import Expr
import qualified Data.Set as S
import Data.Set (Set)

labelL :: Show a => String -> a -> String
labelL l a = l ++ ":\t" ++ show a ++ "\n"
printL :: Show a => String -> a -> IO ()
printL label val = putStr $ labelL label val

type TestResult = Bool
data Test
  = Check String TestResult String
  | Log String String
  -- TODO(jopra): Add support for sets of tests

mkTest :: Show a => String -> (a -> TestResult) -> a -> Test
mkTest name res val = Check name (res val) $ show val

mkLog :: Show a => String -> a -> Test
mkLog name val = Log name $ show val

spacing = 48
printT :: Test -> IO (Int, Int) -- (#Passed, #Failed)
printT (Log name s) = do
  putStrLn $ name ++":"++(replicate spc ' ')++s
  return (0, 0)
  where
    spc = spacing - length (name++":")
printT (Check name res err') = do
  putStrLn $ name ++":"++(replicate spc ' ')++res'
  return $ if res then (1, 0) else (0, 1)
  where
    spc = spacing-length (name++":")
    res' = if res
             then "Passed"
             else "Failed\n"++err'

-- List of Tests
-- Constants
zero = val "0"
ret = var "ret"
a = var "a"
b = var "b"
ne = var "!="
b_ne_zero = [b, ne, zero]
fdiv = func [Div a b ret] (S.fromList [exists a, exists b]) (S.fromList [creates ret])
frac = add_pre b_ne_zero fdiv
minus = func [Sub a b ret] (S.fromList [exists a, exists b]) $ S.fromList [creates ret]
needs_ret = add_pre [ret] emp

-- Test types
prints = (/= "").show

fails = isRight
passes = isLeft
debug = const False

-- Tests
tests :: [Test]
tests
  = [ mkTest "constantsExist" (not.null) $ showList [zero, ret, a, b, ne]
    , mkTest "a-b" prints minus
    , mkTest "unsafe a/b" prints fdiv
    , mkTest "safe a/b" prints frac
    , mkTest "updateFrac with emp fails" fails $ update emp frac
    , mkTest "updateFrac with b!=0 fails" fails
      $ update (assume [b_ne_zero]) frac
    , mkTest "updateFrac with b!=0 and a fails" fails
      $ update (assume [b_ne_zero, [a]]) frac
    , mkTest "updateFrac with b!=0, b and a succeeds" fails
      $ update (assume [b_ne_zero, [a], [b]]) frac
    , mkTest "updateFrac with b!=0, !=, b and a succeeds" fails
      $ update (assume [b_ne_zero, [a], [b], [ne]]) frac
    , mkTest "updateFrac with b!=0, !=, 0, b and a succeeds" passes
      $ update (assume [b_ne_zero, [a], [b], [ne], [zero]]) frac
    , mkTest "require ret" prints needs_ret
    , mkTest "neets ret <*> frac is unsat" fails $ update needs_ret frac
    , mkTest "frac <*> neets ret is sat" passes $ update frac needs_ret
    , mkLog "frac <*> needs_ret" $ update frac needs_ret
    -- TODO(jopra): Execute a triple
    -- TODO(jopra): Compile a triple
    -- TODO(jopra): Resolve predicates for triples
    -- TODO(jopra): Check separation for triples
    -- TODO(jopra): Check parallelisation for triples
    ]

-- Run all tests
main :: IO ()
main = do
  res <- mapM printT tests
  putStrLn ""
  putStrLn $ "Passed: "++ ( show $ sum $ map fst res)
  putStrLn $ "Failed: "++ ( show $ sum $ map snd res)
  -- let (Left if_update_succeeded) = update_with_requires 
  -- printL "require ret + update" $ update needs_ret if_update_succeeded
  -- printL "update + require ret" $ update if_update_succeeded needs_ret
