module Main where
import Prelude hiding (showList)
import System.Exit (exitFailure, exitSuccess)
import Data.Either (isLeft, isRight)
import Util (showList)
import Triple
import Expr
import Operation
import qualified Data.Set as S

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
fdiv = func [T Div a b ret] (S.fromList [exists a, exists b]) (S.fromList [creates ret])
frac = add_pre b_ne_zero fdiv
minus = func [T Sub a b ret] (S.fromList [exists a, exists b]) $ S.fromList [creates ret]
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
    ]++operationTests
    -- TODO(jopra): Execute a triple
    -- TODO(jopra): Compile a triple
    -- TODO(jopra): Resolve predicates for triples
    -- TODO(jopra): Check separation for triples
    -- TODO(jopra): Check parallelisation for triples

operationTests :: [Test]
operationTests
  = [ mkTest "Introducing a literal gives the same value" (==[(a, 3)])
      $ run (L 3 a) []
    , mkTest "Introducing a literal overwrites the old value" (==[(a, 3)])
      $ run (L 3 a) [(a, 100)]
    , mkTest "Free empties memory" (==[])
      $ run (U Free a) [(a, 3)]
    , mkTest "Free doesn't remove un-freed vars" (==[(b, 4)])
      $ run (U Free a) [(a, 3), (b, 4)]
    , mkTest "Complement 10 = -11" ((ret, -11)`elem`)
      $ run (B Not a ret) [(a, 10)]
    , mkTest "Complement -11 = 10" ((ret, 10)`elem`)
      $ run (B Not a ret) [(a, -11)]
    , mkTest "New a b copies a into b" ((b, 2)`elem`)
      $ run (B New a b) [(a, 2)]
    , mkTest "And 10&3 =" ((ret, 2)`elem`)
      $ run (T And a b ret) [(a, 10), (b, 6)]
    , mkTest "Or 10|3 =" ((ret, 14)`elem`)
      $ run (T Or a b ret) [(a, 10), (b, 6)]
    , mkTest "Add 10+3 =" ((ret, 13)`elem`)
      $ run (T Add a b ret) [(a, 10), (b, 3)]
    , mkTest "Sub 10-3 =" ((ret, 7)`elem`)
      $ run (T Sub a b ret) [(a, 10), (b, 3)]
    ]

-- Run all tests
main :: IO ()
main = do
  res <- mapM printT tests
  putStrLn ""
  let n_failed = sum $ map snd res
  let n_passed = sum $ map fst res
  putStrLn $ "Passed: "++ show n_passed
  putStrLn $ "Failed: "++ show n_failed
  if n_failed /= 0
     then exitFailure
     else exitSuccess

  -- let (Left if_update_succeeded) = update_with_requires 
  -- printL "require ret + update" $ update needs_ret if_update_succeeded
  -- printL "update + require ret" $ update if_update_succeeded needs_ret
