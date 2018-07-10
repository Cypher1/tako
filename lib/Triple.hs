module Triple where

import Data.List(nub)
-- import Debug.Trace
import Prelude hiding (not, and, or)

import Util (line)
import Sat

import Expr (Sym, Expr)

data Env
  = Env [Expr] deriving Show

data Triple
  = Tri { pre :: Env
      , name :: Sym
      , op :: Expr
      , post :: Env
      } deriving Show

data Failure
  = Contradiction Expr Expr
  | Unspecified Expr
  | Many [Failure]
  deriving (Ord, Eq)

getAllErrors :: [Failure] -> Failure
getAllErrors xs = case (getErrors (Many xs)) of
                    [x] -> x
                    xs' -> Many xs'

getErrors :: Failure -> [Failure]
getErrors (Many xs) = nub $ concatMap getErrors xs
getErrors x = [x]


instance (Logic a) => Logic (Either a Failure) where
  true = Left true
  false = Left false
  and [] = true
  and ((Left x):xs) = case (and xs) of
                        Left y -> Left (and [x,y])
                        Right y -> Right y
  and ((Right x):xs) = case (and xs) of
                         Left _ -> Right x
                         Right y -> Right (getAllErrors [x,y])
  or [] = false
  or ((Left x):xs) = case (or xs) of
                        Left y -> Left (or [x,y])
                        Right y -> Right y
  or ((Right x):xs) = case (or xs) of
                        Left y -> Left y
                        Right y -> Right (getAllErrors [x,y])
  not (Left x) = Left $ not x
  not x = x

instance Show Failure where
  show (Contradiction a b) = "Contradiction:\n"++(line a)++"\nand\n"++(line b)++"."
  show (Unspecified a) = "Cannot prove:\n"++(line a)++"."
  show (Many ers) = "Multiple errors:\n"++concatMap line ers++"."

-- Assuming x, can we show that y is true.
-- sat_expr :: Expr -> Either Sat Failure
-- sat_expr x = trace (show x++" --> "++show cnf) $ Many []
  -- where cnf = convert_to_cnf x


unmet :: Env -> Env -> Env
unmet (Env _) (Env []) = Env [] -- No conditions, no failure
unmet (Env []) (Env _ys) = Env [] -- No env, no success
unmet (Env _xs) (Env _ys) = undefined

sat :: Env -> Env -> Maybe (Env, Failure)
sat e r = let left_over = unmet e r in case left_over of
            (Env []) -> Nothing
            (Env [x]) -> Just (left_over, Unspecified x)
            (Env xs) -> Just (left_over, Many (map Unspecified xs))

apply :: Env -> Triple -> Either Env Failure
apply _e _t = error "UNDEFINED!"
-- Check for Missing requirements (pre)
-- Check for Contradictions (???)
-- Update the env (removing old stuff?)
