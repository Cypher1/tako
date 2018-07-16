module Logic where

import qualified Prelude as P
import Prelude hiding (not, and, or)

class Not a where
  not_not :: a -> a
  not_true :: a
  not_false :: a

class And a where
  and_and :: [a] -> a

class Or a where
  or_or :: [a] -> a

class Logic a where
  and :: [a] -> a
  or :: [a] -> a
  not :: a -> a
  true :: a
  false :: a

instance (And a, Or a, Not a) => Logic a where
  and = and_and
  or = or_or
  not = not_not
  true = not_true
  false = not_false

instance Not Bool where
  not_not = P.not
  not_true = True
  not_false = False

instance And Bool where
  and_and = P.and

instance Or Bool where
  or_or = P.or
