module Util where

import Prelude hiding (showList)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (intersperse)

indent :: String -> String
indent x = concat $ intersperse "\n" $ map ("  "++) $ lines x

line :: Show a => a -> String
line a = indent $ show a

join :: (Show x, Foldable f) => String -> f x -> String
join j xs = drop (length j) $ concatMap (\x->j++show x) xs

try :: (a -> b -> Either c d) -> a -> Either b d -> Either c d
try f a (Left b) = f a b
try _ _ (Right d) = Right d

showEither :: (Show a, Show b) => Either a b -> String
showEither (Right a) = show a
showEither (Left a) = show a

printEither :: (Show a, Show b) => Either a b -> IO ()
printEither (Right a) = print a
printEither (Left a) = print a

showList :: Show a => [a] -> String
showList xs = drop (length joiner) $ concatMap (\x->joiner++show x) xs
  where
    joiner = ", "

showSet :: Show a => Set a -> String
showSet = showList . S.toList

showMap :: (Show a, Show b) => Map a b -> String
showMap xs = drop (length joiner) $ concatMap (\(k, v)->joiner++show k++k_to_v++show v) $ M.toList xs
  where
    joiner = ", "
    k_to_v = ":"

onPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
onPair f g (a, c) = (f a, g c)

boundedAll :: (Enum a, Bounded a) => [a]
boundedAll = [minBound..maxBound]
