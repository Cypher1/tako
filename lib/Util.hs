module Util where

import Data.Either (isRight, isLeft)


fails :: Either a b -> Bool
fails = isLeft

passes :: Either a b -> Bool
passes = isRight

indent :: String -> String
indent x = unlines $ map ("  "++) $ lines x

line :: Show a => a -> String
line a = indent $ show a

join :: (Show x, Foldable f) => String -> f x -> String
join j xs = drop (length j) $ concatMap (\x->j++show x) xs

try :: (a -> b -> Either d c) -> a -> Either d b -> Either d c
try f a (Right b) = f a b
try f a (Left d) = Left d

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

labelL :: Show a => String -> a -> String
labelL l a = l ++ ":\t" ++ show a ++ "\n"

printL :: Show a => String -> a -> IO ()
printL label val = putStr $ labelL label val
