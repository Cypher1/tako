module Util where

indent :: String -> String
indent x = unlines $ map ("  "++) $ lines x

line :: Show a => a -> String
line a = (indent $ show a)

join :: (Show x, Foldable f) => String -> f x -> String
join j xs = drop (length j) $ concatMap (\x->j++(show x)) xs

showEither :: (Show a, Show b) => Either a b -> String
showEither (Left a) = show a
showEither (Right a) = show a

printEither :: (Show a, Show b) => Either a b -> IO ()
printEither (Left a) = print a
printEither (Right a) = print a

showList :: Show a => [a] -> String
showList xs = drop (length joiner) $ concatMap (\x->joiner++show x) xs
  where
    joiner = ", "
