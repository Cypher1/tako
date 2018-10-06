module TestUtil where

-- Test types
prints :: Show a => a -> Bool
prints = (/= "").show

debug :: a -> Bool
debug = const False
