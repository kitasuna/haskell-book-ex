module Main where

import BadMonoid
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = quickBatch (monoid Twoo)
