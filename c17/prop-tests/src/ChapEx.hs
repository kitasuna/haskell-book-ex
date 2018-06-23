module ChapEx where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f1 f2) (Pair x1 x2) = Pair (f1 x1) (f2 x2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance Eq a => EqProp (Pair a) where (=-=) = eq
-- instance Eq a => EqProp (List a) where (=-=) = eq

main :: IO ()
main = do
  putStrLn "-- applicative Pair a'"
  quickBatch (applicative $ (undefined :: Pair (Int, Double, Char)))
