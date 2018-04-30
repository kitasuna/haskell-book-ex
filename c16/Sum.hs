module Sum where

import Test.QuickCheck
import Test.QuickCheck.Function

data Sum b a =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

instance (Arbitrary b, Arbitrary a) => Arbitrary (Sum b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    frequency [(1, return (First b)),
               (1, return (Second a))]

type IntToInt = Fun Int Int

type SumId = (Sum Int Int) -> Bool

type SumComp = (Sum Int Int)
               -> IntToInt
               -> IntToInt
               -> Bool
  
