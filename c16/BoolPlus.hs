module BoolPlus where

import Test.QuickCheck
import Test.QuickCheck.Function

data BoolPlus a =
  False' a | True' a
  deriving (Eq, Show)

instance Functor BoolPlus where
  fmap f (False' a) = False' (f a)
  fmap f (True'  a) = True'  (f a)

instance Arbitrary a => Arbitrary (BoolPlus a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (False' a)),
               (1, return (True' a))]

type IntToInt = Fun Int Int
type BoolPlusId = (BoolPlus Int) -> Bool
type BoolPlusComp = BoolPlus Int -> IntToInt -> IntToInt -> Bool
