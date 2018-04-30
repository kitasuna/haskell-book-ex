module Company where

import Test.QuickCheck
import Test.QuickCheck.Function

data Company a b c =
    DeepBlue a b
  | Something c
  deriving (Eq, Show)


instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Company a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      frequency [(1, return (DeepBlue a b)),
                 (1, return (Something c))]

type IntToInt = Fun Int Int

type CompanyId = (Company Int Int Int) -> Bool

type CompanyComp = (Company Int Int Int)
                   -> IntToInt
                   -> IntToInt
                   -> Bool
