module BoolPlusM where

import Test.QuickCheck
import Test.QuickCheck.Function

data BoolPlusM a =
  Falsish | Truish a
  deriving (Eq, Show)

instance Functor BoolPlusM where
  fmap f Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

instance Arbitrary a => Arbitrary (BoolPlusM a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ Truish a),
               (1, return Falsish)] 

type IntToInt = Fun Int Int

type BoolPlusMId = BoolPlusM Int -> Bool

type BoolPlusMComp = (BoolPlusM Int) -> IntToInt -> IntToInt -> Bool
