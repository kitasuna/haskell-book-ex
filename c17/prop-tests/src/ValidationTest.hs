module ValidationTest where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    MyFailure e
  | MySuccess a
  deriving (Eq, Show)

data Errors = 
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

-- same as either
instance Functor (Validation e) where
  fmap _ (MyFailure e) = MyFailure e
  fmap f (MySuccess a) = MySuccess $ f a

-- different from either
instance Monoid e =>
          Applicative (Validation e) where
  pure = MySuccess
  (<*>) (MyFailure e1) (MyFailure e2) = MyFailure $ mappend e1 e2
  (<*>) (MyFailure e1) _ = MyFailure e1
  (<*>) _ (MyFailure e1) = MyFailure e1
  (<*>) (MySuccess f) (MySuccess x) = MySuccess $ f x

-- need an arbitrary instance...
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [MySuccess a, MyFailure b]

instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

main :: IO ()
main = do
  putStrLn "-- applicative Validation'"
  quickBatch (applicative $ (undefined :: Validation String (Int, Double, Char)))
