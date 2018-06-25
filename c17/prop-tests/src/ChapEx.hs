module ChapEx where

import Data.Monoid
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

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two m1 f) (Two m2 x) = Two (m1 <> m2) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three ma1 mb1 f) (Three ma2 mb2 x) =
    Three (ma1 <> ma2) (mb1 <> mb2) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq


data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' m1 f f') (Three' m2 x x') = 
    Three' (m1 <> m2) (f x) (f' x')

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      b' <- arbitrary
      return (Three' a b b')

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four ma1 mb1 mc1 f) (Four ma2 mb2 mc2 x) =
    Four (ma1 <> ma2) (mb1 <> mb2) (mc1 <> mc2) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z b) = Four' x y z $ f b

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' ma1 mb1 mc1 f) (Four' ma2 mb2 mc2 x) = 
    Four' (ma1 <> ma2) (mb1 <> mb2) (mc1 <> mc2) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four' a a a b)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

main :: IO ()
main = do
  putStrLn "-- applicative Pair a'"
  quickBatch (applicative $ (undefined :: Pair (Int, Double, Char)))
  putStrLn "-- applicative Two a b'"
  quickBatch (applicative $ (undefined :: Two (String, String, String) (String, String, String)))
  putStrLn "-- applicative Three a b c"
  quickBatch (applicative $ (undefined :: Three (String, [Int], String) (String, [Int], String) (String, [Int], String)))
  putStrLn "-- applicative Three' a b"
  quickBatch (applicative $ (undefined :: Three ([Int], [String], [Bool]) ([Int], [String], [Bool]) ([Int], [String], [Bool])))
  putStrLn "-- applicative Four a b c d"
  quickBatch (applicative $ (undefined :: Four (String, String, String) (String, [Bool], String) ([Bool], String, String) ([Int], [Bool], String)))
  putStrLn "-- applicative Four' a b"
  quickBatch (applicative $ (undefined :: Four' (String, String, String) ([Bool], String, [Int])))
