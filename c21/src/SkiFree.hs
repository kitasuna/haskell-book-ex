{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

instance (Functor n) => Functor (S n) where
  fmap f (S x a) = S (f <$> x) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = mappend (foldMap f na) (f a)

instance (Traversable n) => Traversable (S n) where
  traverse f (S na a) = S <$> (traverse f na) <*> (f a)

{-
instance Traversable n
      => Traversable (S n) where
  traverse = undefined
  -}

main :: IO ()
main = do
  quickBatch (functor (undefined:: S [] (Int, Int, Int)))
  quickBatch (traversable (undefined:: S [] ([Int], [Bool], [Bool])))
