module ChapEx where

import Prelude hiding (Left, Right)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- =============== Nope
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) f x = NopeDotJpg

instance Monad Nope where
  return = pure
  m >>= f = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = do
    return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- =============== PhhhbbtttEither
data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right x)  = Right x
  fmap f (Left x) = Left $ f x

instance Applicative (PhhhbbtttEither b) where
  pure x = Left x
  (<*>) (Right x) _  = Right x
  (<*>) _ (Right x)  = Right x
  (<*>) (Left f) (Left x) = Left $ f x

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Right x) >>= _ = Right x 
  (Left x)  >>= f = f x

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [(1, return (Left b)),
                 (1, return (Right a))]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- =============== Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- =============== List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend l Nil = l
  mappend Nil l' = l'
  mappend (Cons x Nil) l' = Cons x l'
  mappend (Cons x xs) l' = Cons x $ mappend xs l'

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x l) = Cons (f x) $ fmap f l

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = mappend (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= f = Nil
  (Cons x xs) >>= f = (f x) `mappend` (xs >>= f)


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [(3, return $ Cons h t),
             (1, return Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor $ (undefined :: Nope (String, [Bool], [Int]))
  quickBatch $ applicative $ (undefined :: Nope (String, [Bool], [Int]))
  quickBatch $ monad $ (undefined :: Nope (String, [Bool], [Int]))
  putStrLn "^^ =============== Nope"
  quickBatch $ functor $ (undefined :: PhhhbbtttEither (String, [Bool], [Int]) (String, [Bool], [Int]))
  quickBatch $ applicative $ (undefined :: PhhhbbtttEither (String, [Bool], [Int]) (String, [Bool], [Int]))
  quickBatch $ monad $ (undefined :: PhhhbbtttEither (String, [Bool], [Int]) (String, [Bool], [Int]))
  putStrLn "^^ =============== PhhhbbtttEither"
  quickBatch $ functor $ (undefined :: Identity (String, [Bool], [Int]))
  quickBatch $ applicative $ (undefined :: Identity (String, [Bool], [Int]))
  quickBatch $ monad $ (undefined :: Identity (String, [Bool], [Int]))
  putStrLn "^^ =============== Identity"
  quickBatch $ functor $ (undefined :: List (String, [Bool], [Int]))
  quickBatch $ applicative $ (undefined :: List (String, [Bool], [Int]))
  quickBatch $ monad $ (undefined :: List (String, [Bool], [Int]))
  putStrLn "^^ =============== List"
