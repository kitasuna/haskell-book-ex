module ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' x Nil = Nil
take' 0 _ = Nil
take' x (Cons a l) = Cons a $ take' (x - 1) l

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a l) = Cons (f a) $ fmap f l

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) `append` (fs <*> xs)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                  in take' 3000 l
          ys' = let (ZipList' l) = ys
                  in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  --(<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) = ZipList' (Cons (f x) (ZipList' fs <*> ZipList' xs))
  (<*>) (ZipList' c1) (ZipList' c2) = ZipList' $ go c1 c2
    where
      go Nil _ = Nil
      go _ Nil = Nil
      go (Cons f fs) (Cons x xs) = Cons (f x) $ go fs xs
