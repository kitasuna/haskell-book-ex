module Apl2 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  --Not quite: (<*>) (Cons f fl) (Cons x xl) = Cons (f x) $  fl <*> xl
  (<*>) (Cons f fl) ys = (f <$> ys) `append` (fl <*> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x l) = Cons (f x) $ (fmap f l)

{-
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Cons a (Cons b Nil), Nil]
-}
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

listIntStrChar = Cons (([1], "a", 'c') :: ([Int], String, Char)) Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' $ fmap f as 

main :: IO ()
main = do
  putStrLn "-- applicative List'"
  quickBatch (applicative $ (Cons (([1], "a", 'c') :: ([Int], String, Char)) Nil))
