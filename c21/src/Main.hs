module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- =============== Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-- =============== Constant
newtype Constant a b =
  Constant { getConstant :: a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant x) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a


-- =============== Maybe
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (Nada)),
               (3, return (Yep a))]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Functor (Optional) where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable (Optional) where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable (Optional) where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

-- =============== List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

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

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x l) = Cons (f x) $ (fmap f l)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

-- =============== Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- =============== Pair
data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

-- =============== Big
data Big a b =
  Big a b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a  <- arbitrary
    b  <- arbitrary
    b' <- arbitrary
    return $ Big a b b'

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = (f b) `mappend` (f b')

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

-- =============== Bigger
data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a   <- arbitrary
    b   <- arbitrary
    b'  <- arbitrary
    b'' <- arbitrary
    return $ Bigger a b b' b''

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = (f b) `mappend` (f b') `mappend` (f b'')

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

-- =============== Tree
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree

genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  x <- arbitrary
  t1 <- genTree
  t2 <- genTree
  frequency [(3, return $ Leaf x),
             (3, return $ Node t1 x t2),
             (1, return Empty)]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap _ Empty          = Empty
  fmap f (Leaf x)       = Leaf $ f x
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
  foldMap _ Empty          = mempty
  foldMap f (Leaf x)       = f x
  foldMap f (Node t1 a t2) = 
    (foldMap f t1)
    `mappend`
    (f a)
    `mappend`
    (foldMap f t2)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node t1 a t2) = 
    Node <$>
    (traverse f t1) <*>
    (f a) <*>
    (traverse f t2)




-- type TI = Three [Bool] [Int]
-- type TI = Pair [Int]
type TI = Tree

main :: IO ()
main = do
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
