import Test.QuickCheck
import Test.QuickCheck.Function

-- Lawses
functorIdentity :: (Functor f, Eq (f a)) =>
                      f a
                   -> Bool

functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                    f a
                 -> Fun a b
                 -> Fun b c
                 -> Bool

functorCompose x (Fun _ f) (Fun _ g) = 
  (fmap (g . f) x) == (fmap g (fmap f x))

-- Need this...
type IntToInt = Fun Int Int

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityFunctorId = (Identity Int) -> Bool


type IdentityFC = (Identity Int)
               -> IntToInt
               -> IntToInt
               -> Bool

-- Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

type PairFunctorId = (Pair Int) -> Bool

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

-- Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoFunctorId = (Two Int Int) -> Bool
type TwoFC = (Two Int Int) -> IntToInt -> IntToInt -> Bool

main = do
  quickCheck (functorIdentity :: IdentityFunctorId)
  quickCheck (functorCompose  :: IdentityFC)
  quickCheck (functorIdentity :: PairFunctorId)
  quickCheck (functorIdentity :: TwoFunctorId)
  quickCheck (functorCompose  :: TwoFC)

