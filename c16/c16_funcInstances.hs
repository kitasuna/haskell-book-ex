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

type IdentityId = (Identity Int) -> Bool


type IdentityFC = (Identity Int)
               -> IntToInt
               -> IntToInt
               -> Bool

-- Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)


instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

type PairId = (Pair Int) -> Bool
type PairFC = (Pair Int) -> IntToInt -> IntToInt -> Bool


-- Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoId = (Two Int Int) -> Bool
type TwoFC = (Two Int Int) -> IntToInt -> IntToInt -> Bool

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

type ThreeId = (Three Int Int Int) -> Bool
type ThreeFC = (Three Int Int Int) -> IntToInt -> IntToInt -> Bool

-- Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

type ThreePId = (Three' Int String) -> Bool
type ThreePFC = (Three' Int Int) -> IntToInt -> IntToInt -> Bool

-- Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

type FourId = (Four Int Int Int Int) -> Bool
type FourFC = (Four Int Int Int Int) -> IntToInt -> IntToInt -> Bool

-- Four'
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
 fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b  <- arbitrary
    return (Four' a1 a2 a3 b)

type FourPId = (Four' Int Int) -> Bool
type FourPFC = (Four' Int Int) -> IntToInt -> IntToInt -> Bool

main = do
  quickCheck (functorIdentity :: IdentityId)
  quickCheck (functorCompose  :: IdentityFC)
  quickCheck (functorIdentity :: PairId)
  quickCheck (functorCompose  :: PairFC)
  quickCheck (functorIdentity :: TwoId)
  quickCheck (functorCompose  :: TwoFC)
  quickCheck (functorIdentity :: ThreeId)
  quickCheck (functorCompose  :: ThreePFC)
  quickCheck (functorIdentity :: ThreePId)
  quickCheck (functorCompose  :: ThreePFC)
  quickCheck (functorIdentity :: FourId)
  quickCheck (functorCompose  :: FourFC)
  quickCheck (functorIdentity :: FourPId)
  quickCheck (functorCompose  :: FourPFC)

