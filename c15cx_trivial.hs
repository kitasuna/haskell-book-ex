import Data.Semigroup
import Test.QuickCheck

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool
--
-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdAssoc =
  Identity String -> Identity String -> Identity String -> Bool

-- Two
--
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)
  
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- Three
--
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c) where
    (Three x y z) <> (Three a b c) = Three (x <> a) (y <> b) (z <> c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c) 

type ThreeAssoc = (Three String [Int] [Bool])
               -> (Three String [Int] [Bool])
               -> (Three String [Int] [Bool])
               -> Bool
-- Four
--
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    (Four a b c d) <> (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

type FourAssoc = (Four String [Int] [Bool] String)
              -> (Four String [Int] [Bool] String)
              -> (Four String [Int] [Bool] String)
              -> Bool

--BoolConj
--
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

instance Arbitrary BoolConj where
  arbitrary = elements [(BoolConj True), (BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- Laws
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
