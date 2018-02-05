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

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

instance Arbitrary BoolConj where
  arbitrary = elements [(BoolConj True), (BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 || b2)

instance Arbitrary BoolDisj where
  arbitrary = elements [(BoolDisj True), (BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Snd
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _      = Snd x
  Fst x <> Snd y  = Snd y
  Fst x <> Fst y  = Fst y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Fst a)),
               (1, return (Snd b))]

type OrAssoc = (Or Int Char) -> (Or Int Char) -> (Or Int Char) -> Bool
   
-- Combine
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  --(Combine f) <> (Combine g) = Combine (\x -> f x <> g x)
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

-- shamelessly cribbed from dmvianna@github
-- because the error in ghci was driving me batshit
instance Show (Combine a b) where
  show (Combine _ ) = "I got yer Show instance right here"


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
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
