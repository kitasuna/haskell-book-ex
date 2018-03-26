import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialAssoc =
  Trivial -> Trivial -> Trivial -> Bool

type TrivialMli =
  Trivial -> Bool

type TrivialMri =
  Trivial -> Bool
--
-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdAssoc =
  Identity String -> Identity String -> Identity String -> Bool

type IdMli = Identity String -> Bool
type IdMri = Identity String -> Bool


-- Two
--
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Semigroup a, Monoid b, Semigroup b)
  => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)
  
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

type TwoMli = Two String String -> Bool

type TwoMri = Two String String -> Bool

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

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj b1) (BoolConj b2) = BoolConj (b1 && b2)

instance Arbitrary BoolConj where
  arbitrary = elements [(BoolConj True), (BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjMli = BoolConj -> Bool
type BoolConjMri = BoolConj -> Bool

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 || b2)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj b1) (BoolDisj b2) = BoolDisj (b1 || b2)

instance Arbitrary BoolDisj where
  arbitrary = elements [(BoolDisj True), (BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjMli = BoolDisj -> Bool
type BoolDisjMri = BoolDisj -> Bool

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
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Combine a b) where
  mempty = mempty
  mappend (Combine f) (Combine g) = Combine (f <> g)

-- shamelessly cribbed from dmvianna@github
-- because the error in ghci was driving me batshit
instance Show (Combine a b) where
  show (Combine _ ) = "I got yer Show instance right here"


-- Comp
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid a => Monoid (Comp a) where
  mempty = mempty
  mappend (Comp f) (Comp g) = Comp (f . g)

-- Validation
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
    (Failure x) <> (Failure y) = Failure (x <> y)
    _ <> (Success y) = Success y
    (Success x) <> _ = Success x

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Validation a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [(1, return (Failure a)),
                (1, return (Success b))]

type ValidationAssoc = (Validation String Int)
                    -> (Validation String Int)
                    -> (Validation String Int)
                    -> Bool

-- AccumRight
newtype AccumRight a b =
  AccumRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumRight a b) where
  AccumRight (Success b1) <> AccumRight (Success b2) =
    AccumRight $ Success (b1 <> b2)
  AccumRight (Success b1) <> _ = AccumRight (Success b1)
  _  <> AccumRight (Success b2) = AccumRight (Success b2)
  AccumRight (Failure a1) <> _ = AccumRight (Failure a1)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (AccumRight (Failure a))),
               (1, return (AccumRight (Success b)))]

type AccumRightAssoc =
     (AccumRight String String)
  -> (AccumRight String String)
  -> (AccumRight String String)
  -> Bool

-- AccumBoth
newtype AccumBoth a b =
  AccumBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumBoth a b) where
    AccumBoth (Failure a1) <> AccumBoth (Failure a2)
      = AccumBoth (Failure (a1 <> a2))
    AccumBoth (Success b1) <> AccumBoth (Success b2)
      = AccumBoth (Success (b1 <> b2))
    AccumBoth (Success b1) <> _  = AccumBoth (Success b1)
    _ <> AccumBoth (Success b2) = AccumBoth (Success b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ AccumBoth $ Failure a),
               (1, return $ AccumBoth $ Success b)]

type AccumBothAssoc =
      AccumBoth String String
  ->  AccumBoth String String
  ->  AccumBoth String String 
  ->  Bool

-- Laws
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a `mappend` mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty `mappend` a) == a


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: TrivialMli)
  quickCheck (monoidRightIdentity :: TrivialMri)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (monoidLeftIdentity :: IdMli)
  quickCheck (monoidRightIdentity :: IdMri)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: TwoMli)
  quickCheck (monoidRightIdentity :: TwoMri)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConjMli)
  quickCheck (monoidRightIdentity :: BoolConjMri)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisjMli)
  quickCheck (monoidRightIdentity :: BoolDisjMri)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumRightAssoc)
  quickCheck (semigroupAssoc :: AccumBothAssoc)
