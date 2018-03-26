{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr
import Test.QuickCheck
import Test.QuickCheck.Function

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

data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True'  a) = True'  (f a)

data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

newtype Mu f = InF { outF :: f (Mu f) }
{-
-- Don't think we can write a valid functor
-- because `kind` is incorrect...
-- If we don't include `f` in the instance clause, 
-- kind is (* -> *) -> * (wrong)
-- and if we do include `f` in the instance clause,
-- kind is * (also wrong)

instance Functor (Mu f) where
  fmap f (InF m) = undefined
-}

data D = D (Array Word Word) Int Int
{-
-- Don't think we can write a valid functor
-- because `kind` is incorrect...

instance Functor D where
  fmap f (D _ a b) = undefined
-}

data Sum b a =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

instance (Arbitrary b, Arbitrary a) => Arbitrary (Sum b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    frequency [(1, return (First b)),
               (1, return (Second a))]

type SumId = (Sum Int Int) -> Bool

type IntToInt = Fun Int Int

type SumComp = (Sum Int Int)
               -> IntToInt
               -> IntToInt
               -> Bool
  

data Company a b c =
    DeepBlue a b
  | Something c
  deriving (Eq, Show)


instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c


data More a b =
    L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype J a b = J a

instance Functor (Flip J a) where
  fmap f (Flip (J a)) = Flip (J (f a))

data EvilGoateeConst a b = GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut $ fmap g x

data Parappa f g a = 
  DaWrappa (f a) (g a)

instance (Functor f1, Functor f2) => Functor (Parappa f1 f2) where
  fmap g (DaWrappa (f1) (f2)) = DaWrappa (fmap g f1) (fmap g f2)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething f1 f2) = IgnoringSomething f1 $ fmap f f2

data Notorius g o a t =
  Notorius (g o) (g a) (g t)

instance Functor g => Functor (Notorius g o a) where
  fmap f (Notorius f1 f2 f3) = Notorius f1 f2 $ fmap f f3

data List a =
    Nil
  | Cons a (List a)

instance Functor (List) where
  fmap f Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b) 

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor (GoatLord) where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) =
    MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a =
    Halt
  | Print String a
  | Read  (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x $ f y
  fmap f (Read g) = Read $ fmap f g

main = do
  quickCheck (functorIdentity :: SumId)
  quickCheck (functorCompose  :: SumComp)
