-- v this is the type sig in the book,
-- but might be a typo? anyway...
data Constant a b =
  Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z

  foldMap f (Constant x) = f x

data Two a b = 
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f acc (Two a b) = f b acc
  foldMap f (Two a b) = f b 

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f acc (Three a b c) = f c acc
  foldMap f (Three a b c) = f c

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b `mappend` f b'
-- looked up how to define foldr in terms of this
-- seems to involve something called Endo and appEndo

data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b `mappend` f b' `mappend` f b''

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
-- ^ shout out to dmvianna, gvolpe again. this one killed me.
