{-# LANGUAGE InstanceSigs #-}

newtype Identity a =
  Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

{-
-- alternatively
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  fmap f c =
    Compose $ (fmap . fmap) f $ getCompose c
-}

instance (Applicative f, Applicative g)
    => Applicative (Compose f g) where
  pure :: a -> Compose f g a 
  pure x = Compose (pure (pure x))

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ ((<*>) <$> f <*> a)

-- a :: Compose [] Sum a
-- m :: Sum?
-- t a :: ? Array of a?
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
instance (Foldable f, Foldable g) =>
    Foldable (Compose f g) where
  foldMap f (Compose fga) = ( foldMap . foldMap ) f fga

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
instance (Traversable f, Traversable g) =>
          Traversable (Compose f g) where
  -- traverse f (Compose fga) = sequenceA $ Compose $ (fmap . fmap) f fga
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga


class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a-> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor (Deux) where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a deriving (Eq, Show)

instance Bifunctor (Const) where
  bimap f g (Const a) = Const (f a)

data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data MyEither a b =
      MyLeft a
    | MyRight b
    deriving (Eq, Show)

instance Bifunctor (MyEither) where
  bimap _ g (MyRight a) = MyRight (g a)
  bimap f _ (MyLeft a) = MyLeft (f a)
