-- Applicative example
module MT where

x0 = fmap (+1) (Just 1)

x1 = (,,) <$> Just 1 <*> Just "lol" <*> Just [1, 2]


newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Functor m)
    => Functor (IdentityT m) where
  fmap f (IdentityT fa) =
    IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m)
    => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a
  -- f :: (a -> m b), so we get a monad-wrapped value back

instance (Monad m)
    => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
    -- f takes a, returns m b, runIdentityT gets us (f a)... argh wtf
    -- f :: (a -> m b) :: (a -> IdentityT m b)



