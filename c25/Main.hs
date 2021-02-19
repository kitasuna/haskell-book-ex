{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad (join)

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity ab) <*> (Identity a) = Identity $ ab a

instance Monad Identity where
  return = pure
  (Identity a) >>= amb = amb a

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor f => Functor (IdentityT f ) where
  fmap f (IdentityT fa) = IdentityT $ f <$> fa

instance Applicative f => Applicative (IdentityT f) where
  pure a = IdentityT $ pure a
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance Monad m => Monad (IdentityT m) where
  return = pure
  -- Originally had:
  -- (IdentityT ma) >>= amb = IdentityT $ join (runIdentityT <$> (amb <$> ma))
  -- which simplifies to the below based on functor laws:
  -- (IdentityT ma) >>= amb = IdentityT $ join (fmap (runIdentityT . amb) ma)
  -- which we can further simplify based on the definition of (>>=),
  -- i.e. that it is an fmap followed be a join:
  (IdentityT ma) >>= amb = IdentityT $ ma >>= (runIdentityT . amb)

newtype Compose f g a =
  Compose { getCompose :: f ( g a ) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g)
  => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ (pure . pure) x

    -- Much help provided by:
    -- https://fbrs.io/applicative_compose
    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose f) <*> (Compose a) =
      Compose $ ((<*>) <$> f <*> a)


main :: IO ()
main = putStrLn "Hello, Haskell!"
