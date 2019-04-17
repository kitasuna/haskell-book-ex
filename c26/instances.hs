{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Chap26 where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m)
      => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
      MaybeT $ (fmap . fmap) f ma

instance (Applicative m)
      => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))

    (MaybeT fab) <*> (MaybeT mma) =
      MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m)
      => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a
          -> (a -> MaybeT m b)
          -> MaybeT m b

    (MaybeT ma) >>= f = -- f :: a -> MaybeT m b
      MaybeT $ do
        v <- ma -- ma :: m (Maybe x), v :: Maybe a
        case v of
          Nothing -> return Nothing
          Just y  -> runMaybeT (f y)
          -- the unwrapping is:
          -- (MaybeT ma), pattern match unwraps inner monad which still wraps Maybe a, so (ma :: m (Maybe a))
          -- v <- ma, which unwraps inner monad and gives us a Maybe a
          -- Just y, which unwraps the Maybe and gives us an a
          --
newtype EitherT e m a = 
  EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ (pure (pure x))
  (EitherT fab) <*> (EitherT ema) = EitherT $ (fmap (<*>) fab) <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  (EitherT ema) >>= f = -- f :: a -> EitherT e m b, ema :: m (Either e a)
    EitherT $ do
      eithA <- ema
      case eithA of
        Right a -> runEitherT (f a)
        Left e  -> return (Left e)

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

swapEither :: Either e a -> Either a e
swapEither x =
  case x of
    Left e  -> Right e
    Right a -> Left a

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT amb) =
  do
    eithAB <- amb
    case eithAB of
      Left a  -> f a
      Right b -> g b

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m)
      => Functor (StateT s m) where
  fmap :: forall a b. (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT g) =
    StateT $ \x ->
      let mas = g x -- :: m (a, s)
      in  fmap (\(a, s) -> ( f a, s )) mas

instance (Monad m)
      => Applicative (StateT s m) where
    pure x = StateT $ \s -> pure (x, s)
    (StateT fab) <*> (StateT g) =
      StateT $ \s ->
        let mas = g s -- :: m (a, s)
            ma2bs = fab s -- :: m (a -> b, s)
        in mas >>= (\(a, s) ->
          do
            (a2b, s) <- ma2bs
            return ((a2b a), s))

instance (Monad m)
      => Monad (StateT s m) where
    return = pure
    (StateT sma) >>= f = StateT $ \s ->
      let mas = sma s
      in mas >>= (\(a, s) -> (runStateT (f a)) s)

    -- need  :: s -> m (b, s)
    -- sma   :: s -> m (a, s)
    -- (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
