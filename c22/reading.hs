{-# LANGUAGE InstanceSigs #-}

newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 ::
  Applicative f =>
    (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g f1 f2 = g <$> f1 <*> f2

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  
  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r $ (ra r)

instance Monad (Reader r) where
  return :: a -> Reader r a
  return a = Reader $ const a

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb $ ra r) r
