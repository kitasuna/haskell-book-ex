{-# LANGUAGE InstanceSigs #-}
module Exercises where

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \st -> (f (fst (g st)), st)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \st -> (a, st)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) = Moi $ \st -> ((fst (f st)) (fst (g st)), st)


instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \st -> let a = fst $ f st in runMoi (g a) st

  -- Needed this to get `modify` working
  -- Thanks to dmvianna
  -- https://github.com/dmvianna/haskellbook/blob/master/src/Ch23-MyState.hs
  Moi f >> Moi g = Moi $ \s -> let (_, s') = f s
                                     in g s'
get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put x = Moi $ \s -> ((), x)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
