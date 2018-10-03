{-# LANGUAGE InstanceSigs #-}

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- fmap f (Moi g) = Moi $ f . (fst (runMoi m))
  fmap f (Moi g) = Moi $ (\(a, s) -> ((f a),  s)) . g

{-
 Assists from:
 http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
 https://github.com/gvolpe/haskell-book-exercises/blob/master/chapter23/state.hs
 -}
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\x -> (a, x))

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \x -> (fst (f x) $ fst (g x), x)


instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $ \x -> runMoi (g (fst (f x))) x

  -- Needed this to get `modify` working
  -- Thanks to dmvianna
  -- https://github.com/dmvianna/haskellbook/blob/master/src/Ch23-MyState.hs
  Moi f >> Moi g = Moi $ \s -> let (_, s') = f s
                                     in g s'

get :: Moi s s
get = Moi $ (\x -> (x, x))

put :: s -> Moi s ()
put s = Moi $ (\x -> ((), s))

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) = (\x -> fst $ sa x)

modify :: (s -> s) -> Moi s ()
modify f = Moi $ (\x -> ((), f x))
