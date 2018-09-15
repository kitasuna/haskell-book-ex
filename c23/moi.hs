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
