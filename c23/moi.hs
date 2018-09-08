{-# LANGUAGE InstanceSigs #-}

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- fmap f (Moi g) = Moi $ f . (fst (runMoi m))
  fmap f (Moi g) = Moi $ (\(a, s) -> ((f a),  s)) . g
