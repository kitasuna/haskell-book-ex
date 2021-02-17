{-# LANGUAGE InstanceSigs #-}

module Main where

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

main :: IO ()
main = putStrLn "Hello, Haskell!"
