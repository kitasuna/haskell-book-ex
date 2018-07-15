data Either' a b =
    Left' a
  | Right' b
  deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' x) = Right' $ f x

instance Applicative (Either' a) where
  pure = Right'
  (<*>) (Left' x) _ = Left' x
  (<*>) _ (Left' x) = Left' x
  (<*>) (Right' f) (Right' x) = Right' $ f x 

instance Foldable (Either' a) where
  foldMap _ (Left' x)  = mempty
  foldMap f (Right' y) = f y

  foldr _ acc (Left' x)  = acc 
  foldr f acc (Right' y) = f y acc

instance Traversable (Either' a) where
  traverse _ (Left' x)  = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y
