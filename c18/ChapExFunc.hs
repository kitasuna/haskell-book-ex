import Control.Monad (join) 

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = fmap f m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = (fmap f x) <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a mx mf = mf <*> mx

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh (x:[]) f = l2 (:) (f x) (return [])
meh (x:xs) f = l2 (:) (f x) (meh xs f)

-- used this as my (a -> m b) in `meh`
isEven :: Integer -> Maybe Integer
isEven x | even x = Just x
         | otherwise   = Nothing

flipType :: Monad m
         => [m a] -> m [a]
flipType xs = meh xs id
