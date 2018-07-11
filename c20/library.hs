import Data.Foldable
import Data.Monoid

data Identity a =
  Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x


data Optional a =
    Yep a
  | Nada
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z
  
  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x


-- Ended up looking at Data.Foldable to figure this out
-- Had to write the types out to see what was up
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . (foldMap Sum)

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . (foldMap Product)

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (Any . (== x)) xs

minimum' :: (Foldable t, Ord a)
         => t a -> Maybe a
minimum' = foldr f Nothing
             where
               f x Nothing = Just x
               f x mAcc = case fmap (> x) mAcc of
                            Just True -> Just x
                            Just False -> mAcc
 
maximum' :: (Foldable t, Ord a)
         => t a -> Maybe a
maximum' = foldr f Nothing
             where
               f x Nothing = Just x
               f x mAcc = case fmap (< x) mAcc of
                            Just True -> Just x
                            Just False -> mAcc

null' :: (Foldable t) => t a -> Bool
null' = foldr (\x acc -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\x acc -> acc + 1) 0 

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x acc -> [x] ++ acc) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (mappend mempty)

foldMap' :: (Foldable t, Monoid m)
         => (a -> m) -> t a -> m
foldMap' f a = foldr (\x acc -> acc `mappend` (f x)) mempty a
