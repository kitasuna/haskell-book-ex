module ReaderPractice where

import Prelude hiding (lookup, uncurry)
import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup k ((x,y):xs) = if k == x then Just y else lookup k xs

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ x `zip` y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ y `zip` z

-- one Nothing
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- zip x and z w/ a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ x `zip` z

-- (<*>) = f (a -> b) -> f a -> f b
x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 k = (,) (z' k) (z' k)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

summed :: Num c => (c, c) -> c
summed = uncurry (+)

s' = summed <$> ((,) <$> xs <*> ys)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

ex1 = foldr (&&) True $ sequA 6
ex1' = foldr (&&) True $ sequA 7

ex2 = sequA $ fromMaybe 0 s'

ex3 = bolt $ fromMaybe 0 ys

main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequA 7
  
