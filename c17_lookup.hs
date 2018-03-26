import Control.Applicative
import Data.List (elemIndex)

f x =
  lookup x [ (3, "hello")
           , (4, "julie")
           , (5, "kbai") ]

g y =
  lookup y [ (7, "sup?")
           , (8, "chris")
           , (9, "aloha") ]

h z =
  lookup z [(2, 3), (5, 6), (7, 8)]

m x =
  lookup x [(4, 10), (8, 13), (1, 9001)]

xs = [1,2,3]
ys = [4,5,6]

added :: Maybe Integer
added =
 pure (+3) <*> (lookup 3 $ zip xs ys)

y :: Maybe Integer
y = lookup 3 $ zip xs ys

z :: Maybe Integer
z = lookup 2 $ zip xs ys

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x' :: Maybe Int
x' = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'


x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

-- alternatively...
summed' :: Maybe Integer
summed' = sum <$> (pure (,) <*> x'' <*> y'')
