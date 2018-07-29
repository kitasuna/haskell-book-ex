import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

mon :: [Char] -> ([Char], [Char])
mon = do
  a <- cap
  b <- rev
  return (a, b)

mon' :: [Char] -> ([Char], [Char])
mon' = cap >>= \x -> rev >>= \y -> return (x, y)
