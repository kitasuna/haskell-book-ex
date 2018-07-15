module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = 
  case n of
        0 -> "zero"
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"


digits :: Int -> [Int]
digits n = reverse $ go n 10 
  where go num denom 
          | num < denom = [num]
          | num <= 0 = [0]
          | otherwise = (num `mod` denom) : go (num `div` denom) denom

wordNumber :: Int -> String
wordNumber n = concat $ f <$> map digitToWord $ digits n
  where f = intersperse "-"
