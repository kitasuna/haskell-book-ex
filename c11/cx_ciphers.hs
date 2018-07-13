module Cipher where

import Data.Char

-- use chr and ord functions
--
shiftChar :: (Char, Char) -> Char
shiftChar (base, offset) = chr $ (ord base) + (getShift offset)

unshiftChar :: (Char, Char) -> Char
unshiftChar (base, offset) = chr $ (ord base) - (getShift offset)

getShift :: Char -> Int
getShift x = (ord 'A') - (ord x)

genOffsetString :: String -> String -> String
genOffsetString xs ys = take (length xs) (concat $ repeat ys)

encode :: String -> String -> String
encode xs ys = shiftChar <$> zip xs (genOffsetString xs ys)

decode :: String -> String -> String
decode xs ys = unshiftChar <$> zip xs (genOffsetString xs ys)

