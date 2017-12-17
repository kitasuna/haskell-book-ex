module Cipher where

import Data.Char

-- use chr and ord functions
--
shiftChar :: Int -> Char -> Char
shiftChar offset c = chr $ (ord c) + offset

encode :: String -> Int -> String
encode xs offset = shiftChar offset <$> xs
