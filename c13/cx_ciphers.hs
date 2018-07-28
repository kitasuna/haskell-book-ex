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

-- Vigenere
encodeV :: String -> String -> String
encodeV xs ys = shiftChar <$> zip xs (genOffsetString xs ys)

decodeV :: String -> String -> String
decodeV xs ys = unshiftChar <$> zip xs (genOffsetString xs ys)

--Caesar
encodeC :: String -> Int-> String
encodeC [] _ = ""
encodeC (x:xs) offset = chr ((ord x) + offset) : encodeC xs offset

decodeC :: String -> Int-> String
decodeC [] _ = ""
decodeC (x:xs) offset = chr((ord x) - offset) : decodeC xs offset


caesar :: IO ()
caesar = do
  putStrLn "Caesar cipher"
  putStr "Input string to encode: "
  xs <- getLine
  putStr "Input offset: "
  offset <- readLn
  putStrLn $ encodeC xs offset

uncaesar :: IO ()
uncaesar = do
  putStrLn "Uncaesar!"
  putStr "Input string to decode: "
  xs <- getLine
  putStr "Input offset: "
  offset <- readLn
  putStrLn $ decodeC xs offset

vigenere :: IO ()
vigenere = do
  putStrLn "Vigenere ciphere"
  putStr "Input string to encode: "
  xs <- getLine
  putStr "Input salt: "
  ys <- getLine
  putStrLn $ encodeV xs ys

unvigenere :: IO ()
unvigenere = do
  putStrLn "Unvigenere!"
  putStr "Input string to decode: "
  xs <- getLine
  putStr "Input salt: "
  ys <- getLine
  putStrLn $ decodeV xs ys
