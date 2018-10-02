module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty

fizzBuzzList' :: [Integer] -> [String]
fizzBuzzList' list = (fizzBuzz <$> list)


fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo start end = go start end []
  where
    go s e ns = if s > e then ns else go s (e - 1) ((fizzBuzz e) : ns)

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main :: IO ()
main =
  mapM_ putStrLn $ fizzBuzzList [1..100]

noState :: IO ()
noState =
  mapM_ putStrLn $ fizzBuzzList' [1..100]

fromTo :: IO ()
fromTo =
  mapM_ putStrLn $ fizzBuzzFromTo 1 100
