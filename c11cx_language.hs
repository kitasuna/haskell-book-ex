module Language where

-- for toUpper
import Data.Char

-- for intercalate
import Data.List

capitalizeWord :: String -> String
capitalizeWord (x:xs) = [toUpper x] ++ (toLower <$> xs)

capitalizeParagraph :: String -> String
capitalizeParagraph xs = intercalate " " $ f <$> words xs 
  where f (one:two:rest) = 
