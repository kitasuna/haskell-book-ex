module Language where

-- for toUpper
import Data.Char

-- for intercalate
import Data.List

capitalizeWord :: String -> String
capitalizeWord (x:xs) = [toUpper x] ++ (toLower <$> xs)

capitalizeParagraph :: String -> String
capitalizeParagraph [] = ""
capitalizeParagraph xs = intercalate " " $ f $ capitalizeFirst $ words xs 
  where
      capitalizeFirst [] = []
      capitalizeFirst (x:xs) = (capitalizeWord x) : xs
      f [] = []
      f (x:[]) = [x]
      f (one:two:rest) =
          if last one == '.'
          then one : (capitalizeWord two) : f rest
          else one : two : f rest
