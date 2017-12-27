import Data.List (intercalate)

notThe :: String -> Maybe String
notThe xs
  | xs /= "the" = Just xs
  | otherwise = Nothing

replaceThe :: String -> String
replaceThe xs =
  intercalate " " $ rep <$> words xs
      where rep w =
              case notThe w of
                Just str -> str
                Nothing -> "a"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel xs = 0 + go werd1 werd2 xs
  where go w1 w2 xs = if (w1 == "the" && ((head w2) `elem` ['a','e','i','o','u']))
                      then 1 + countTheBeforeVowel (drop 2 xs)
                      else countTheBeforeVowel (drop 2 xs)
        werds = words xs
        werd1 = head werds
        werd2 = head $ tail werds


isAVowel :: Char -> Bool
isAVowel = (flip elem) ['a','e','i','o','u']

onlyVowels :: String -> [Char]
onlyVowels = filter isAVowel

onlyConsonants :: String -> [Char]
onlyConsonants = filter (not . isAVowel)

countVowels :: String -> Int
countVowels = length . onlyVowels

countConsonants :: String -> Int
countConsonants = length . onlyConsonants

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str
  | countVowels str > countConsonants str = Nothing
  | otherwise = Just (Word' str)
