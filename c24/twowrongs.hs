-- Messing with the code in the article:
-- https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html
--

-- METAR example
-- BIRK 281500Z 09014KT CAVOK M03/M06 Q0980 R13/910195
import Text.ParserCombinators.ReadP
import Control.Applicative

data WindInfo = WindInfo
  { dir :: Int 
  , speed :: Int
  , gusts :: Maybe Int
  } deriving Show

data Report = Report
  { station :: String
  , time :: (Int, Int, Int)
  , wind :: WindInfo
  } deriving Show

metar :: ReadP Report
metar = do
  code <- airport
  time <- timestamp
  wind <- windInfo
  return (Report code time wind)

isVowel :: Char -> Bool
isVowel char = 
  any (char ==) "aeiou"

vowel :: ReadP Char
vowel =
  satisfy isVowel

atLeastOneVowel :: ReadP [Char]
atLeastOneVowel = many1 vowel

capAz :: ReadP Char
capAz = satisfy (\char -> char >= 'A' && char <= 'Z')

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

timestamp :: ReadP (Int, Int, Int)
timestamp = do
  day     <- numbers 2
  hour    <- numbers 2
  minute  <- numbers 2
  string "Z "
  if day < 1 || day > 31 || hour > 23 || minute > 59 then
    pfail
  else
    return (day, hour, minute)

numbers :: Int -> ReadP Int
numbers digits = 
  fmap read (count digits digit)

windInfo :: ReadP WindInfo
windInfo = do
  direction <- numbers 3
  speed <- numbers 2 <|> numbers 3
  gusts <- option Nothing (fmap Just gustParser)
  unit <- string "KT" <|> string "MPS"
  string " "
  return (WindInfo
    direction
    (toMPS unit speed)
    (fmap (toMPS unit) gusts))

gustParser :: ReadP Int
gustParser = do
  satisfy (== 'G')
  numbers 2 <|> numbers 3

toMPS :: String -> Int -> Int
toMPS unit speed =
  case unit of
    "KT" -> div speed 2
    "MPS" -> speed

airport :: ReadP String
airport = do
  code <- many1 capAz
  satisfy (== ' ')
  return code
