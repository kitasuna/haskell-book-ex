module Phone where

import Data.List (elemIndex)
import Data.Char (toLower)

type SpaceChar = Char
type CapsChar = Char
data Key = Key String
data DaPhone = DaPhone [Key]

phone :: DaPhone
phone = DaPhone [
  Key "",
  Key "abc2",
  Key "def3",
  Key "ghi4",
  Key "jkl5",
  Key "mno6",
  Key "pqrs7",
  Key "tuv8",
  Key "wxyz9",
  Key " 0",
  Key "*"
  ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have you ever tasted alcohol?"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

type Digit = Char

type Presses = Int

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
            -- prolly wanna change the above to match my def...
reverseTaps _ ' ' = [('0', 1)] 
reverseTaps (DaPhone keys) c =
  if c >= 'A' && c <= 'Z'
  then ('0', 1) : findChar keys (toLower c)
  else findChar keys c
    where
      findChar [] _ = []
      findChar (k@(Key str):ks) c =
        case charInKey k c of
          Just pos -> [((last str), (pos + 1))]
          Nothing -> findChar ks c

charInKey :: Key -> Char -> Maybe Int
charInKey (Key []) _ = Nothing
charInKey (Key ks) c = elemIndex c ks

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead phone xs = reverseTaps phone `concatMap` xs
