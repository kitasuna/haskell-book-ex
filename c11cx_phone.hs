module Phone where

type SpaceChar = Char
type CapsChar = Char
data Key = Key String
data DaPhone = DaPhone [Key] SpaceChar CapsChar

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
  Key "wxyz9"
  ] '0' '*'

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

type presses = Int

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
            -- prolly wanna change the above to match my def...
reverseTaps = undefined

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead = undefined

