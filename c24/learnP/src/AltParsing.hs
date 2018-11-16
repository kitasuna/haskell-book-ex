{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

a = "blah"
b = "123"
c = "123blah789"

atoz :: Parser String
atoz = some $ satisfy (\c -> (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
      (Left <$> integer)
  <|> (Right <$> atoz)

main = do
  let p f i =
        parseString f mempty i
  print $ p parseNos eitherOr
  {-
  print $ p atoz a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c 
  -}
