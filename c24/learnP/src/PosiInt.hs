module PosiInt where

import Text.Trifecta

parseDigit :: Parser Char 
parseDigit = satisfy (\c -> c >= '0' && c <= '9')

base10Integer :: Parser Integer
base10Integer =
  do
    neg <- option ' ' (char '-')
    x <- some parseDigit
    return $ read ([neg] ++ x)

