{-# LANGUAGE OverloadedStrings #-}

module WithTrifecta where

import Control.Applicative hiding (optional)
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/' 
  denominator <- decimal
  return ( numerator % denominator )
  
getDigits :: Parser Integer
getDigits = do
  digits <- integer
  e <- eof
  return (digits)

digitChar :: Parser Char
digitChar = satisfy (\c -> c >= '0' && c <= '9')

digitString :: Parser String
digitString = some digitChar

parseFloat :: Parser Float
parseFloat = do
  intPart <- decimal
  char '.'
  fracPart <- digitString
  return (read $ (show intPart) ++ "." ++ fracPart :: Float)

type FloatOrFrac = Either Float Rational

parseFF :: Parser FloatOrFrac
parseFF = 
      (Left <$> try parseFloat)
  <|> (Right <$> parseFraction)


