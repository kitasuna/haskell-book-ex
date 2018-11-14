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
  
