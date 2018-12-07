module SemVer where

import Control.Applicative
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare (SemVer majorX minorX patchX _ _) (SemVer majorY minorY patchY _ _) = 
    case compare majorX majorY of
      EQ -> case compare minorX minorY of
              EQ -> compare patchX patchY
              LT -> LT
              GT -> GT
      LT -> LT
      GT -> GT
parseNoS :: Parser NumberOrString
parseNoS = 
  try (NOSI <$> decimal)
  <|>
  NOSS <$> some letter

{-
parseNoS' :: Parser NumberOrString
parseNoS' = (decimal <|> some letter) `sepBy` (char '.')
-}

digit :: Parser Char
digit = satisfy (\c -> c >= '0' && c <= '9')

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  patch <- decimal
  option ' ' (char '-')
  rel <- option [] (parseNoS `sepBy` (char '.'))
  option ' ' (char '+')
  meta <- option [] (parseNoS `sepBy` (char '.'))
  return $ SemVer major minor patch rel meta
