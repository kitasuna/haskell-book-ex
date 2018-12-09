{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFile where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Time
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

data LogDay = LogDay Day [LogLine]
  deriving (Eq, Show)

data LogLine =
  LogLine StartTime (Maybe EndTime) Activity Comment
  deriving (Eq, Show)

type Activity = String
type Comment = String
type StartTime = TimeOfDay
type EndTime = TimeOfDay

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseComment :: Parser Comment
parseComment = string "-- " >> parseLine

parseLine :: Parser [Char]
parseLine = some (noneOf "\n")

parseActWComment :: Parser (Activity, Comment)
parseActWComment = do
  act <- some (noneOf "\n-")
  _ <- string "-- "
  comment <- parseLine
  return (act, comment)

parseActWOComment :: Parser (Activity, Comment)
parseActWOComment = do
  act <- parseLine
  return (act, "")

parseDateLine :: Parser Day
parseDateLine = do
  string "# "
  year <- count 4 digit
  char '-' 
  month <- count 2 digit
  char '-' 
  day <- count 2 digit
  skipEOL
  return $ fromGregorian (read year) (read month) (read day)

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = do
  hours <- count 2 digit
  char ':'
  mins <- count 2 digit
  return $ TimeOfDay (read hours) (read mins) 0

parseLogLine :: Parser LogLine
parseLogLine = do
  tod <- parseTimeOfDay
  skipWhitespace
  (act, comment) <- try parseActWComment <|> parseActWOComment
  skipEOL
  return $ LogLine tod Nothing act comment

parseLogDay :: Parser LogDay
parseLogDay = do
  d <- parseDateLine
  ll <- many parseLogLine
  -- maybe parseLogLine needs to use `lookAhead` to check for the next start time?
  return $ LogDay d ll

loggy :: ByteString
loggy = [r|# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch -- cocoichi
|]

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  
  describe "Logfile parsing" $
    it "can parse a single day" $ do
      let m = parseByteString
              parseLogDay
              mempty loggy
          r' = maybeSuccess m
      print m
      r' `shouldBe`
        Just (
          LogDay (
            fromGregorian 2025 2 5
          )
          [
            LogLine (TimeOfDay 8 0 0) Nothing  "Breakfast" ""
          , LogLine (TimeOfDay 9 0 0) Nothing  "Sanitizing moisture collector" "" 
          , LogLine (TimeOfDay 11 0 0) Nothing "Exercising in high-grav gym" ""
          , LogLine (TimeOfDay 12 0 0) Nothing "Lunch " "cocoichi"
          ]
        )
