module PhoneNum where

import Control.Applicative
import Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

pnSep :: Parser Char
pnSep = char '-' <|> char ' '

parsePhone :: Parser PhoneNumber
parsePhone = do
  optional $ string "1-" <|> string "1 "
  area <- optional (char '(') *> (count 3 digit) <* optional (char ')')
  optional pnSep
  exchange <- count 3 digit
  optional pnSep
  line <- count 4 digit
  return $ PhoneNumber (read area) (read exchange) (read line)
