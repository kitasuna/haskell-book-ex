module WithRP where

import Text.ParserCombinators.ReadP
import Control.Applicative

one = char '1'

two = char '2'

three = char '2'

oneTwo = one >> two

oneTwo' = oneTwo >> eof

hifumi = string "1" <|> string "12" <|> string "123"

hifumi' = one <|> (one >> two) <|> (one >> two >> three)
