module WithRP where

import Text.ParserCombinators.ReadP
import Control.Applicative

one = char '1'

two = char '2'

three = char '2'

oneTwo = one >> two

oneTwo' = oneTwo >> eof

wtf = string "1" <|> string "12" <|> string "123"

wtf' = one <|> (one >> two) <|> (one >> two >> three)
