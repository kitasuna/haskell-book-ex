module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1' eof

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)
