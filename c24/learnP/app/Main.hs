module Main where

import WithRP
import Text.ParserCombinators.ReadP (readP_to_S)

pNL s =
  putStrLn ('\n' : s)

main :: IO ()
main = do
  --pNL "stop:"
  --testParse stop
  pNL "one:" 
  pNL $ show $ readP_to_S one "1"
