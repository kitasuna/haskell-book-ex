module Main where

import WithRP
import WithTrifecta
-- import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Trifecta

pNL s =
  putStrLn ('\n' : s)

main :: IO ()
main = do
  --pNL "stop:"
  --testParse stop
  {-
    pNL "one:" 
    pNL $ show $ readP_to_S one "1"
    pNL "hifumi 1:" 
    pNL $ show $ readP_to_S hifumi "1"
    pNL "hifumi 12:" 
    pNL $ show $ readP_to_S hifumi "12"
    pNL "hifumi 123:" 
    pNL $ show $ readP_to_S hifumi "123"
    pNL "hifumi' 1:" 
    pNL $ show $ readP_to_S hifumi' "1"
    pNL "hifumi' 12:" 
    pNL $ show $ readP_to_S hifumi' "12"
    pNL "hifumi' 123:" 
    pNL $ show $ readP_to_S hifumi' "123"
  -}
  let parseFraction' =
        parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction
