import Control.Monad
import Data.Char (isLetter, toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case ((f line1) == reverse (f line1)) of
    True -> putStrLn "It's a palindrome!"
    --False -> putStrLn "Nope!"
    False -> exitSuccess 
  where f = (filter isLetter) . (fmap toLower)
