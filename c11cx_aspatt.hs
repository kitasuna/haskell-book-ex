module AsPatt where

-- Non-contiguous
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf sub@(sh:st) full@(fh:ft) =
  if sub == take (length sub) full
  then True
  else isSubseqOf sub ft

-- Contiguous
isSubseqOf' :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf' [] _ = True
isSubseqOf' _ [] = False
isSubseqOf' sub@(sh:st) full@(fh:ft) =
  if sh == fh
  then True && isSubseqOf' st ft
  else isSubseqOf' sub ft

capitalizeWords :: String -> [(String, String)]
capitalizeWords = undefined
