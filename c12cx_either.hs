lefts' :: [Either a b] -> [a]
lefts' = foldr f [] 
  where
    f (Right _) acc = acc
    f (Left x) acc = x : acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right x) acc = x : acc
    f (Left _ ) acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' = undefined

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
