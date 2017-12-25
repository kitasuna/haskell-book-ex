isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just x) = True

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc f Nothing = acc
mayybee acc f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe fb Nothing = fb
fromMaybe fb (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) =
  case x of
    Nothing -> catMaybes xs
    Just x -> x : (catMaybes xs)

-- Shout out to gvolpe@github for their solution on this one
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs =
  if hasNothings == False 
  then Just $ catMaybes xs
  else Nothing
    where hasNothings = or $ isNothing <$> xs
