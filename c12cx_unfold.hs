myIterate :: (a -> a) -> a -> [a]
myIterate f b = b : (myIterate f (f b))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
   Just (x, y) -> x : (myUnfoldr f y)
   Nothing -> myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
--betterIterate f a = myUnfoldr (\b -> Just(b, ))
