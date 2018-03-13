{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a . f a -> g a

-- This works
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

{-
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]
-}
