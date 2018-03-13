incIfRight :: Num a
           => Either e a
           -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a
            => Either e a
            -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left  e) = Left e

incEither :: Num a
          => Either e a 
          -> Either e a
incEither = fmap (+1)

showEither :: Show a
           => Either e a
           -> Either e String
showEither = fmap show 

liftedInc :: (Functor f, Num b) =>
             f b
          -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show b) =>
              f b
           -> f String
liftedShow = fmap show
