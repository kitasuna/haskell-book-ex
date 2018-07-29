newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 ::
  Applicative f =>
    (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g f1 f2 = g <$> f1 <*> f2

asks :: (r -> a) -> Reader r a
asks f = Reader f
