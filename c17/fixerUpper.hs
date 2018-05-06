x = const <$> Just "Hello" <*> Just "World"

y = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
