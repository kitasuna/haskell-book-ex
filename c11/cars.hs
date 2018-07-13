data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size String deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline = 
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Size
    deriving (Eq, Show)

myCar     = Car Mini (Price 14000)
urCar     = Car Mazda (Price 20000)
clownCar  = Car Tata (Price 7000)
doge      = Plane PapuAir (Size "747")

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> Bool
areCars =
  foldr f True
    where f a b =
                case a of
                  (Car _ _ ) -> True && b
                  _          -> False

areCars' :: [Vehicle] -> [Bool]
areCars' = fmap f
                  where f x =
                            case x of
                              (Car _ _ ) -> True
                              _          -> False

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

