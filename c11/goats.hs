{-# LANGUAGE FlexibleInstances #-}

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

type Dogs = Int

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
  tooMany (x, s) = x > 45

instance TooMany (Int, Int) where
  tooMany (x, y) = (x + y) > 100

--instance TooMany (Num a, TooMany a) => (a, a) where
--  tooMany (x, y) = (x + y)
--
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)

data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

{-
data Person =
  MkPerson String Int
  deriving (Eq, Show)
namae :: Person -> String
namae (MkPerson s _) = s
-}

{-
data Person =
  Person { name :: String
         , age :: Int }
        deriving (Eq, Show)


data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show
-}

type AuthorName = String

--data Author = Author (AuthorName, BookType)

data Author =
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)
