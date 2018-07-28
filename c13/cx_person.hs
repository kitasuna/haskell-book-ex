type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person

mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter a name: " 
  name <- getLine
  putStr "Enter an age: "
  age <- readLn
  case mkPerson name age of
    Left err -> putStrLn $ "got error: " ++ (show  err)
    Right p -> putStrLn $ "Yay, here's ur person: " ++ (show p)
