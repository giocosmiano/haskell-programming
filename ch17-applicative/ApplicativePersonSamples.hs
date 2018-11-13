module ApplicativePersonSamples where

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address
            deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

-- e.g.
-- fmap (fmap Person (mkName "Bob")) (mkAddress "123 Main St")
-- will error out with "Couldn't match expected type ‘Address -> b’ with actual type ‘Maybe (Address -> Person)’
--
-- but this works
-- Person <$> mkName "Bob" <*> mkAddress "123 Main St"    -> Just (Person (Name "Babe") (Address "farm"))
-- OR
-- liftA2 Person (mkName "Bob") (mkAddress "123 Main St") -> Just (Person (Name "Babe") (Address "farm"))
