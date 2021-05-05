module ApplicativeCowSample where

import Control.Applicative

data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
    case noEmpty name' of
      Nothing -> Nothing
      Just nammy ->
        case noNegative age' of
          Nothing -> Nothing
          Just agey ->
            case noNegative weight' of
              Nothing -> Nothing
              Just weighty -> Just (Cow nammy agey weighty)

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
    Cow <$> noEmpty name'
        <*> noNegative age'
        <*> noNegative weight'

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
    liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')
