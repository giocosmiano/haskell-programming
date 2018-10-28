module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbString "Hello, world!"
              , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
              , DbNumber 9002
              , DbString "Again, Hello world!"
              ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\a xs -> getDbDate a : xs) [] . filter isDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\a xs -> getDbNumber a : xs) [] . filter isDbNumber

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb dbs =
  let xs = filterDbNumber dbs
  in  fromIntegral (sum xs) / fromIntegral (length xs)

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _          = False

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False

getDbDate :: DatabaseItem -> UTCTime
getDbDate (DbDate x) = x
getDbDate _          = undefined

getDbNumber :: DatabaseItem -> Integer
getDbNumber (DbNumber x) = x
getDbNumber _            = undefined
