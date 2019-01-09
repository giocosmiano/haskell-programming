{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParsingLoggerExercises where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.ByteString (ByteString)
import Data.Maybe
import Data.List (sortBy, intercalate)

import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn)

import Data.Time
import Text.Trifecta
import Text.RawString.QQ

-----------------------------------------------------------------------------------
-- | parser daily logger file
-----------------------------------------------------------------------------------

type Activity = String
type Comments = String
type TimeSpentInMinutes = Integer
type TimeActivities  = Map LocalTime TimeActivity
type DailyActivities = Map Day TimeActivities

data TimeActivity = TimeActivity LocalTime Activity TimeSpentInMinutes
                  deriving (Eq, Show, Ord)

data LogInfo = LogInfo Day TimeActivities
             deriving (Eq, Show, Ord)

newtype DailyLogs = DailyLogs { getDailyLogs :: [TimeActivity] }
                  deriving (Eq, Ord)

instance Show DailyLogs where
  show (DailyLogs xs) = intercalate "\n" $ map show xs

-----------------------------------------------------------------------------------
-- | listing the daily logs
-----------------------------------------------------------------------------------

listOfDailyLogs :: Maybe DailyLogs
listOfDailyLogs = do
  mDailyActivities    <- maybeSuccess $ parseLogger
  let dailyActivities  = M.elems <$> mDailyActivities -- using <$> because the possibility of Nothing in Maybe
      timeActivities   = M.elems dailyActivities -- getting the lists of timeActivities for each dailyActivities
      tmpActivities'   = join timeActivities -- flattening the list of list from timeActivities for each dailyActivities
      actWithTimeSpent = foldr timeSpentOnActivity [] $ sortByLocalTime tmpActivities'
  return $ DailyLogs $ sortByLocalTime actWithTimeSpent

sortByLocalTime :: [TimeActivity] -> [TimeActivity]
sortByLocalTime = sortBy $ \(TimeActivity t _ _) (TimeActivity t' _ _) -> compare t t'

-----------------------------------------------------------------------------------
-- | see these references for further reading
-- https://wiki.haskell.org/Time
-- https://two-wrongs.com/haskell-time-library-tutorial
-----------------------------------------------------------------------------------
timeSpentOnActivity :: TimeActivity -> [TimeActivity] -> [TimeActivity]
timeSpentOnActivity (TimeActivity t a _) timeActivities =
  let timeDiff = if null timeActivities
      then 0
      else
        let (TimeActivity t' a' _) = last timeActivities
-- why am I getting this error??? -> Variable not in scope: diffLocalTime :: LocalTime -> LocalTime -> t
--            ltDiff    = diffLocalTime  t' t
            utcTime   = localTimeToUTC utc t
            utcTime'  = localTimeToUTC utc t'
            timeDiff  = diffUTCTime utcTime' utcTime
        in  (floor timeDiff :: Integer) `div` 60 -- in minutes
  in  timeActivities ++ [TimeActivity t a timeDiff]

-----------------------------------------------------------------------------------

-- | e.g.
-- parseLogger
parseLogger :: Result DailyActivities
parseLogger = parseByteString parseLogData mempty dailyLogs

parseLogData :: Parser DailyActivities
parseLogData = do
  a <- parseDailyActivities
  return $ M.fromList a

parseDailyActivities :: Parser [(Day, TimeActivities)]
parseDailyActivities = some $ do
  skipComments
  d <- parseDate
  a <- some $ parseTimeActivity d
  return $ (d, M.fromList a)

parseDate :: Parser Day
parseDate = do
  char '#' >> someSpace
  y <- read <$> count 4 digit
  char '-'
  m <- read <$> count 2 digit
  char '-'
  d <- read <$> count 2 digit
  skipOptional parseComments >> skipMany (noneOf "\n") >> skipEOL
  return $ fromGregorian y m d

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = do
  h <- read <$> count 2 digit
  char ':'
  m <- read <$> count 2 digit
  someSpace
  return $ TimeOfDay h m 0

parseTimeActivity :: Day -> Parser (LocalTime, TimeActivity)
parseTimeActivity d = do
  t <- parseTimeOfDay
  s <- some (noneOf "\n")
  let (a:_) = splitOn "--" s
      lt    = LocalTime d t
      ta    = TimeActivity lt a 0
  skipEOL
  return (lt, ta)

-----------------------------------------------------------------------------------

parseComments :: Parser Comments
parseComments = try (someSpace >> string "--") <|> string "--"

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments = skipMany $ do
  parseComments
  skipMany (noneOf "\n")
  skipEOL

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

-----------------------------------------------------------------------------------

dailyLogs :: ByteString
dailyLogs = [r|

-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

-----------------------------------------------------------------------------------

