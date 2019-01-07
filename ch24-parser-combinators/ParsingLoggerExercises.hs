{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParsingLoggerExercises where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.ByteString (ByteString)
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn)

import Data.Time
import Text.Trifecta
import Text.RawString.QQ

-----------------------------------------------------------------------------------
-- parser daily logger file
-----------------------------------------------------------------------------------

type Activity = String
type Comments = String
type TimeSpentInMinutes = Integer
type TimeActivities  = Map TimeOfDay Activity
type DailyActivities = Map Day TimeActivities

data TimeActivity = TimeActivity TimeOfDay Activity
                  deriving (Eq, Show)

data LogInfo = LogInfo Day TimeActivities
             deriving (Eq, Show)

-----------------------------------------------------------------------------------

-- e.g.
-- parseLogger
parseLogger :: Result [LogInfo]
parseLogger = parseByteString parseLogData mempty dailyLogs

-- e.g.
-- parseByteString parseLogData mempty dailyLogs
parseLogData :: Parser [LogInfo]
parseLogData = some $ do
  skipComments
  d <- parseDate
  a <- some parseTimeActivity
  return $ LogInfo d (M.fromList a)

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

parseTimeActivity :: Parser (TimeOfDay, Activity)
parseTimeActivity = do
  t <- parseTimeOfDay
  s <- some (noneOf "\n")
  let (a:_) = splitOn "--" s
  skipEOL
  return (t, a)

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

