module FunctorAddUTCTimeSample where

import Data.Time.Clock

-----------------------------------------------------------------------------------

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $ getCurrentTime

-----------------------------------------------------------------------------------

-- Lifting over IO
--
-- Here we’re taking a function that doesn’t perform I/O, addUTCTime,
-- partially applying it to the offset we’re going to add to the second
-- argument, then mapping it over the IO action that gets us
-- the current time
--
-- 1. NominalDiffTime is a newtype of Pico and has a Num instance, that’s why the arithmetic works.
-- addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
--
-- 2. getCurrentTime :: IO UTCTime
--
-- 3. fmap’s type got specialized.
-- fmap :: (UTCTime -> UTCTime) -> IO UTCTime -> IO UTCTime
--
-- Some reference materials to read
--
-- 1. A Haskell Time Library Tutorial
--    https://two-wrongs.com/haskell-time-library-tutorial.html
--
-- 2. 24 Days of Hackage: time
--    https://ocharles.org.uk/posts/2013-12-15-24-days-of-hackage-time.html
