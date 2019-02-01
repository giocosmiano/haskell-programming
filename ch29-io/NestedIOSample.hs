module NestedIOSample where

import Data.Time.Calendar
import Data.Time.Clock
import System.Random

-----------------------------------------------------------------------------------

-- |
-- Prelude> :t getCurrentTime
-- getCurrentTime :: IO UTCTime
--
-- Prelude> :t toGregorian
-- toGregorian :: Day -> (Integer, Int, Int)
--
-- Prelude> :t utctDay
-- utctDay :: UTCTime -> Day
--
-- Prelude> :t randomIO
-- randomIO :: Random a => IO a
--
-- Prelude> :t either
-- either :: (a -> c) -> (b -> c) -> Either a b -> c

hueHue :: IO (Either (IO Int) (IO ()))
hueHue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  case even dayOfMonth of
    True ->  return $ Left  randomIO
    False -> return $ Right (putStrLn $ "The dayOfMonth is " ++ show dayOfMonth)

main :: IO ()
main = do
  blah <- hueHue
  either (>>= print) id blah

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- The dayOfMonth is 1

-- |
-- What sets the IO Monad apart from the Applicative is that the effects performed by the
-- outer IO action can influence what recipe we get in the inner part. The nesting also lets
-- us express order dependence, a useful trick for lambda calculi noted by Peter J. Landin
--
-- see -> A correspondence between ALGOL 60 and Church’s Lambda-notations; P.J. Landin

-----------------------------------------------------------------------------------




