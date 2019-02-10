module StoppingTheParty where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)

-----------------------------------------------------------------------------------

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1..9]
  then throwIO DivideByZero
  else throwIO StackOverflow

main :: IO ()
main = forever $ do
  let tryS :: IO () -> IO (Either ArithException ())
      tryS = try
  _ <- tryS randomException -- allowing to continue with ArithException (DivideByZero)
  putStrLn "Live to loop another day!"

  -- microseconds
  threadDelay (1 * 1000000)

-- |
-- Prelude> main
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- *** Exception: stack overflow

-----------------------------------------------------------------------------------

main' :: IO ()
main' = forever $ do
  let tryS :: IO () -> IO (Either SomeException ())
      tryS = try
  _ <- tryS randomException -- allowing all Exceptions and loop will never terminate
  putStrLn "Live to loop another day!"

  -- microseconds
  threadDelay (1 * 1000000)

-- |
-- Prelude> main'
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!
-- Live to loop another day!

-----------------------------------------------------------------------------------


