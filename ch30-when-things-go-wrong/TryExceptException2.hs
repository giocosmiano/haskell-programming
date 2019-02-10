module Main where

import Control.Exception
import System.Environment (getArgs)

-----------------------------------------------------------------------------------

-- |
--
-- Prelude> :t cast
-- cast :: (Typeable b, Typeable a) => a -> Maybe b
--
-- Prelude> :t catch
-- catch :: Exception e => IO a -> (e -> IO a) -> IO a
--
-- Prelude> :t try
-- try :: Exception e => IO a -> IO (Either e a)
--
-- Prelude> :t throw
-- throw :: Exception e => e -> a
--
-- Prelude> :t throwIO
-- throwIO :: Exception e => e -> IO a

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e  -> print e
    Right _ -> return ()

testDiv :: String -> IO ()
testDiv d = onlyReportError $ willIFail (read d)

main :: IO ()
main = do
  args <- getArgs
  mapM_ testDiv args

-- |
-- $ stack ghc -- TryExceptException2.hs -o sampleException
--
-- $ ./sampleException 4 5 0 9
-- 1
-- 1
-- divide by zero
-- 0

-- |
-- Prelude> :main 4 5 0 9
-- 1
-- 1
-- divide by zero
-- 0

-----------------------------------------------------------------------------------


