{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module TryExceptException where

import Control.Exception

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

-- |
-- Prelude> willIFail 1
-- 5
-- Right ()
--
-- Prelude> willIFail 0
-- Left divide by zero

-----------------------------------------------------------------------------------

-- |
-- To print the result only without the Left or Right

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e  -> print e
    Right _ -> return ()

willFail :: Integer -> IO ()
willFail denom = onlyReportError $ willIFail denom

-- |
-- Prelude> willFail 1
-- 5
--
-- Prelude> willFail 0
-- divide by zero

-----------------------------------------------------------------------------------


-- |
-- To print the result only without the Left or Right
-- Or we could use catch:

willIFail' :: Integer -> IO ()
willIFail' denom =
  print (div 5 denom) `catch` handler
  where handler :: ArithException -> IO ()
        handler e = print e

-- |
-- Prelude> willIFail' 1
-- 5
--
-- Prelude> willIFail' 0
-- divide by zero

-----------------------------------------------------------------------------------


