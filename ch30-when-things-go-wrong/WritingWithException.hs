{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Main where

import Control.Exception
import Data.Typeable

-----------------------------------------------------------------------------------

-- |
--
-- class (Typeable e, Show e) => Exception e where
--   toException :: e -> SomeException
--   fromException :: SomeException -> Maybe e
--   displayException :: e -> String
--
-- data SomeException =
--   forall e . Exception e => SomeException e
--
-- data SomeException where
--   SomeException :: Exception e => e -> SomeException
--
-- Prelude> :t cast
-- cast :: (Typeable b, Typeable a) => a -> Maybe b
--
-- Prelude> :t catch
-- catch :: Exception e => IO a -> (e -> IO a) -> IO a
--
-- Prelude> :t try
-- try :: Exception e => IO a -> IO (Either e a)

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn ("We errored! It was: " ++ show e)

main :: IO ()
main = do
  writeFile "zzz.txt" "hi" `catch` handler
  putStrLn "wrote to file"

-----------------------------------------------------------------------------------

-- |
--
-- $ touch zzz.txt
--
-- $ chmod 400 zzz.txt
--
-- $ stack ghc -- WritingWithException.hs -o sampleException
--
-- $ ./sampleException
-- We errored! It was: zzz.txt: openFile: permission denied (Permission denied)

-----------------------------------------------------------------------------------




