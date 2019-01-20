{-# LANGUAGE InstanceSigs #-}

module FixTheCodeExercises where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

-----------------------------------------------------------------------------------

-- |
-- Use `hoogle` to search for `guard` function
--
-- Prelude> :t guard
-- guard :: GHC.Base.Alternative f => Bool -> f ()

-----------------------------------------------------------------------------------

isValid :: String -> Bool
isValid v = '!' `elem` v

-----------------------------------------------------------------------------------

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
  guard $ isValid v
  case isValid v of
    True  -> return $ Just v
    False -> return Nothing

-----------------------------------------------------------------------------------

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT $ maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)

-----------------------------------------------------------------------------------

-- |
-- original codes needed fixing as chapter exercise
--
--maybeExcite :: MaybeT IO String
--maybeExcite = do
--  v <- getLine
--  guard $ isValid v
--  return v
--
-------------------------------------------------------------------------------------
--
--doExcite :: IO ()
--doExcite = do
--  putStrLn "say something excite!"
--  excite <- maybeExcite
--  case excite of
--    Nothing -> putStrLn "MOAR EXCITE"
--    Just e -> putStrLn ("Good, was very excite: " ++ e)

-----------------------------------------------------------------------------------
