{-# LANGUAGE InstanceSigs #-}

module LiftAndLiftIOExercises where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  password <- runMaybeT getPassword
  case password of
    Just x  -> putStrLn $ "valid password!!! " ++ x
    Nothing -> putStrLn "invalid password!"

isValid :: String -> Bool
isValid = (>= 10) . length

getPassword :: MaybeT IO String
getPassword = do
  password <- lift getLine
  guard (isValid password)
  return password

-----------------------------------------------------------------------------------

type MyDeeperStack = ReaderT Int (WriterT String (MaybeT (ExceptT String IO))) Bool

main' :: IO ()
main' = do
  password <- runExceptT . runMaybeT $ getPassword'
  case password of
    Right (Just x) -> putStrLn $ "valid password!!! " ++ x
    _ -> putStrLn "invalid password!"

getPassword' :: MaybeT (ExceptT String IO) String
getPassword' = do
  password <- liftIO getLine
  guard (isValid password)
  return password

-----------------------------------------------------------------------------------



