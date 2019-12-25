{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module AnotherEitherTExercises where

import Data.Text

import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

-----------------------------------------------------------------------------------
-- https://two-wrongs.com/a-gentle-introduction-to-monad-transformers
-- https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
-- We need to import Data.Text and set the OverloadedStrings pragma.
-- The latter lets us write string literals (such as "Hello, world!")
-- and have them become Text values automatically
-----------------------------------------------------------------------------------

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving Show

-- |
-- e.g.
-- :module +Data.Text
-- :set -XOverloadedStrings
-- getDomain "test@example.com"          -> Right "example.com"
-- getDomain "invalid.email@example@com" -> Left InvalidEmail

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- :module +Data.Text
-- :set -XOverloadedStrings
-- printResult' (getDomain "test@example.com") -> Domain: example.com
-- printResult' (getDomain "test#example.com") -> ERROR: Invalid domain
printResult' :: Either LoginError Text -> IO ()
printResult' domain =
  case domain of
    Right text        -> T.putStrLn (append "Domain: " text)
    Left InvalidEmail -> T.putStrLn "ERROR: Invalid domain"

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- :module +Data.Text
-- :set -XOverloadedStrings
-- printResult'' (getDomain "test@example.com") -> Domain: example.com
-- printResult'' (getDomain "test#example.com") -> ERROR: Invalid domain
printResult'' :: Either LoginError Text -> IO ()
printResult'' = T.putStrLn . either
  (const "ERROR: Invalid domain")
  (append "Domain: ")

-----------------------------------------------------------------------------------

getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  return (getDomain email)

-----------------------------------------------------------------------------------

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

-----------------------------------------------------------------------------------

userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken

  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userPw -> do
          T.putStrLn "Enter password:"
          password <- T.getLine

          if userPw == password
             then return token
             else return (Left WrongPassword)

        Nothing -> return (Left NoSuchUser)
    left -> return left

-----------------------------------------------------------------------------------















