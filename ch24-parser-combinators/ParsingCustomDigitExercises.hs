{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParsingCustomDigitExercises where

import Control.Applicative
import Control.Monad
import Data.Char
import Text.Trifecta
import Text.RawString.QQ

-----------------------------------------------------------------------------------
-- custom parser for digit/integer values (NOT using the digit/integer parser)
-----------------------------------------------------------------------------------

-- e.g.
-- parseString parseDigit mempty "123" -> Success '1'
-- parseString parseDigit mempty "abc" -> Failure (interactive):1:1: error: expected: parseDigit; abc<EOF>
parseDigit :: Parser Char
parseDigit = oneOf "0123456789" <?> "not an integer"

-- parseString base10Integer mempty "123abc" -> Success '123'
-- parseString base10Integer mempty "abc"    ->
-- Failure (interactive):1:1: error: expected: parseDigit; abc<EOF>
base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

-- parseString base10Integer' mempty "-123abc" -> Success '123'
-- parseString base10Integer' mempty "abc"     ->
-- Failure (interactive):1:1: error: expected: parseDigit; abc<EOF>
base10Integer' :: Parser Integer
base10Integer' = do
  neg <- optional (char '-')
  xs  <- some parseDigit
  case neg of
    Nothing -> return $ read xs
    Just x  -> return $ read $ x:xs

-----------------------------------------------------------------------------------
