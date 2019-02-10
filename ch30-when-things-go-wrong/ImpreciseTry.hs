module ImpreciseTry where

import Control.Exception

-----------------------------------------------------------------------------------

canICatch :: Exception e => e -> IO (Either ArithException ())
canICatch e = try $ throwIO e

-- |
-- Prelude> canICatch DivideByZero
-- Left divide by zero
--
-- Prelude> canICatch StackOverflow
-- *** Exception: stack overflow
--
-- Prelude> :t DivideByZero
-- DivideByZero :: ArithException
--
-- Prelude> :t StackOverflow
-- StackOverflow :: AsyncException

-- |
-- The latter case blew past our try because we were trying to catch an ArithException,
-- not an AsyncException. It was mentioned several times that SomeException will match
-- on all types that implement the Exception type class, so try rewriting the above such
-- that the StackOverflow or any other exception can also be caught.

-----------------------------------------------------------------------------------

canICatch' :: Exception e => e -> IO (Either SomeException ())
canICatch' e = try $ throwIO e

-- |
-- Prelude> canICatch' DivideByZero
-- Left divide by zero
--
-- Prelude> canICatch' StackOverflow
-- Left stack overflow

-----------------------------------------------------------------------------------


