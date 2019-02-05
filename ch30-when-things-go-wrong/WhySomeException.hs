{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module WhySomeException where

import Control.Exception
  ( ArithException(..)
  , AsyncException(..))

import Data.Typeable

-----------------------------------------------------------------------------------

data MyException =
  forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

-----------------------------------------------------------------------------------

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    _ -> Right n

-----------------------------------------------------------------------------------

-- |
-- Prelude> :t cast
-- cast :: (Typeable b, Typeable a) => a -> Maybe b

data SomeError =
    Arith ArithException
  | Async AsyncException
  | SomethingElse
  deriving (Show)

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
  case cast e of
    (Just arith) -> Arith arith
    Nothing      ->
      case cast e of
        (Just async) -> Async async
        Nothing      -> SomethingElse

-----------------------------------------------------------------------------------

runDisc n =
  either discriminateError
  (const SomethingElse) (multiError n)

-- |
-- Prelude> runDisc 0
-- Arith divide by zero
--
-- Prelude> runDisc 1
-- Async stack overflow
--
-- Prelude> runDisc 2
-- SomethingElse
--

-----------------------------------------------------------------------------------




