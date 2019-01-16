{-# LANGUAGE InstanceSigs #-}

module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-----------------------------------------------------------------------------------

-- |
-- *** Reminder to myself ***
-- Come back to this chapter regarding `Lexically inner is structurally outer`
--
-- One of the trickier parts of monad transformers is that the lexical representation
-- of the types will violate your intuitions with respect to the relationship it has
-- with the structure of our values.
--
-- newtype MaybeT    m a = MaybeT  { runMaybeT  :: m (Maybe a) }
-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
--
-- A necessary byproduct of how transformers work is that the additional structure `𝑚`
-- is always wrapped around our value. One thing to note is that it’s only wrapped around
-- things we can have, not things we need, such as with `ReaderT`. The consequence of this
-- is that a series of monad transformers in a type will begin with the innermost type
-- structurally speaking.
--
-- A terminological point to keep in mind when reading about monad transformers is that when
-- `Haskellers` say base monad they usually mean what is structurally outermost.
--
-- type MyType a = IO [Maybe a]
--
-- In MyType, the base monad is IO.

-----------------------------------------------------------------------------------

-- We only need to use return once because it's one big Monad

-- |
-- newtype MaybeT m a = MaybeT { runMaybeT  :: m (Maybe a) }
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

-- |
-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- |
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-----------------------------------------------------------------------------------

-- |
-- `readerUnwrapWithUnit` - Manually passing-in the `unit` type to `eitherUnwrap` as
--                         `ReaderT` awaits for `()` on its argument to produce a function
--                          that when applied will result a monadic structure of
--                         `IO (Either String (Maybe Int))`
--
-- The question is
-- How does the `unit`, ReaderT's argument, propagated from `eitherUnwrap` to `maybeUnwrap`
-- and finally to `embedded` function's most inner monadic structure of `ReaderT () IO`???
readerUnwrapWithUnit :: IO (Either String (Maybe Int))
readerUnwrapWithUnit = runReaderT eitherUnwrap ()

-----------------------------------------------------------------------------------

-- |
-- using `String` as argument to `ReaderT` as oppose to a `unit`
--
-- Prelude> readerUnwrapStr
-- Right (Just "xyz")
embeddedStr :: MaybeT (ExceptT String (ReaderT String IO)) String
embeddedStr = return "xyz"

maybeUnwrapStr :: ExceptT String (ReaderT String IO) (Maybe String)
maybeUnwrapStr = runMaybeT embeddedStr

eitherUnwrapStr :: ReaderT String IO (Either String (Maybe String))
eitherUnwrapStr = runExceptT maybeUnwrapStr

readerUnwrapStr :: IO (Either String (Maybe String))
readerUnwrapStr = runReaderT eitherUnwrapStr "abc"

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- Prelude> readerUnwrap ()
-- Right (Just 1)


-- |
-- We can treat having used `return` for the Reader/Either/Maybe stack as composition,
-- consider how we get the same result as `readerUnwrap ()`
--
-- e.g.
-- Prelude> (const . Right . Just $ 1) ()
-- Right (Just 1)
--
-- Prelude> (const . Right . Just $ 1) 1
-- Prelude> (const . Right . Just $ 1) Nothing
-- Prelude> (const . Right . Just $ 1) "abc"

{-
instance Monad ((->) r) where
  return = const

instance Monad (Either e) where
  return = Right

instance Monad Maybe where
  return = Just
-}

-----------------------------------------------------------------------------------

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT $ ExceptT $ ReaderT $ return <$> const (Right (Just 1))

maybeUnwrap' :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap' = runMaybeT embedded'

eitherUnwrap' :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap' = runExceptT maybeUnwrap'

readerUnwrap' :: IO (Either String (Maybe Int))
readerUnwrap' = runReaderT eitherUnwrap' ()

-----------------------------------------------------------------------------------




