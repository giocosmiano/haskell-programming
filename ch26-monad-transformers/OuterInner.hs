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
-- with the structure of your values.
--
-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- newtype MaybeT    m a = MaybeT  { runMaybeT  :: m (Maybe a) }
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
--
-- A necessary byproduct of how transformers work is that the additional structure `𝑚`
-- is always wrapped around our value. One thing to note is that it’s only wrapped around
-- things we can have, not things we need, such as with `ReaderT`. The consequence of this
-- is that a series of monad transformers in a type will begin with the innermost type
-- structurally speaking
--
-- A terminological point to keep in mind when reading about monad transformers is that when
-- `Haskellers` say base monad they usually mean what is structurally outermost.
--
-- type MyType a = IO [Maybe a]
--
-- In MyType, the base monad is IO.

-----------------------------------------------------------------------------------

-- We only need to use return once because it's one big Monad
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- Prelude> readerUnwrap ()
-- Right (Just 1)


-- |
-- We can treat having used return for the Reader/Either/Maybe stack as composition,
-- consider how we get the same result as `readerUnwrap ()`
--
-- e.g.
-- Prelude> (const . Right . Just $ 1) ()
-- Right (Just 1)

{-
instance Monad ((->) r) where
  return = const

instance Monad (Either e) where
  return = Right

instance Monad Maybe where
  return = Just
-}


-----------------------------------------------------------------------------------




