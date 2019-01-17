{-# LANGUAGE InstanceSigs #-}

module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

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
-- ReaderT awaits a `unit` as its argument and will throw an error running without it
--
-- Prelude> (runReaderT . runExceptT . runMaybeT $ embedded)
-- No instance for (Show (() -> IO (Either String (Maybe Int))))
--
-- Prelude> (runReaderT . runExceptT . runMaybeT $ embedded) ()
-- Right (Just 1)
--
-- Prelude> (runReaderT . runExceptT . runMaybeT $ return 1) ()
-- Right (Just 1)

-----------------------------------------------------------------------------------
-- `Lexically inner is structurally outer` samples
-----------------------------------------------------------------------------------

-- |
-- Prelude> (runReaderT . runMaybeT $ embedded') ()
-- Just 1
--
-- Prelude> (runReaderT . runMaybeT $ return 1) ()
-- Just 1

embedded' :: MaybeT (ReaderT () IO) Int
embedded' = return 1

-- |
-- Prelude> (runExceptT . runMaybeT $ embedded'')
-- Right (Just 1)
--
-- Prelude> (runExceptT . runMaybeT $ return 1)
-- Right (Just 1)

embedded'' :: MaybeT (ExceptT String IO) Int
embedded'' = return 1

-- |
-- Prelude> (runReaderT . runExceptT $ embedded''') ()
-- Right 1
--
-- Prelude> (runReaderT . runExceptT $ return 1) ()
-- Right 1

embedded''' :: ExceptT String (ReaderT () IO) Int
embedded''' = return 1

-- |
-- Prelude> (runMaybeT . runExceptT $ embedded'''')
-- Just (Right 1)
--
-- Prelude> (runMaybeT . runExceptT $ return 1)
-- Just (Right 1)

embedded'''' :: ExceptT String (MaybeT IO) Int
embedded'''' = return 1

-- |
-- Prelude> (runStateT . runExceptT . runMaybeT $ embeddedSt) "abc"
-- (Right (Just 1),"abc")
--
-- Prelude> (runStateT . runExceptT . runMaybeT $ return 1) "xyz"
-- (Right (Just 1),"xyz")
embeddedSt :: MaybeT (ExceptT String (StateT String IO)) Int
embeddedSt = return 1

-- |
-- Prelude> (runReaderT $ ((runStateT . runExceptT . runMaybeT $ embeddedRdSt) "abc")) ()
-- (Right (Just 1),"abc")
--
-- Prelude> (runReaderT $ ((runStateT . runExceptT . runMaybeT $ return 1) "xyz")) ()
-- (Right (Just 1),"xyz")
embeddedRdSt :: MaybeT (ExceptT String (StateT String (ReaderT () IO))) Int
embeddedRdSt = return 1

-- |
-- Prelude> (runStateT $ ((runReaderT . runExceptT . runMaybeT $ embeddedStRd) "hello")) "world"
-- (Right (Just 1),"world")
--
-- Prelude> (runStateT $ ((runReaderT . runExceptT . runMaybeT $ return 1) "hello")) "world"
-- (Right (Just 1),"world")
embeddedStRd :: MaybeT (ExceptT String (ReaderT String (StateT String IO))) Int
embeddedStRd = return 1

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

embeddedX :: MaybeT (ExceptT String (ReaderT () IO)) Int
embeddedX = MaybeT $ ExceptT $ ReaderT $ return <$> const (Right (Just 1))

maybeUnwrapX :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrapX = runMaybeT embeddedX

eitherUnwrapX :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrapX = runExceptT maybeUnwrapX

readerUnwrapX :: IO (Either String (Maybe Int))
readerUnwrapX = runReaderT eitherUnwrapX ()

-----------------------------------------------------------------------------------




