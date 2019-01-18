{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottySample where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)
import Data.Monoid (mconcat)

-----------------------------------------------------------------------------------
-- |
-- All the (..) means is that we want to import all the data constructors of the
-- ActionT type, rather than none or a particular list of them.

-----------------------------------------------------------------------------------
-- |
-- Lift a computation from the argument monad to the constructed monad.
--
-- Here the `t` is a (constructed) monad transformer type that has an instance of MonadTrans defined.
--
-- class MonadTrans t where
--   lift :: (Monad m) => m a -> t m a

-----------------------------------------------------------------------------------
-- |
-- newtype ScottyT e m a =
--   ScottyT
--   { runS
--     :: State (ScottyState e m) a
--   }
--   deriving (Functor, Applicative, Monad)
--
-- newtype ActionT e m a =
--   ActionT
--   { runAM
--     :: ExceptT
--          (ActionError e)
--          (ReaderT ActionEnv
--            (StateT ScottyResponse m))
--          a
--   }
--   deriving ( Functor, Applicative )
--
-- type ScottyM = ScottyT Text IO
-- type ActionM = ActionT Text IO
--
-- We’ll use ActionM and ActionT and ScottyM and ScottyT as if they were the same thing, but the M variants
-- are type synonyms for the transformers with the inner types already set. This roughly translates to the
-- errors (the left side of the ExceptT) in ScottyM or ActionM being returned as Text, while the right side
-- of the ExceptT, whatever it does, is IO. ExceptT is the transformer version of Either, and a ReaderT and
-- a StateT are stacked up inside that as well.
--
-- Prelude> :t putStrLn
-- putStrLn :: String -> IO ()
--
-- Prelude> :t get
-- get :: RoutePattern -> ActionM () -> ScottyM ()
--
-- Prelude> :t runAM
-- runAM
--   :: ActionT e m a
--      -> ExceptT
--           (Web.Scotty.Internal.Types.ActionError e)
--           (Control.Monad.Trans.Reader.ReaderT
--              Web.Scotty.Internal.Types.ActionEnv
--              (Control.Monad.Trans.State.Lazy.StateT
--                 Web.Scotty.Internal.Types.ScottyResponse m))
--           a
--
--
-- Prelude> :t lift
-- lift :: (Monad m, MonadTrans t) => m a -> t m a
--
-- Prelude> :t lift (putStrLn "hello")
-- lift (putStrLn "hello") :: MonadTrans t => t IO ()

-----------------------------------------------------------------------------------
-- |
-- *** See `get` and `putStrLn` type signatures above ***
--
-- *** Error we're trying to solve, if putStrLn "hello" isn't lifted under the get $ do below ***
--     * Couldn't match type `IO'
--                      with `ActionT Data.Text.Internal.Lazy.Text IO'
--       Expected type: ActionT Data.Text.Internal.Lazy.Text IO ()
--         Actual type: IO ()
--
-- The reason for this type error is that putStrLn has the type IO (), but it is inside a do block inside
-- our get, and the monad that code is in is therefore ActionM/ActionT

-----------------------------------------------------------------------------------
-- |
-- `lift` it all in 1-big monadic action (ExceptT, ReaderT and StateT)
-- to bring `putStrLn` into `ActionM/ActionT` monadic structure
-----------------------------------------------------------------------------------

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    lift $ putStrLn "hello"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------
-- |
-- `lift` ExceptT, ReaderT and StateT individually
-- to bring `putStrLn` into `ActionM/ActionT` monadic structure
--
-- instance MonadTrans (ActionT e) where
--   lift = ActionT . lift . lift . lift
-----------------------------------------------------------------------------------

-- |
-- Lexically inner is structurally outer
-- `lift` ExceptT, then `lift` ReaderT and finally `lift` StateT
-- (see `runAM` type signature above)

mainLiftIO = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT . lift . lift . lift) $ putStrLn "hello"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------
-- |
-- `lift` ExceptT manually
-- to bring `putStrLn` into `ActionM/ActionT` monadic structure
--
-- instance MonadTrans (ExceptT e) where
--   lift = ExceptT . fmap Right
-----------------------------------------------------------------------------------

mainLiftIO' = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT
      . (ExceptT . fmap Right)
      . lift
      . lift) $ putStrLn "hello"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------
-- |
-- `lift` ExceptT and ReaderT manually
-- to bring `putStrLn` into `ActionM/ActionT` monadic structure
--
-- instance MonadTrans (ReaderT r) where
--   lift = liftReaderT
-----------------------------------------------------------------------------------

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

-- |
-- OR
-- mainLiftIO'' = scotty 3000 $ do
--   get "/:word" $ do
--     beam <- param "word"
--     (ActionT
--       . (ExceptT . fmap Right)
--       . liftReaderT
--       . lift) $ putStrLn "hello"
--     html $ mconcat
--       [ "<h1>Scotty, "
--       , beam
--       , " me up!</h1>" ]

-- |
-- OR
mainLiftIO'' = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT
      . (ExceptT . fmap Right)
      . (\m -> ReaderT (const m))
      . lift) $ putStrLn "hello"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-- |
-- OR
-- mainLiftIO'' = scotty 3000 $ do
--   get "/:word" $ do
--     beam <- param "word"
--     (ActionT
--       . (ExceptT . fmap Right)
--       . ReaderT . const
--       . lift) $ putStrLn "hello"
--     html $ mconcat
--       [ "<h1>Scotty, "
--       , beam
--       , " me up!</h1>" ]

-----------------------------------------------------------------------------------
-- |
-- `lift` ExceptT, ReaderT and StateT manually
-- to bring `putStrLn` into `ActionM/ActionT` monadic structure
--
-- instance MonadTrans (StateT s) where
--   lift m = StateT $ \s -> do
--     a <- m
--     return (a, s)
-----------------------------------------------------------------------------------

mainLiftIO''' = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT
      . (ExceptT . fmap Right)
      . (\m -> ReaderT (const m))
      . (\m -> StateT (\s -> do
                         a <- m
                         return (a,s)))
      ) $ putStrLn "hello"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------
-- |
-- Typically a MonadTrans instance lifts over only one layer at a time, but scotty abstracts
-- away the underlying structure so that you don’t have to care. That’s why it goes ahead
-- and does the next three lifts for you. The critical thing to realize here is that lifting
-- means you’re embedding an expression in a larger
--
-----------------------------------------------------------------------------------





