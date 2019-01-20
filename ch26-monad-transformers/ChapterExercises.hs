{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where

import Data.Functor.Identity
import Control.Monad.Reader
import Control.Monad.State

-----------------------------------------------------------------------------------

-- |
-- Further reading
-- https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html
--
-- Prelude> :t ask
-- ask :: MonadReader r m => m r
--
-- Prelude> :t runReader
-- runReader :: Reader r a -> r -> a
--
-- e.g.
-- Prelude> runReader rDec 1
-- 0
--
-- Prelude> (runReader rDec) <$> [1..10]
-- [0,1,2,3,4,5,6,7,8,9]

rDec :: Num a => Reader a a
rDec = do
  n <- ask -- value from the environment
  return $ n - 1

-- |
-- using point-free
--
-- Prelude> :t reader
-- reader :: MonadReader r m => (r -> a) -> m a
--
-- e.g.
-- Prelude> runReader rDec' 1
-- 0
--
-- Prelude> (runReader rDec') <$> [1..10]
-- [0,1,2,3,4,5,6,7,8,9]

rDec' :: Num a => Reader a a
rDec' = reader $ \x -> x - 1

-----------------------------------------------------------------------------------

-- |
-- Not sure how to return the value of Identity when the function type is `ReaderT a Identity String`
--
-- e.g.
-- Prelude> runReaderT rShow 1
-- Identity "1"
--
-- Prelude> fmap (runReaderT rShow) [1..5]
-- [Identity "1",Identity "2",Identity "3",Identity "4",Identity "5"]
--
-- For now, just use `runIdentity` to get the value of `Identity`
--
-- e.g.
-- Prelude> runIdentity $ runReaderT rShow 1
-- "1"
--
-- Prelude> runIdentity <$> fmap (runReaderT rShow) [1..5]
-- ["1","2","3","4","5"]

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \x -> return $ show x

-----------------------------------------------------------------------------------

-- |
--
-- e.g.
-- Prelude> runReaderT rPrintAndInc 1
-- Hi: 1
-- 2
--
-- e.g.
-- Prelude> traverse (runReaderT rPrintAndInc) [1..5]
-- Hi: 1
-- Hi: 2
-- Hi: 3
-- Hi: 4
-- Hi: 5
-- [2,3,4,5,6]

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT $ \x -> do
    liftIO $ print ("Hi: " ++ show x)
    return $ x + 1

-----------------------------------------------------------------------------------

-- |
--
-- e.g.
-- Prelude> runStateT sPrintIncAccum 10
-- Hi: 10
-- ("10",11)
--
-- e.g.
-- Prelude> mapM (runStateT sPrintIncAccum) [1..5]
-- Hi: 1
-- Hi: 2
-- Hi: 3
-- Hi: 4
-- Hi: 5
-- [("1",2),("2",3),("3",4),("4",5),("5",6)]

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum =
  StateT $ \x -> do
    liftIO $ print ("Hi: " ++ show x)
    return $ (show x, x + 1)

-----------------------------------------------------------------------------------


