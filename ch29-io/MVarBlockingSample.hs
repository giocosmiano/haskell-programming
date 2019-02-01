module MVarBlockingSample where

import Control.Concurrent
import System.IO.Unsafe

-----------------------------------------------------------------------------------

-- |
-- Prelude> :t newEmptyMVar
-- newEmptyMVar :: IO (MVar a)
--
-- Prelude> :t putMVar
-- putMVar :: MVar a -> a -> IO ()
--
-- Prelude> :t takeMVar
-- takeMVar :: MVar a -> IO a
--
-- Prelude> :t unsafePerformIO
-- unsafePerformIO :: IO a -> a

myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- *** Exception: thread blocked indefinitely in an MVar operation

-- |
-- Essentially, when we see a type like `IO String`
-- It means we don’t have a String but rather we have a means of (possibly) obtaining a String,
-- with some effects possibly performed along the way.
--
-- so with the type `IO (MVar a)`,
-- It tells us that we have a recipe for producing as many empty `MVars` as we want, not a
-- reference to a single shared `MVar`.
--
-- We can share the `MVar`, but it has to be done explicitly rather than implicitly. Failing
-- to explicitly share the `MVar` reference after binding it once will simply spew out new,
-- empty `MVars`.

-----------------------------------------------------------------------------------

myData' :: MVar Int
myData' = unsafePerformIO newEmptyMVar

main' :: IO ()
main' = do
  putMVar myData' 0
  zero <- takeMVar myData'
  print zero

-----------------------------------------------------------------------------------

-- |
-- Prelude> main'
-- 0

-- |
-- The type of unsafePerformIO is IO a -> a, which is seemingly impossible and not a
-- good idea in general. In real code, we should pass references to MVars as an argument
-- or via ReaderT, but the combination of MVar and unsafePerformIO gives us an opportunity
-- to see in very stark terms what it means to use unsafePerformIO in our code. The new
-- empty MVar can now be shared implicitly, as often as we want, instead of creating a
-- new one each time.



