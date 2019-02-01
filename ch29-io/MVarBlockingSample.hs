module MVarBlockingSample where

import Control.Concurrent

-----------------------------------------------------------------------------------

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




