module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import System.IO

-----------------------------------------------------------------------------------

data PleaseDie = PleaseDie
               deriving Show

instance Exception PleaseDie

-----------------------------------------------------------------------------------

-- |
--
-- Prelude> :t forkIO
-- forkIO :: IO () -> IO GHC.Conc.Sync.ThreadId
--
-- Prelude> :t mask_
-- mask_ :: IO a -> IO a

openAndWrite :: IOMode -> IO ()
openAndWrite ioMode = do
  h <- openFile "test.dat" ioMode
  -- You may need to jiggle this
  threadDelay 1500
  hPutStr h (replicate 100000000 '0' ++ "abc")
  hClose h

main :: IO ()
main = do
  threadId <- forkIO $ openAndWrite WriteMode
  threadDelay 1000
  throwTo threadId PleaseDie

-----------------------------------------------------------------------------------

-- |
-- If we run this program, the intended result is that we’ll have a file named `test.dat`
-- with only zeroes that didn’t reach the “abc” at the end. Since we can’t predict the future,
-- if we have a disk with preternaturally fast I/O, increase the arguments to replicate to
-- reproduce the intended issue. If it ain’t broken, break it.
--
-- What happened was that we threw an asynchronous exception from the main thread to our
-- child thread, short-circuiting what we were doing in the middle of doing it. If we did this
-- in a loop, we’d leak file handles, too. Done continually over a period of time, leaking
-- file handles can cause our process to get killed or our computer to become unstable.
--
-- We can think of asynchronous exceptions as exceptions raised from a different thread
-- than the one that’ll receive the error. They’re immensely useful and give us a means of
-- talking about error conditions that are quite real and possible in languages that don’t
-- have formal asynchronous exceptions. Our process can get axe-murdered by the operating system
-- out of nowhere in any language. We just happen to have the ability to do the same within
-- the programming language at the thread level as well. The issue is that we want to temporarily
-- ignore exceptions until we’ve finished what we’re doing. This is so the state of the file
-- is correct but also so that we don’t leak resources like file handles or perhaps database
-- connections or something similar.

main' :: IO ()
main' = do
  threadId <- forkIO (mask_ $ openAndWrite AppendMode)
  threadDelay 1000
  throwTo threadId PleaseDie

-- |
-- Here we used `mask_` from Control.Exception in order to mask or delay exceptions thrown to our
-- child thread until the IO action openAndWrite was complete. Incidentally, since the end of
-- the mask is the last thing our child thread does, the exception our main thread tried to throw
-- to the child blows up in its face and is now thrown within the main thread.
--
-- Async exceptions are helpful and manifest in less obvious ways in other language runtimes and
-- ecosystems. Don’t try to catch everything; just let it die, and make sure we have a process
-- supervisor and good logs. No execution is better than bad execution.

-----------------------------------------------------------------------------------

