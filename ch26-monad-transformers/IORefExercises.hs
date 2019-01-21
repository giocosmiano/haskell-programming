{-# LANGUAGE InstanceSigs #-}

module IOExercises where

import Data.IORef

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#data-ioref

main = do
    ref <- newIORef 0
    value <- readIORef ref
    print value

    writeIORef ref $ value + 1
    readIORef ref >>= print

    modifyIORef ref (+ 2)
    readIORef ref >>= print

-----------------------------------------------------------------------------------


