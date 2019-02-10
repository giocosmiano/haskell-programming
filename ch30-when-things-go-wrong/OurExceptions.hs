module OurExceptions where

import Control.Exception

-----------------------------------------------------------------------------------

data NotDivThree = NotDivThree Int
                 deriving (Eq, Show)

instance Exception NotDivThree

data NotEven = NotEven Int
             deriving (Eq, Show)

instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | odd i = throwIO (NotEven i)
  | otherwise = return i

-----------------------------------------------------------------------------------

-- |
-- Prelude> evenAndThreeDiv 12
-- 12
--
-- Prelude> evenAndThreeDiv 9
-- *** Exception: NotEven 9
--
-- Prelude> evenAndThreeDiv 8
-- *** Exception: NotDivThree 8
--
-- Prelude> evenAndThreeDiv 3
-- *** Exception: NotEven 3
--
-- Prelude> evenAndThreeDiv 2
-- *** Exception: NotDivThree 2

-----------------------------------------------------------------------------------

-- |
-- Defined in ‘Control.Exception’
-- data Handler a where
--   Handler :: Exception e => (e -> IO a) -> Handler a
--
-- Prelude> :t catches
-- catches :: IO a -> [Handler a] -> IO a

catchNotDivThree :: IO Int -> (NotDivThree -> IO Int) -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int -> (NotEven -> IO Int) -> IO Int
catchNotEven = catch

catchBoth :: IO Int -> IO Int
catchBoth ioInt = catches ioInt
  [ Handler
      (\(NotEven _) -> return maxBound)
  , Handler
      (\(NotDivThree _) -> return minBound)
  ]

-- |
-- Prelude> type EA e = IO (Either e Int)
--
-- Prelude> try (evenAndThreeDiv 2) :: EA NotEven
-- *** Exception: NotDivThree 2
--
-- Prelude> try (evenAndThreeDiv 2) :: EA NotDivThree
-- Left (NotDivThree 2)

-----------------------------------------------------------------------------------

data EATD = NotEven' Int
          | NotDivThree' Int
          deriving (Eq, Show)

instance Exception EATD
evenAndThreeDiv' :: Int -> IO Int
evenAndThreeDiv' i
  | rem i 3 /= 0 = throwIO (NotDivThree' i)
  | odd i = throwIO (NotEven' i)
  | otherwise = return i

-- |
-- Prelude> evenAndThreeDiv' 12
-- 12
--
-- Prelude> evenAndThreeDiv' 9
-- *** Exception: NotEven 9
--
-- Prelude> evenAndThreeDiv' 8
-- *** Exception: NotDivThree 8
--
-- Prelude> evenAndThreeDiv' 3
-- *** Exception: NotEven 3
--
-- Prelude> evenAndThreeDiv' 2
-- *** Exception: NotDivThree 2

-- |
-- Prelude> type EA e = IO (Either e Int)
--
-- Prelude> try (evenAndThreeDiv' 0) :: EA EATD
-- Right 0
--
-- Prelude> try (evenAndThreeDiv' 1) :: EA EATD
-- Left (NotDivThree 1)

-----------------------------------------------------------------------------------

