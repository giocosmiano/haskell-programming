module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

-- Six-sided die
data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Show)

-----------------------------------------------------------------------------------

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

-----------------------------------------------------------------------------------
-- mkStdGen :: Int -> StdGen
-----------------------------------------------------------------------------------
-- The idea is that it takes an Int argument and maps it into a generator to
-- return a value of type StdGen, which is a pair of Int32 values.
-- e.g.
-- mkStdGen 0 -> 1 1

-----------------------------------------------------------------------------------
-- next :: RandomGen g => g -> (Int, g)
-----------------------------------------------------------------------------------
-- The Int that is first in the tuple is the pseudo-random number generated from the
-- StdGen value; the second value is a new StdGen value.
-- e.g.
-- next $ mkStdGen 0 -> (2147482884,40014 40692)

-----------------------------------------------------------------------------------
-- random :: (RandomGen g, Random a) => g -> (a, g)
-----------------------------------------------------------------------------------
-- Similar to next but allows us to generate random values that aren’t numbers.
-- The range generated will be determined by the type.
-- e.g.
-- random $ mkStdGen 0 -> (9106162675347844341,1346387765 2103410263)

-----------------------------------------------------------------------------------
-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
-----------------------------------------------------------------------------------
-- Similar to `random` but within a range
-- e.g.
-- randomR (0,8192) $ mkStdGen 0 -> (7460,40014 40692)

-----------------------------------------------------------------------------------
-- evalState :: State s a -> s -> a
-----------------------------------------------------------------------------------
-- Gets the output value from State

-----------------------------------------------------------------------------------

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

-----------------------------------------------------------------------------------
-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-----------------------------------------------------------------------------------
-- the state function is a constructor that takes a State-like
-- function and embeds it in the State monad transformer.

-----------------------------------------------------------------------------------

-- State StdGen had a final type argument of Int.
-- We lifted Int-> Die over it and transformed that final type argument to Die.
-- e.g.
-- evalState rollDie' $ mkStdGen 0 -> DieSix
-- evalState rollDie' $ mkStdGen 1 -> DieSix
rollDie' :: State StdGen Die
rollDie' = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- State StdGen had a final type argument of Int.
-- We lifted Int-> Die over it and transformed that final type argument to Die.
-- e.g.
-- evalState rollDie'' $ mkStdGen 0 -> DieSix
-- evalState rollDie'' $ mkStdGen 1 -> DieSix
rollDie'' :: State StdGen Die
rollDie'' = intToDie <$> state (randomR (1, 6))

-- e.g.
-- evalState rollDieThreeTimes' $ mkStdGen 0 -> (DieSix,DieSix,DieFour)
-- evalState rollDieThreeTimes' $ mkStdGen 1 -> (DieSix,DieFive,DieTwo)
rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

-- e.g.
-- evalState rollDieThreeTimes'' $ mkStdGen 0 -> (DieSix,DieSix,DieFour)
-- evalState rollDieThreeTimes'' $ mkStdGen 1 -> (DieSix,DieFive,DieTwo)
rollDieThreeTimes'' :: State StdGen (Die, Die, Die)
rollDieThreeTimes'' = liftA3 (,,) rollDie'' rollDie'' rollDie''

-----------------------------------------------------------------------------------
-- replicateM :: Monad m => Int -> m a -> m [a]
-----------------------------------------------------------------------------------

-- e.g.
-- take 6 $ evalState infiniteDie (mkStdGen 0) -> [DieSix,DieSix,DieSix,DieSix,DieSix,DieSix]
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie'

-- e.g.
-- evalState (nDie 5) (mkStdGen 0) -> [DieSix,DieSix,DieFour,DieOne,DieFive]
-- evalState (nDie 5) (mkStdGen 1) -> [DieSix,DieFive,DieTwo,DieSix,DieFive]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

-----------------------------------------------------------------------------------
-- Number we keep rolling a single die until we reach or exceed a sum of 20
-----------------------------------------------------------------------------------

-- e.g.
-- rollsToGetTwenty $ mkStdGen 0 -> 5
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go  sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in  go (sum + die)
               (count + 1) nextGen

-----------------------------------------------------------------------------------
-- randomIO :: Random a => IO a
-----------------------------------------------------------------------------------

-- e.g.
-- rollsToGetTwentyUsingRandomIO -> 5
-- rollsToGetTwentyUsingRandomIO -> 7
-- rollsToGetTwentyUsingRandomIO -> 6
rollsToGetTwentyUsingRandomIO :: IO Int
rollsToGetTwentyUsingRandomIO = (rollsToGetTwenty . mkStdGen) <$> randomIO

-- NOTE ***
-- Under the hood, it’s the same interface and State Monad driven mechanism, but it’s
-- mutating a single globally used StdGen to walk the generator forward on each use.

-----------------------------------------------------------------------------------

-- e.g.
-- rollsToGetN 50 $ mkStdGen 0 -> 14
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 n g
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go  sum count n' gen
      | sum >= n' = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in  go (sum + die)
               (count + 1) n' nextGen

-----------------------------------------------------------------------------------
-- randomIO :: Random a => IO a
-----------------------------------------------------------------------------------

-- e.g.
-- rollsToGetNUsingRandomIO 50 -> 18
-- rollsToGetNUsingRandomIO 50 -> 15
-- rollsToGetNUsingRandomIO 50 -> 16
rollsToGetNUsingRandomIO :: Int -> IO Int
rollsToGetNUsingRandomIO n = ((rollsToGetN n) . mkStdGen) <$> randomIO

-----------------------------------------------------------------------------------

type CountLogger = (Int, [Die])

setCountLogger :: Int -> [Die] -> CountLogger
setCountLogger n d = (n, d)

rollsCountLogged :: Int -> StdGen -> CountLogger
rollsCountLogged n g = go 0 (setCountLogger 0 []) n g
  where
    go :: Int -> CountLogger -> Int -> StdGen -> CountLogger
    go  sum countLogger n' gen
      | sum >= n' = countLogger
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
            count = (fst countLogger) + 1
            dies  = (snd countLogger) ++ [intToDie die]
        in  go (sum + die) (setCountLogger count dies) n' nextGen

-----------------------------------------------------------------------------------
-- randomIO :: Random a => IO a
-----------------------------------------------------------------------------------

-- e.g.
-- rollsCountLoggedUsingRandomIO 20 -> (6,[DieOne,DieFive,DieSix,DieOne,DieFive,DieFive])
-- rollsCountLoggedUsingRandomIO 20 -> (7,[DieThree,DieOne,DieFive,DieThree,DieThree,DieOne,DieFive])
rollsCountLoggedUsingRandomIO :: Int -> IO CountLogger
rollsCountLoggedUsingRandomIO n = ((rollsCountLogged n) . mkStdGen) <$> randomIO

-----------------------------------------------------------------------------------

main = do

  putStrLn "\nrollDieThreeTimes"
  print $ rollDieThreeTimes

  putStrLn "\nrollDieThreeTimes'"
  print $ evalState rollDieThreeTimes' $ mkStdGen 0

  putStrLn "\nrollDieThreeTimes''"
  print $ evalState rollDieThreeTimes'' $ mkStdGen 0

  putStrLn "\nusing replicateM"
  print $ evalState (nDie 5) (mkStdGen 1)

  putStrLn "\nrollsToGetTwenty"
  print $ rollsToGetTwenty (mkStdGen 0)

  putStrLn "\nrollsToGetTwentyUsingRandomIO"
  rollsToGetTwentyValue <- rollsToGetTwentyUsingRandomIO
  print $ rollsToGetTwentyValue

  putStrLn $ "\nAdded few more practices"
  putStrLn $ "rollsToGetN 50"
  print $ rollsToGetN 50 $ mkStdGen 0

  putStrLn "\nrollsToGetNUsingRandomIO 50"
  rollsToGetNValue <- rollsToGetNUsingRandomIO 50
  print $ rollsToGetNValue

  putStrLn "\nrollsCountLoggedUsingRandomIO 50"
  rollsCountLoggedValue <- rollsCountLoggedUsingRandomIO 50
  print $ rollsCountLoggedValue
