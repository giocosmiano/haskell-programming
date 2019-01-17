{-# LANGUAGE InstanceSigs #-}

module MyStateExercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- import Data.Monoid
-- (runMoi $ Moi (\s -> (1, s))) 7              -> (1,7)
-- (runMoi $ Moi (\s -> (1, s))) $ [7]          -> (1,[7])
-- (runMoi $ Moi (\s -> (1, s))) $ Just 7       -> (1,Just 7)
-- (runMoi $ Moi (\s -> (1, s))) $ Identity 7   -> (1,Identity 7)

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-----------------------------------------------------------------------------------

-- |
-- 1) Apply the prior computation/function `g` to argument `s` to get a new state `(a, s')`
-- 2) Then, apply the current computation/function `f` to value `a`, resulting to `(b, s')`,
--    which will become the argument to `Moi $`

-- |
-- e.g.
-- import Data.Monoid
-- runMoi ((+3) <$> (Moi $ \s -> (5, s))) 7            -> (8,7)
-- runMoi ((*3) <$> (Moi $ \s -> (5, s))) 7            -> (15,7)
-- runMoi ((+3) <$> (Moi $ \s -> (5, s))) $ Nothing    -> (8,Nothing)
-- runMoi ((+3) <$> (Moi $ \s -> (5, s))) $ Just 7     -> (8,Just 7)
-- runMoi ((*3) <$> (Moi $ \s -> (5, s))) $ Identity 7 -> (15,Identity 7)
instance Functor (Moi s) where

  fmap :: (a -> b) -> Moi s a -> Moi s  b
  fmap f (Moi g) = Moi $ \s ->
    let (a, s') = g s
    in  (f a, s')

-----------------------------------------------------------------------------------

-- |
-- 1) Apply the prior computation/function `f` to argument `s` to get a new computation and state `(f', s')`
-- 2) Then, apply the current computation/function `g` to state `s'` from prior computation, resulting to new state `(a', s'')`
-- 3) Finally, apply the new computation/function `f'` to value `a'`, resulting to `(b', s'')`,
--    which will become the argument to `Moi $`

-- |
-- e.g.
-- import Data.Monoid
-- runMoi ((Moi $ \s -> ((+3), s)) <*> (Moi $ \s -> (5, s))) 7            -> (8,7)
-- runMoi ((*) <$> (Moi $ \s -> (3, s)) <*> (Moi $ \s -> (5, s))) 7       -> (15,7)
-- runMoi ((Moi $ \s -> ((+3), s)) <*> (Moi $ \s -> (5, s))) $ Nothing    -> (8,Nothing)
-- runMoi ((Moi $ \s -> ((+3), s)) <*> (Moi $ \s -> (5, s))) $ Just 7     -> (8,Just 7)
-- runMoi ((Moi $ \s -> ((*3), s)) <*> (Moi $ \s -> (5, s))) $ Identity 7 -> (15,Identity 7)
instance Applicative (Moi s) where

  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (f', s')  = f s
        (a', s'') = g s'
    in  (f' a', s'')

-----------------------------------------------------------------------------------

-- |
-- 1) Apply prior computation/function `f` to argument `s` to get a new state `(a, newState)`
-- 2) Then, apply the current computation/function `g` to value `a`, resulting to `Moi s b` -- see (>>=) signature
-- 3) Finally, unpack/extract function `s -> (b, s)` from `Moi s b` and apply to `newState`, resulting to `(b, newState)`,
--    which will become the argument to `Moi $`

-- |
-- e.g.
-- runMoi ((Moi $ \s -> (5, s)) >>= return . (+3)) 7            -> (8,7)
-- runMoi ((Moi $ \s -> (5, s)) >>= return . (*3)) 7            -> (15,7)
-- runMoi ((Moi $ \s -> (5, s)) >>= return . (+3)) $ Nothing    -> (8,Nothing)
-- runMoi ((Moi $ \s -> (5, s)) >>= return . (+3)) $ Just 7     -> (8,Just 7)
-- runMoi ((Moi $ \s -> (5, s)) >>= return . (*3)) $ Identity 7 -> (15,Identity 7)
instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a, newState) = f s
    in  (runMoi (g a)) newState

-- |
-- OR
--  (Moi f) >>= g = Moi $ \s ->
--    let (a, newState) = f s -- apply first/previous stateful computation `f` to `s` creating tuple with `newState`
--        Moi h = g a -- apply function `g` to output value `a ` creating new Moi stateful computation/function `h`
--    in  h newState -- apply the new Moi stateful computation/function `h` to `newState` resulting to new tuple with newState

-----------------------------------------------------------------------------------
--
-- |
-- for further reading
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/12-State-Monad
-- http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
-- http://dev.stephendiehl.com/hask/

-- |
-- `get` is when we want to construct a `State`, feeding some `s` to State returning `(s, s)`
-- e.g.
-- runMoi get "curryIsAmaze" -> ("curryIsAmaze","curryIsAmaze")
get :: Moi s s
get = Moi $ \s -> (s, s)

-- |
-- `put` is when we want to construct a `State`, feeding some `s` to State returning `((), s)`
-- e.g.
-- runMoi (put "blah") "woot" -> ((),"blah")
put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

-- |
-- `exec` is when we want the `state` value and not the `a` value
-- e.g.
-- exec (put "wilma") "daphne" -> "wilma"
-- exec get "scooby papu"      -> "scooby papu"
exec :: Moi s a -> s -> s
exec sa s = snd $ runMoi sa s

-- |
-- `eval` is when we want the `a` value and not the `state` value
-- e.g.
-- eval get "bunnicula"     -> "bunnicula"
-- eval get "stake a bunny" -> "stake a bunny"
eval :: Moi s a -> s -> a
eval sa s = fst $ runMoi sa s

-- |
-- `modify` is when we want to construct a `State` with function to modifying the state,
-- feeding some `s` to State returning `((), fn s)`
-- e.g.
-- runMoi (modify (+1)) 0                -> ((),1)
-- runMoi (modify (+1) >> modify (+1)) 0 -> ((),2)
-- runMoi (modify (+3) >> modify (*5)) 0 -> ((),15)
-- runMoi (modify (*3) >> modify (+5)) 0 -> ((),5)
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)

-----------------------------------------------------------------------------------

-- TODO: how-to implement quickBatch for State???
{-
instance (Arbitrary s, CoArbitrary s,
          Arbitrary a, CoArbitrary a) => Arbitrary (State s a) where
  arbitrary = do
    s <- arbitrary
    a <- arbitrary
    return $ State $ \s -> (a, s)

instance (Eq s, Eq a) => EqProp (State s a) where (=-=) = eq
-}

-----------------------------------------------------------------------------------

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

-- https://hackage.haskell.org/package/checkers
-- https://hackage.haskell.org/package/checkers-0.4.11/docs/Test-QuickCheck-Classes.html

-- https://github.com/conal/checkers
-- https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs

--main = do
--
--  putStrLn "\nTesting Applicative, Monad : State"
--  quickBatch $ functor (undefined :: State (Int, Double, Char))
--  quickBatch $ applicative (undefined :: State (Int, Double, Char))
--  quickBatch $ monad (undefined :: State (Int, Double, Char))
