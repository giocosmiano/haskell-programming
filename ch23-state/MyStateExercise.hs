module MyStateExercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-- e.g.
-- runMoi ((+3) <$> (Moi $ \s -> (5, s))) 7 -> (8,7)
-- runMoi ((*3) <$> (Moi $ \s -> (5, s))) 7 -> (15,7)
instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s ->
    let (a, s') = g s
    in  (f a, s')

-- e.g.
-- runMoi ((+) <$> (Moi $ \s -> (3, s)) <*> (Moi $ \s -> (5, s))) 7 -> (8,7)
-- runMoi ((*) <$> (Moi $ \s -> (3, s)) <*> (Moi $ \s -> (5, s))) 7 -> (15,7)
instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)

  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (f', s')  = f s
        (a', s'') = g s'
    in  (f' a', s'')

-- e.g.
-- runMoi ((Moi $ \s -> (5, s)) >>= return . (+3)) 7 -> (8,7)
-- runMoi ((Moi $ \s -> (5, s)) >>= return . (*3)) 7 -> (15,7)
instance Monad (Moi s) where
  return = pure

  (Moi f) >>= g = Moi $ \s ->
    let (a, newState) = f s -- apply first/previous stateful computation `f` to `s` creating tuple with `newState`
    in  runMoi (g a) newState -- apply function `g` to output value `a ` creating new Moi stateful, feeding `newState`

-- OR
--  (Moi f) >>= g = Moi $ \s ->
--    let (a, newState) = f s -- apply first/previous stateful computation `f` to `s` creating tuple with `newState`
--        Moi h = g a -- apply function `g` to output value `a ` creating new Moi stateful computation/function `h`
--    in  h newState -- apply the new Moi stateful computation/function `h` to `newState` resulting to new tuple with newState

-----------------------------------------------------------------------------------
--
-- for further reading
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/12-State-Monad
-- http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
-- http://dev.stephendiehl.com/hask/

-- `get` is when we want to construct a `State`, feeding some `s` to State returning `(s, s)`
-- e.g.
-- runMoi get "curryIsAmaze" -> ("curryIsAmaze","curryIsAmaze")
get :: Moi s s
get = Moi $ \s -> (s, s)

-- `put` is when we want to construct a `State`, feeding some `s` to State returning `((), s)`
-- e.g.
-- runMoi (put "blah") "woot" -> ((),"blah")
put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

-- `exec` is when we want the `state` value and not the `a` value
-- e.g.
-- exec (put "wilma") "daphne" -> "wilma"
-- exec get "scooby papu"      -> "scooby papu"
exec :: Moi s a -> s -> s
exec sa s = snd $ runMoi sa s

-- `eval` is when we want the `a` value and not the `state` value
-- e.g.
-- eval get "bunnicula"     -> "bunnicula"
-- eval get "stake a bunny" -> "stake a bunny"
eval :: Moi s a -> s -> a
eval sa s = fst $ runMoi sa s

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
