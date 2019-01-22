module OutsideInInsideOut where

-----------------------------------------------------------------------------------

-- |
-- (\f -> f fst snd (0, undefined))
-- (\a -> (\b -> a))
-- (\a -> (\b -> a)) fst snd (0, undefined)
-- (\b -> fst) snd (0, undefined)
-- fst (0, undefined)
-- 0
--
-- e.g.
-- Prelude> possiblyKaboom true
-- 0
--
-- e.g.
-- Prelude> possiblyKaboom const
-- 0
--
-- Prelude> possiblyKaboom false
-- *** Exception: Prelude.undefined

possiblyKaboom
  :: (((a1, b1) -> a1) ->
        ((a, b) -> b)  ->
        (Integer, t)   -> t1) -> t1
possiblyKaboom = \f -> f fst snd (0, undefined)

-----------------------------------------------------------------------------------

-- booleans in lambda form
true :: a -> a -> a
true = \a -> (\b -> a)

-- |
-- https://en.wikibooks.org/wiki/Haskell/Higher-order_functions
-- https://www.quora.com/What-does-this-Haskell-code-mean-code-haskell-const-a-b-a-const-x-_-x-code
--
-- OR
-- Prelude> :t const
-- const :: a -> b -> a

-----------------------------------------------------------------------------------

false :: a -> a -> a
false = \a -> (\b -> b)

-- |
-- OR
-- Prelude> :t const id
-- const id :: b -> a -> a
--
-- OR
-- Prelude> :t flip const
-- flip const :: b -> c -> c

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- Prelude> possiblyKaboom' True
-- 0
--
-- Prelude> possiblyKaboom' False
-- *** Exception: Prelude.undefined

possiblyKaboom' :: Num a => Bool -> a
possiblyKaboom' b =
  case b of
    True  -> fst tup
    False -> snd tup
  where tup = (0, undefined)

-----------------------------------------------------------------------------------

-- |
-- When we say evaluation works outside in, we’re talking about evaluating a series of
-- nested expressions, and not only are we starting from the outside and working in,
-- but we’re also only evaluating some of the expressions some of the time. In Haskell,
-- we evaluate expressions when we need them rather than when they are first referred to
-- or constructed. This is one of the ways in which non-strictness makes Haskell expressive
-- we can refer to values before we’ve done the work to create them.

foldr' :: (t1 -> t -> t) -> t -> [t1] -> t
foldr' k z xs = go xs
  where
    go [] = z
    go (y:ys) = y `k` go ys

-- |
-- e.g.
-- Prelude> foldr' const 'z' ['a'..'e']
-- 'a'
--
-- Prelude> foldr' const 'z' ['a', undefined]
-- 'a'

-----------------------------------------------------------------------------------

-- |
-- A strict language cannot evaluate `hypo` successfully unless the `x` isn’t bottom.
-- This is because strict languages will force the bottom before binding `x`. A strict
-- language is evaluating each binding as it comes into scope, not when a binding is used.
--
-- The idea is that evaluation is driven by demand, not by construction. We don’t get the
-- exception unless we’re forcing evaluation of `x` — outside in.

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"

-----------------------------------------------------------------------------------

-- |
-- `seq` function forces evaluation of the first argument if and when the second argument
-- has to be evaluated. Adding `seq` means that anytime `s` is evaluated, `x` must also be
-- evaluated.
--
-- Prelude> :t seq
-- seq :: a -> b -> b
--
-- Essentially, seq is
-- seq bottom b                     = bottom
-- seq literallyAnythingNotBottom b = b

hypo' :: IO ()
hypo' = do
  let x :: Int
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _ -> putStrLn "hello"


-----------------------------------------------------------------------------------
-- |
-- *** weak head normal form ***
--
-- `dc` has a data constructor, `seq` doesn’t need to care about the values inside
-- that constructor; weak head normal form evaluation only requires it to evaluate
-- the constructor. On the other hand, `noDc` has no data constructor or lambda
-- outside the value, so there’s no head for the evaluation to stop at. Finally,
-- `lam` has a lambda outside the expression which has the same effect on evaluation
-- as a data constructor does.
--
-- Prelude> let dc = (,) undefined undefined
-- Prelude> let noDc = undefined
-- Prelude> let lam = \_ -> undefined
-- Prelude> dc `seq` 1
-- 1
-- Prelude> noDc `seq` 1
-- *** Exception: Prelude.undefined
-- Prelude> lam `seq` 1
-- 1

-----------------------------------------------------------------------------------



