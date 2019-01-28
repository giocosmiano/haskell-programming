module RefutableIrrefutable where

-----------------------------------------------------------------------------------

refutable :: Bool -> Bool
refutable True  = False
refutable False = True

irrefutable :: Bool -> Bool
irrefutable x = not x

oneOfEach :: Bool -> Bool
oneOfEach True = False
oneOfEach _    = True

-- |
-- An irrefutable pattern is one which will never fail to match. A refutable pattern is one
-- which has potential failures. Often, the problem is one of specificity.
--
-- Remember, the pattern is refutable or not, not the function itself. The function refutable
-- is refutable because each case is refutable; each case could be given an input that fails to match.
-- In contrast, irrefutable has an irrefutable pattern; that is, its pattern doesn’t rely on matching
-- with a specific value.
--
-- In the case of oneOfEach, the first pattern is refutable because it pattern matches on the True
-- data constructor. irrefutable and the second match of oneOfEach are irrefutable because they
-- don’t need to look inside the data they are applied to. That said, the second pattern match of
-- oneOfEach being irrefutable isn’t terribly semantically meaningful as Haskell will have to
-- inspect the data to see if it matches the first case anyway. The irrefutable function works for
-- any inhabitant (all two of them) of Bool because it doesn’t specify which Bool value in the
-- pattern to match. You could think of an irrefutable pattern as one which will never fail to
-- match. If an irrefutable pattern for a particular value comes before a refutable pattern, the
-- refutable pattern will never get invoked.

-----------------------------------------------------------------------------------

-- |
-- Lazy patterns are also irrefutable
-- The tilde is how one makes a pattern match lazy. A caveat is that since it makes the pattern
-- irrefutable, you can’t use it to discriminate cases of a sum — it’s useful for unpacking
-- products that might not get used.
--
-- e.g.
-- Prelude> strictPattern undefined
-- *** Exception: Prelude.undefined
--
-- Prelude> lazyPattern undefined
-- "Cousin It"
--
-- In the lazy pattern version since `const` didn’t actually need `a` from the tuple, we never forced the
-- bottom. The default behavior is to just go ahead and force it before evaluating the function body,
-- mostly for more predictable memory usage and performance.

strictPattern :: (a, b) -> String
strictPattern (a,b) = const "Cousin It" a

lazyPattern :: (a, b) -> String
lazyPattern ~(a,b) = const "Cousin It" a

-----------------------------------------------------------------------------------

