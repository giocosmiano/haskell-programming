{-# LANGUAGE BangPatterns #-}

module ManualBang where

-----------------------------------------------------------------------------------

-- |
-- Sometimes we want to evaluate an argument to a function whether we use it or not.
-- We can do this with `seq` Or we can also do it with a bang pattern
--
-- e.g.
-- Prelude> doesntEval False
-- 1
--
-- Prelude> manualSeq False
-- 1
--
-- Prelude> banging False
-- 1

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

banging :: Bool -> Int
banging !b = 1

-----------------------------------------------------------------------------------

data DoesntForce = TisLazy Int String

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s

-----------------------------------------------------------------------------------

-- |
-- note the exclamation marks again

data BangBang = SheShotMeDown !Int !String

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s

-- |
-- e.g.
--
-- Prelude> let x = TisLazy undefined "blah"
-- Prelude> gibString x
-- "blah"
--
-- Prelude> let s = SheShotMeDown
-- Prelude> let x = s undefined "blah"
-- Prelude> gimmeString x
-- *** Exception: Prelude.undefined
--
-- The idea here is that in some cases, it’s cheaper to just compute something than to construct a thunk and then evaluate
-- it later. This case is particularly common in numerics code where you have a lot of Int and Double values running around
-- which are individually cheap to conjure. If the values are both cheap to compute and small, then you may as well make them
-- strict unless you’re trying to dance around bottoms. Types with underlying primitive representations Int and Double most
-- assuredly qualify as small.
--
-- A good rule to follow is lazy in the spine, strict in the leaves! Sometimes a “leak” isn’t really a leak but temporarily
-- excessive memory that subsides because you made 1,000,000 tiny values into less-tiny thunks when you could’ve just computed
-- them as your algorithm progressed.

-----------------------------------------------------------------------------------
