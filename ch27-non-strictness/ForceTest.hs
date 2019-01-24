module ForceTest where

-----------------------------------------------------------------------------------

-- |
-- Using GHC Core Dump
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType#Caseexpressions
--
-- Prelude> :set -dsuppress-all
-- Prelude> :l ForceTest

-----------------------------------------------------------------------------------

data Test = A Test2
          | B Test2
          deriving (Show)

data Test2 = C Int
           | D Int
           deriving (Show)

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- Prelude> forceNothing undefined
-- 0
--
-- Prelude> forceNothing (A undefined)
-- 0

forceNothing :: Test -> Int
forceNothing _ = 0

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- Prelude> forceTest (A undefined)
-- 1
--
-- Prelude> forceTest (B undefined)
-- 2
--
-- Prelude> forceTest undefined
-- *** Exception: Prelude.undefined

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- Prelude> forceTest2 (A (C 0))
-- 0
--
-- Prelude> forceTest2 (A (C undefined))
-- *** Exception: Prelude.undefined
--
-- Prelude> forceTest2 (A undefined)
-- *** Exception: Prelude.undefined
--
-- Prelude> forceTest2 undefined
-- *** Exception: Prelude.undefined

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (B (C i)) = i
forceTest2 (A (D i)) = i
forceTest2 (B (D i)) = i

-----------------------------------------------------------------------------------



