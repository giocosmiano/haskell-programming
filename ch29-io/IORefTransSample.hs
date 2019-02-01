module IORefTransSample where

import Control.Monad (replicateM)
import System.Random (randomRIO)

-----------------------------------------------------------------------------------

gimmeShelter :: Bool -> IO [Int]
gimmeShelter True  = replicateM 10 (randomRIO (0, 10))
gimmeShelter False = pure [0]

-----------------------------------------------------------------------------------

-- |
-- Prelude> gimmeShelter True
-- [3,9,9,6,10,7,4,3,8,8]
--
-- Prelude> gimmeShelter True
-- [7,5,5,2,2,8,6,4,0,8]
--
-- Prelude> gimmeShelter False
-- [0]

-- |
-- Referential transparency means that any function, when given the same inputs, returns
-- the same result. More precisely, an expression is referentially transparent when it can be
-- replaced with its value without changing the behavior of a program.
--
-- The mistake people make with IO is that they conflate the effects with the semantics of the
-- program. A function that returns `IO a` is still referentially transparent, because given the
-- same arguments, it’ll generate the same `IO` action every time.

-----------------------------------------------------------------------------------




