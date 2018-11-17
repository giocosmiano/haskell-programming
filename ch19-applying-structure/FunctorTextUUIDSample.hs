module FunctorTextUUIDSample where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

-----------------------------------------------------------------------------------

-- An efficient packed, immutable Unicode text type for Haskell
-- https://github.com/haskell/text
-- Prelude> stack install text

-----------------------------------------------------------------------------------

-- A Haskell library for creating, printing and parsing UUIDs
-- https://github.com/haskell-hvr/uuid
-- Prelude> stack install uuid

-----------------------------------------------------------------------------------

textUuid :: IO Text
textUuid = fmap (T.pack . UUID.toString) UUIDv4.nextRandom

-- Another example of Lifting over IO
--
-- e.g.
-- textUuid -> "b0c0716b-4a44-4b23-889d-30fdd6c9ac46"
--
-- 1. nextRandom :: IO UUID
-- 2. toString :: UUID -> String
-- 3. pack :: String -> Text
-- 4. fmap :: (UUID -> Text) -> IO UUID -> IO Text

