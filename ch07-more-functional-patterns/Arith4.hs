module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

-- e.g.
-- roundTrip "abc" :: String
-- roundTrip 123 :: Int
