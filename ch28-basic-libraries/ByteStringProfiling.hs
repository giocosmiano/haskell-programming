{-# LANGUAGE OverloadedStrings #-}

module ByteStringProfiling where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

-- |
-- https://hackage.haskell.org/package/zlib
import qualified Codec.Compression.GZip as GZip

-----------------------------------------------------------------------------------

input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZip.compress input

main :: IO ()
main = do
  TIO.putStrLn $ TE.decodeUtf8 (s input)
  TIO.putStrLn $ TE.decodeUtf8 (s compressed)
  where s = BL.toStrict

-- |
-- The encoding module in the text library expects strict ByteStrings, so we have
-- to make them strict before attempting a decoding. The second text decode will
-- fail because there’ll be a byte that isn’t recognizably correct as an encoding
-- of text information.
--
--
-- Prelude> main
-- 123
-- *** Exception: Cannot decode byte '\x8b': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream


-----------------------------------------------------------------------------------
