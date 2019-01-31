{-# LANGUAGE OverloadedStrings #-}

module Char8Profiling where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- |
-- utf8-string
import qualified Data.ByteString.UTF8 as UTF8

-----------------------------------------------------------------------------------

-- |
-- Manual unicode encoding of Japanese text
-- GHC Haskell allows UTF8 in source files

s :: String
s = "\12371\12435\12395\12385\12399\12289\
\20803\27671\12391\12377\12363\65311"

utf8ThenPrint :: B.ByteString -> IO ()
utf8ThenPrint = putStrLn . T.unpack . TE.decodeUtf8

throwsException :: IO ()
throwsException = utf8ThenPrint (B8.pack s)

bytesByWayOfText :: B.ByteString
bytesByWayOfText = TE.encodeUtf8 (T.pack s)

-- |
-- letting utf8-string do it for us
libraryDoesTheWork :: B.ByteString
libraryDoesTheWork = UTF8.fromString s

thisWorks :: IO ()
thisWorks = utf8ThenPrint bytesByWayOfText

alsoWorks :: IO ()
alsoWorks = utf8ThenPrint libraryDoesTheWork

-- |
-- The Char8 module is really a convenience for data that mingles bytes and ASCII data 18 there. It doesn’t work for
-- Unicode and shouldn’t be used anywhere there’s even a hint of possibility that there could be Unicode data.
--
-- Prelude> thisWorks
-- こんにちは、20803気ですか？
--
-- Prelude> alsoWorks
-- こんにちは、20803気ですか？
--
-- Prelude> libraryDoesTheWork
-- "\227\129\147\227\130\147\227\129\171\227\129\161\227\129\175\227\128\129\&20803\230\176\151\227\129\167\227\129\153\227\129\139\239\188\159"
--
-- Prelude> throwsException
-- *** Exception: Cannot decode byte '\x93': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream

-- |
-- When to use ByteString instead of Text for textual data?
-- This does happen sometimes, usually because we want to keep data that arrived in a UTF-8 encoding in UTF-8. Often this
-- happens because we read UTF-8 data from a file or network socket and we don’t want the overhead of bouncing it into
-- and back out of Text. If we do this, we might want to use newtypes to avoid accidentally mixing this data with non-UTF-8 bytestrings.

-----------------------------------------------------------------------------------
