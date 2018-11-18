{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

-----------------------------------------------------------------------------------

-- Redis is an in-memory data structure store, used as a database, cache and message broker
-- https://redis.io/
-- Prelude> stack install redis
--
-- https://redis.io/topics/quickstart
-- https://www.digitalocean.com/community/tutorials/how-to-install-and-secure-redis-on-ubuntu-18-04
--
-- sudo systemctl restart redis.service
-- sudo systemctl status redis
-- sudo systemctl disable redis
-- sudo systemctl restart redis
--
-- https://stackoverflow.com/questions/6910378/how-can-i-stop-redis-server
-- /etc/init.d/redis-server start
-- /etc/init.d/redis-server stop
-- /etc/init.d/redis-server restart
--
-- e.g.
-- redis-cli
-- set blah "this is an in-memory test for http://localhost:3000/blah and it worked"
-- get blah
--
-- e.g.
-- Prelude> main
-- http://localhost:3000
-- http://localhost:3000/blah -> should see the `blah` key-value setting above from redis

-----------------------------------------------------------------------------------

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

-----------------------------------------------------------------------------------

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

-----------------------------------------------------------------------------------

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)

-----------------------------------------------------------------------------------

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

-----------------------------------------------------------------------------------

getURI  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

-----------------------------------------------------------------------------------

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

-----------------------------------------------------------------------------------

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

-----------------------------------------------------------------------------------

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

-----------------------------------------------------------------------------------

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

-----------------------------------------------------------------------------------

app :: R.Connection
    -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _  -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        resp <- liftIO (saveURI rConn shorty uri')
        html (shortyCreated resp shawty)
      Nothing -> text (shortyAintUri uri)
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)
          where tbs :: TL.Text
                tbs = TL.fromStrict (decodeUtf8 bs)

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)

-----------------------------------------------------------------------------------
