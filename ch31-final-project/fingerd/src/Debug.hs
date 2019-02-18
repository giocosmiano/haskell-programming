module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

-----------------------------------------------------------------------------------

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  (soc, _) <- accept sock
  printAndKickback soc
  sClose soc
  where printAndKickback conn = do
          msg <- recv conn 1024
          print msg
          sendAll conn msg

-----------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo
                 (Just (defaultHints
                   {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just "79")
  let serverAddr = head addrinfos
  sock <- socket (addrFamily serverAddr)
            Stream defaultProtocol
  bindSocket sock (addrAddress serverAddr)
  listen sock 1
  logAndEcho sock
  sClose sock

-----------------------------------------------------------------------------------

-- |
--
-- $ cd fingerd
--
-- $ sudo `stack exec which debug`
--
-- $ telnet localhost 79
-- Hello World
-- Hello World
--
-- $ telnet localhost 79
-- Haskell is a very cool FP language
-- Haskell is a very cool FP language

-----------------------------------------------------------------------------------