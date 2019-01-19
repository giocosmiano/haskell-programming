{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottyLiftIO where

import Web.Scotty

import Control.Monad.IO.Class
import Data.Monoid (mconcat)

-----------------------------------------------------------------------------------
-- |
-- e.g.
-- curl -X GET http://localhost:3000/beam/
--
-- Prelude> :t get
-- get :: RoutePattern -> ActionM () -> ScottyM ()
--
-- Prelude> :t param
-- param :: Parsable a => Data.Text.Internal.Lazy.Text -> ActionM a
--
-- Prelude> :t rescue
-- rescue
--   :: ActionM a
--      -> (Data.Text.Internal.Lazy.Text -> ActionM a) -> ActionM a

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    liftIO $ putStrLn "hello"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------



