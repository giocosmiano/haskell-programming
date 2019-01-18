{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottySampleLiftIO where

import Web.Scotty

import Control.Monad.IO.Class
import Data.Monoid (mconcat)

-----------------------------------------------------------------------------------
-- |
-- e.g.
-- curl -X GET http://localhost:3000/beam/

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    liftIO $ putStrLn "hello"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------



