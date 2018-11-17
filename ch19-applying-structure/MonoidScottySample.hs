{-# LANGUAGE OverloadedStrings #-}

module MonoidScottySample where

import Web.Scotty
import Data.Monoid (mconcat)

-----------------------------------------------------------------------------------
-- Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp (Official Repository)
-- https://github.com/scotty-web/scotty
-- Prelude> stack install scotty
--
-- e.g.
-- Prelude> main
-- http://localhost:3000
-- http://localhost:3000/test -> Scotty, test me up!
-----------------------------------------------------------------------------------

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]
