{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottySampleLiftIO2 where

import Web.Scotty

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)

-----------------------------------------------------------------------------------
-- |
-- e.g.
-- curl -X GET http://localhost:3000/beam/         -> Nothing
-- curl -X GET http://localhost:3000/beam?num=123  -> Just 123
--
-- Prelude> :t param
-- param :: Parsable a => Data.Text.Internal.Lazy.Text -> ActionM a
--
-- Prelude> :t rescue
-- rescue
--   :: ActionM a
--      -> (Data.Text.Internal.Lazy.Text -> ActionM a) -> ActionM a

param' :: Parsable a => Text -> ActionM (Maybe a)
param' k = rescue (Just <$> param k)
  (const (return Nothing))

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam' <- param' "word"
    let beam = fromMaybe "" beam'
    i <- param' "num"
    liftIO $ print (i :: Maybe Integer)
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------



