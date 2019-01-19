{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottyLiftIOMaybeT where

import Web.Scotty

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)

-----------------------------------------------------------------------------------
-- |
-- e.g.
-- curl -X GET http://localhost:3000/beam/?1=1             -> Nothing
-- curl -X GET http://localhost:3000/beam?1=1&2=2&3=3&4=4  -> Just (1,2,3,4)
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

type Reco = (Integer, Integer, Integer, Integer)

param' :: Parsable a => Text -> MaybeT ActionM a
param' k = MaybeT $
  rescue (Just <$> param k)
         (const (return Nothing))

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
-- intentional `issue` and one of the tricks from the authors
-- to see if I'm paying attention to details
--    beam' <- param "word"
    beam' <- runMaybeT $ param' "word"
    let beam = fromMaybe "" beam'
    reco <- runMaybeT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    liftIO $ print reco
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------
