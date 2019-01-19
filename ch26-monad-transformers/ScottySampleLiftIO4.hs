{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottySampleLiftIO4 where

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

type Reco = (Integer, Integer, Integer, Integer)

param' :: Parsable a => Text -> ActionM (Either String a)
param' k =
  rescue (Right <$> param k)
           (const
             (return
               (Left $ "The key: " ++ show k ++ " was missing!")))

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    a <- param' "1"
    let a' = either (const 0) id a
    liftIO $ print (a :: Either String Int)
    liftIO $ print (a' :: Int)
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------
