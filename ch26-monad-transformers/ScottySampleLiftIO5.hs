{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottySampleLiftIO5 where

import Web.Scotty

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as TL

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

param' :: Parsable a => TL.Text -> ExceptT String ActionM a
param' k =
  ExceptT $
  rescue (Right <$> param k)
           (const
             (return
               (Left $ "The key: " ++ show k ++ " was missing!")))

tshow = TL.pack . show

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    reco <- runExceptT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)

    case reco of
      (Left e) -> text (TL.pack e)
      (Right r) ->
        html $ mconcat
          [ "<h1>Success! Reco was: "
          , tshow r
          , "</h1>" ]

-----------------------------------------------------------------------------------
