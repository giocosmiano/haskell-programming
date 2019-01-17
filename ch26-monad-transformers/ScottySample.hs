{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottySample where

import Web.Scotty
import Data.Monoid (mconcat)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

-----------------------------------------------------------------------------------

{-
newtype ScottyT e m a =
  ScottyT
  { runS
    :: State (ScottyState e m) a
  }
  deriving (Functor, Applicative, Monad)

newtype ActionT e m a =
  ActionT
  { runAM
    :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a
  }
  deriving ( Functor, Applicative )

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO
-}

-----------------------------------------------------------------------------------

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>" ]

-----------------------------------------------------------------------------------







