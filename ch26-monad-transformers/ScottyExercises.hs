{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module ScottyExercises where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

-----------------------------------------------------------------------------------

-- |
-- Use `hoogle` to search for function APIs
--
-- Prelude> :i getArgs
-- getArgs :: IO [String]  -- Defined in ‘System.Environment’
--
-- Prelude> :i newIORef
-- newIORef :: a -> IO (IORef a)   -- Defined in ‘GHC.IORef’
--
-- Prelude> :i scotty
-- scotty ::
--   warp-3.2.22:Network.Wai.Handler.Warp.Types.Port
--   -> ScottyM () -> IO ()
--         -- Defined in ‘Web.Scotty’

-----------------------------------------------------------------------------------

data Config =
  Config {
-- that's one, one click!
-- two...two clicks!
-- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a }

type Scotty  = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

-----------------------------------------------------------------------------------

-- |
-- Data.Map samples
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#data-map

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = do
  m' <-
    case M.member k m of
      True  -> return $ M.adjust (+1) k m
      False -> return $ M.insert k 1 m
  let count = M.findWithDefault 0 k m'
  (m', count)

-----------------------------------------------------------------------------------

-- |
-- Data.IORef samples
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#data-ioref

app :: Scotty ()
app =
  get "/:key" $ do
    unPrefixed <- param "key"
    config <- lift ask -- retrieves the monad value from the environment
    let key' = mappend (prefix config) unPrefixed
        map' = readIORef . counts $ config
    (newMap, newInteger) <- liftIO (bumpBoomp key' <$> map')
    liftIO $ writeIORef (counts config) newMap
    html $ mconcat
      [ "<h1>Success! Count was: "
      , TL.pack $ show newInteger
      , "</h1>"
      ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> :t scottyT
-- scottyT
--   :: (Monad m, Control.Monad.IO.Class.MonadIO n) =>
--          Port
--      -> (m Response -> IO Response)
--      -> ScottyT e m ()
--      -> n ()
--
-- referencing scotty's runReaderT examples from
-- https://github.com/scotty-web/scotty/blob/master/examples/reader.hs
-- https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs
--
-- scottyT arguments
-- port                        -- 3000
-- (m Response -> IO Response) -- runR r  *OR*  ReaderT r m a -> r -> m a
-- ScottT e m ()               -- ScottyT Text (ReaderT Config IO)
-- n ()                        -- IO ()

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- Prelude> :main lol
-- Setting phasers to stun... (port 3000) (ctrl-c to quit)
--
-- $ curl localhost:3000/woot
-- <h1>Success! Count was: 1</h1>
-- $ curl localhost:3000/woot
-- <h1>Success! Count was: 2</h1>
-- $ curl localhost:3000/blah
-- <h1>Success! Count was: 1</h1>
--
-- *** NOTE ***
-- The underlying “key” used in the counter when we GET /woot is "lolwoot"
-- because we passed ”lol” to main.

-----------------------------------------------------------------------------------

