-- We’re going to provide an initial scaffold of a scotty application that counts hits to specific URIs. It also prefixes the keys with a prefix defined on app initialization, retrieved via command line arguments:

{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    -- that's one, one click!
    -- two... two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

-- Stuff inside ScottyT is, except for things that escape via IO, effectively read-only, so we can’t use StateT. It would overcomplicate things to attempt to do so, and you should be using a proper database for production applications:

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let (mv, m') =
        M.insertLookupWithKey (const (+)) k 1 m
  in (m', (+1) $ fromMaybe 0 mv)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    conf <- lift ask
    let key' = mappend (prefix conf) unprefixed
    newInteger <- lift . lift $
                    atomicModifyIORef
                      (counts conf)
                      (bumpBoomp key')

    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR (ReaderT rmr) = rmr config
  scottyT 3000 runR app

hello :: IO ()
hello = do
  putStrLn "hello"
  putStrLn "goodbye"
