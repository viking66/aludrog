{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Aludrog.Server
  ( runApp
  ) where

import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import Servant.API

type AludrogAPI = "ping" :> Get '[PlainText] Text

aludrogAPI :: Proxy AludrogAPI
aludrogAPI = Proxy

server :: Server AludrogAPI
server = ping
  where
    ping = return "pong"

app :: Application
app = serve aludrogAPI server

runApp :: IO ()
runApp = runTLS tlsOpts warpOpts app
  where
    tlsOpts = tlsSettings "localhost.crt" "localhost.key"
    warpOpts = setPort 8080 defaultSettings
