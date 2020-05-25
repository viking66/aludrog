{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Aludrog.Server
  ( runApp
  ) where

import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
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
runApp = run 8080 app
