{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Aludrog (runApp) where

import Data.Text (Text)
import Lucid
  ( Attribute,
    Html,
    a_,
    alt_,
    b_,
    body_,
    charset_,
    content_,
    crossorigin_,
    doctype_,
    h1_,
    head_,
    href_,
    html_,
    img_,
    integrity_,
    lang_,
    li_,
    link_,
    meta_,
    name_,
    p_,
    rel_,
    script_,
    src_,
    title_,
    type_,
    ul_,
  )
import Lucid.Base (makeAttribute)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Capture,
    Get,
    PlainText,
    Proxy (Proxy),
    Raw,
    Server,
    serve,
    serveDirectoryWebApp,
    (:<|>) (..),
    (:>),
  )
import Servant.HTML.Lucid (HTML)

type AssetsAPI =
  "css" :> Raw
    :<|> "img" :> Raw
    :<|> "scripts" :> Raw

type API =
  "ping" :> Get '[PlainText] Text
    :<|> Get '[HTML] (Html ())
    :<|> "swap" :> Capture "name" Text :> Get '[HTML] (Html ())
    :<|> AssetsAPI

server :: Server API
server = handlePing :<|> handleHome :<|> handleSwap :<|> assetsServer
  where
    handlePing = pure "pong"
    handleHome = pure $ do
      doctype_
      html_ [lang_ "en-US"] $ do
        head_ $ do
          title_ "aludrog"
          link_ [rel_ "shorcut icon", type_ "image/jpg", href_ "img/icon.jpeg"]
          meta_ [charset_ "utf-8"]
          meta_
            [ name_ "viewport",
              content_ "width=device-width"
            ]
          link_ [href_ "css/style.css", rel_ "stylesheet"]
          link_ [href_ "https://fonts.googleapis.com/css2?family=Roboto+Flex:opsz,wght@8..144,100&display=swap", rel_ "stylesheet"]
          script_
            [ src_ "https://unpkg.com/htmx.org@1.8.5",
              integrity_ "sha384-7aHh9lqPYGYZ7sTHvzP1t3BAfLhYSTy9ArHdP3Xsr9/3TlGurYgcPBoFmXX2TX/w",
              crossorigin_ "anonymous"
            ]
            ("" :: Html ())
        body_ $ do
          h1_ "Giving Back is Fashionable"
          img_
            [ src_ "img/rambutan.jpg",
              alt_ "Yummy!",
              hxTrigger_ "click",
              hxSwap_ "outerHTML",
              hxGet_ "swap/rambutan.jpg"
            ]
          p_ "Every purchase or sale is a contribution to good cause."
          p_ "Ways to contribute:"
          ul_ $ do
            li_ (b_ "Purchase -" <> " a portion of each sale goes to a charity of your choice.")
            li_ (b_ "Sell -" <> " your unused items will serve a purpose rather than just taking up space.")
            li_ (b_ "Donate -" <> " 100% of proceeds from donations will go to the charity of your choice.")
    handleSwap name = pure $ do
      img_
        [ src_ $ "img/" <> name',
          alt_ "Yummy!",
          hxTrigger_ "click",
          hxSwap_ "outerHTML",
          hxGet_ $ "swap/" <> name'
        ]
      where
        name' = if name == "rambutan.jpg" then "bam.png" else "rambutan.jpg"

hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"

hxGet_ :: Text -> Attribute
hxGet_ = makeAttribute "hx-get"

assetsServer :: Server AssetsAPI
assetsServer =
  serveDirectoryWebApp "assets/css"
    :<|> serveDirectoryWebApp "assets/img"
    :<|> serveDirectoryWebApp "assets/scripts"

app :: Application
app = serve (Proxy @API) server

runApp :: IO ()
runApp = run 8081 app
