{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fcontext-stack=1000 #-}
module Main where

import           Data.ByteString.Lazy     (ByteString, fromStrict)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Proxy
import qualified Network.HTTP.Media       as M
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Server
import           System.Environment       (getArgs)
import           Control.Concurrent       (runInUnboundThread)


main :: IO ()
main = do
    port:_ <- getArgs
    runInUnboundThread $ run (read port) $ serve api server

api :: Proxy Api
api = Proxy

server :: Server Api
server = serverHello :<|> serverCapture :<|> serverDeep :<|> serverAfter

data BS

instance Accept BS where
   contentType _ = "text" M.// "plain" M./: ("charset", "utf-8")

instance MimeRender BS ByteString where
    mimeRender _ = id

type GetBS = Get '[BS] ByteString

type Api = ApiHello :<|> ApiCapture :<|> ApiDeep :<|> ApiAfter

type ApiHello = "echo" :> "hello-world" :> GetBS

serverHello :: Server ApiHello
serverHello = return "Hello World"

type ApiCapture = "echo" :> "plain"
               :> Capture "param" Text
               :> Capture "int" Int
               :> GetBS

serverCapture :: Server ApiCapture
serverCapture param int = return . fromStrict . encodeUtf8 $ Text.replicate int param

type Simple a = "deep" :> "foo" :> "bar" :> "baz" :> a :> GetBS
type ApiDeep = Simple "0"
          :<|> Simple "1"
          :<|> Simple "2"
          :<|> Simple "3"
          :<|> Simple "4"
          :<|> Simple "5"
          :<|> Simple "6"
          :<|> Simple "7"
          :<|> Simple "8"
          :<|> Simple "9"
          :<|> Simple "10"
          :<|> Simple "11"
          :<|> Simple "12"
          :<|> Simple "13"
          :<|> Simple "14"
          :<|> Simple "15"
          :<|> Simple "16"
          :<|> Simple "17"
          :<|> Simple "18"
          :<|> Simple "19"
          :<|> Simple "20"
          :<|> Simple "21"
          :<|> Simple "22"
          :<|> Simple "23"
          :<|> Simple "24"
          :<|> Simple "25"
          :<|> Simple "26"
          :<|> Simple "27"
          :<|> Simple "28"
          :<|> Simple "29"
          :<|> Simple "30"
          :<|> Simple "31"
          :<|> Simple "32"
          :<|> Simple "33"
          :<|> Simple "34"
          :<|> Simple "35"
          :<|> Simple "36"
          :<|> Simple "37"
          :<|> Simple "38"
          :<|> Simple "39"
          :<|> Simple "40"
          :<|> Simple "41"
          :<|> Simple "42"
          :<|> Simple "43"
          :<|> Simple "44"
          :<|> Simple "45"
          :<|> Simple "46"
          :<|> Simple "47"
          :<|> Simple "48"
          :<|> Simple "49"
          :<|> Simple "50"
          :<|> Simple "51"
          :<|> Simple "52"
          :<|> Simple "53"
          :<|> Simple "54"
          :<|> Simple "55"
          :<|> Simple "56"
          :<|> Simple "57"
          :<|> Simple "58"
          :<|> Simple "59"
          :<|> Simple "60"
          :<|> Simple "61"
          :<|> Simple "62"
          :<|> Simple "63"
          :<|> Simple "64"
          :<|> Simple "65"
          :<|> Simple "66"
          :<|> Simple "67"
          :<|> Simple "68"
          :<|> Simple "69"
          :<|> Simple "70"
          :<|> Simple "71"
          :<|> Simple "72"
          :<|> Simple "73"
          :<|> Simple "74"
          :<|> Simple "75"
          :<|> Simple "76"
          :<|> Simple "77"
          :<|> Simple "78"
          :<|> Simple "79"
          :<|> Simple "80"
          :<|> Simple "81"
          :<|> Simple "82"
          :<|> Simple "83"
          :<|> Simple "84"
          :<|> Simple "85"
          :<|> Simple "86"
          :<|> Simple "87"
          :<|> Simple "88"
          :<|> Simple "89"
          :<|> Simple "90"
          :<|> Simple "91"
          :<|> Simple "92"
          :<|> Simple "93"
          :<|> Simple "94"
          :<|> Simple "95"
          :<|> Simple "96"
          :<|> Simple "97"
          :<|> Simple "98"
          :<|> Simple "99"
          :<|> Simple "100"

serverDeep :: Server ApiDeep
serverDeep = return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"
        :<|> return "deep"

type ApiAfter = "after" :> GetBS

serverAfter :: Server ApiAfter
serverAfter = return "after"
