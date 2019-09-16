--{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Client (main) where
import ClassyPrelude
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import qualified Network.Socket.ByteString as Nsb


import qualified Network.Wai as Wai
import Network.HTTP.Proxy



main :: IO ()
main = do

    let set = defaultProxySettings{proxyPort = 3000, proxyRequestModifier = proxyReq}
    runProxySettings set

proxyReq :: Request -> IO (Either Wai.Response Request)
proxyReq req = do
    print req
    return(Right req)