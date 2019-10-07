module Lib(someFunc) where

import Network.HTTP.Proxy.Server
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Base
import Debug.Trace
import Network.HTTP
import Data.ByteString.Base64.Lazy

someFunc :: IO ()
someFunc = proxyMain (def :: Settings LB.ByteString) {hostname = Just "localhost",portnum = 3000, responseModifier = responseMod}

responseMod :: Request LB.ByteString -> Response LB.ByteString -> IO (Response LB.ByteString)
responseMod req rep = do
    let aa = rspHeaders rep
    let body = rspBody rep
    let b = encode body 

    traceM(show "---------------")
    traceM(show b)
    traceM(show aa)
    traceM(show $ body)
    return rep