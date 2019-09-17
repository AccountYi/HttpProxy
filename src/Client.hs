--{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Client (main) where
import Prelude
import qualified Network.Wai as Wai
import Control.Concurrent.Async (race_)
import Control.Exception -- (SomeException, catch, toException)
import Blaze.ByteString.Builder (fromByteString)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as HC
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types (Method)
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Client.Conduit as HCC
import qualified Data.CaseInsensitive as CI
import Network.Wai.Conduit hiding (Request, requestMethod)
import qualified Network.HTTP.Proxy as Proxy
import Data.Conduit (ConduitT, Flush (..), (.|), mapOutput, runConduit, yield)
import Conduit(mapM_C,mapC)
import Data.Void (Void)
import qualified Data.Conduit.Network as NC
import Debug.Trace



proxyRequest :: Wai.Request -> Proxy.Request
proxyRequest wreq = Proxy.Request
                        (Wai.requestMethod wreq)
                        (Wai.httpVersion wreq)
                        (Wai.requestHeaders wreq)
                        (Wai.rawPathInfo wreq)
                        (Wai.rawQueryString wreq)

waiRequest :: Wai.Request -> Proxy.Request -> Wai.Request
waiRequest original req = original
    { Wai.requestMethod  = Proxy.requestMethod req
    , Wai.httpVersion    = Proxy.httpVersion req
    , Wai.requestHeaders = Proxy.requestHeaders req
    , Wai.rawPathInfo    = Proxy.requestPath req
    , Wai.rawQueryString = Proxy.queryString req
    }                        
main :: IO ()
main = do

    let set = Proxy.defaultProxySettings{Proxy.proxyPort = 3000, Proxy.proxyRequestModifier = proxyReq}

    mgr <- HC.newManager HC.tlsManagerSettings
    Warp.runSettings (Proxy.warpSettings set) $ httpProxyAppRewrite set mgr

proxyReq :: Proxy.Request -> IO (Either Wai.Response Proxy.Request)
proxyReq req = do
    
    return(Right req)


httpProxyAppRewrite :: Proxy.Settings -> HC.Manager -> Wai.Application
httpProxyAppRewrite settings mgr wreq respond = do
    mwreq <- Proxy.proxyRequestModifier settings $ proxyRequest wreq
    either respond (doUpstreamRequest settings mgr respond . waiRequest wreq) mwreq


doUpstreamRequest :: Proxy.Settings -> HC.Manager -> (Wai.Response -> IO Wai.ResponseReceived) -> Wai.Request -> IO Wai.ResponseReceived
doUpstreamRequest settings mgr respond mwreq
    | Wai.requestMethod mwreq == "CONNECT" =
        respond $ responseRawSource (handleConnect mwreq)
                    (Wai.responseLBS HT.status500 [("Content-Type", "text/plain")] "No support for responseRaw")
    | otherwise = do
        hreq0 <- HC.parseRequest $ BS.unpack (Wai.rawPathInfo mwreq <> Wai.rawQueryString mwreq)
        let hreq = hreq0
                { HC.method = Wai.requestMethod mwreq
                , HC.requestHeaders = filter dropRequestHeader $ Wai.requestHeaders mwreq
                , HC.redirectCount = 0 -- Always pass redirects back to the client.
                , HC.requestBody =
                    case Wai.requestBodyLength mwreq of
                        Wai.ChunkedBody ->
                            HC.requestBodySourceChunkedIO (sourceRequestBody mwreq)
                        Wai.KnownLength l ->
                            HC.requestBodySourceIO (fromIntegral l) (sourceRequestBody mwreq)
                -- Do not touch response body. Otherwise there may be discrepancy
                -- between response headers and the response content.
                , HC.decompress = const False
                }
        handle (respond . errorResponse) $
            HC.withResponse hreq mgr $ \res -> do
                let rep = HC.responseBody res
                {- a  <- HC.brConsume  rep  
                traceM(show a) -}  
                let body = mapOutput (Chunk . fromByteString) . HCC.bodyReaderSource $ rep
                    headers = (CI.mk "X-Via-Proxy", "yes") : filter dropResponseHeader (HC.responseHeaders res)
                    
                let repon = responseSource (HC.responseStatus res) headers body
                
                respond $ repon
      where
        dropRequestHeader (k, _) = k `notElem`
            [ "content-encoding"
            , "content-length"
            ]
        dropResponseHeader (k, _) = k `notElem` []

        errorResponse :: SomeException -> Wai.Response
        errorResponse = Proxy.proxyOnException settings . toException

handleConnect :: Wai.Request -> ConduitT () BS.ByteString IO ()-> ConduitT BS.ByteString Void IO a -> IO ()

handleConnect wreq fromClient toClient = do
    let (host, port) =
            case BS.break (== ':') $ Wai.rawPathInfo wreq of
                (x, "") -> (x, 80)
                (x, y) ->
                    case BS.readInt $ BS.drop 1 y of
                        Just (port', _) -> (x, port')
                        Nothing -> (x, 80)
        settings = NC.clientSettings port host
    NC.runTCPClient settings $ \ad -> do
        _ <- runConduit $ yield "HTTP/1.1 200 OK\r\n\r\n" .| toClient
        race_
            (runConduit $ fromClient .| NC.appSink ad .| mapM_C (traceM))
            --(runConduit $ NC.appSource ad .| mapC changeResponse .| toClient )  
            (runConduit $ NC.appSource ad .| toClient )        

changeResponse :: BS.ByteString ->  BS.ByteString 
changeResponse rep = do
    trace $ show rep >> rep



