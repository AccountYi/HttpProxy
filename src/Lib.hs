module Lib where

import ClassyPrelude hiding (getContents)
import qualified Control.Exception as E
import qualified Data.ByteString as S
import Network.Socket
import qualified Network.Socket.ByteString as Nsb


import Network.Socket.Internal 
import Control.Monad
import Control.Concurrent
import GHC.IO.Handle
import System.Posix.IO
import System.Posix.Types
import System.Environment

import Network.Socket.ByteString.Lazy


import Time.Repeatedly
import Control.Concurrent
import System.Clock

import Control.Monad.Catch
import Network.DNS
import Data.IP
import qualified Data.List as List(head,(!!))
import qualified Data.ByteString.Char8 as Dbc

runProxy :: String  -> IO ()

runProxy port   = do
  let proxyHints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
  proxyAI:_ <- getAddrInfo  -- 此函数返回的AddrInfo值包含可以直接传递给connect或bind的SockAddr值
                        (Just proxyHints) -- 首选套接字类型或协议
                        Nothing         -- 要查找的主机名
                        (Just port)   -- 要查找的服务名称  例如“http”或数字端口号。

  proxySock <- socket -- 使用给定的地址家庭，套接字类型和协议号创建一个新套接字。地址族通常是AF_INET，AF_INET6或AF_UNIX。套接字类型通常是Stream或Datagram。协议号通常是defaultProtocol。
                        (addrFamily proxyAI)     -- 地址家庭
                        (addrSocketType proxyAI) -- 套接字类型
                        (addrProtocol proxyAI)   -- 协议号  (addrProtocol proxyAI)
  bind proxySock (addrAddress proxyAI) --将套接字绑定到一个地址。套接字必须尚未绑定。传递给bind的Family必须与传递给socket的相同。
  
  listen proxySock 5 --侦听对套接字的连接。第二个参数指定排队连接的最大数量，并且应至少为1;最大值取决于系统（通常为5）。
  forever $ do
    (clientSock, sockAddr) <- accept proxySock -- 返回的第一个是可用于在连接上发送和接收数据的新套接字对象，第二个是绑定到连接另一端的套接字的地址。
    
{-     case sockAddr of
      SockAddrInet portNumber hostAddress ->
            traceM((show "port:") ++ (show portNumber) ++ (show "==========host:") ++ (show hostAddress)) -}


    proxySocket clientSock  sockAddr


getSockAddrFamily :: SockAddr -> Family

getSockAddrFamily (SockAddrUnix _) = AF_UNIX

getSockAddrFamily (SockAddrInet _ _) = AF_INET

getSockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6    






  
forwardData :: Socket -> Socket  -> Int -> SockAddr ->IO ()

forwardData srcSock destSock1  a addrs = do
  traceM("sssssssssssssssssssssssssssssssssssssssssssss")
  msg <- Nsb.recv srcSock 32768 --从套接字接收数据。套接字必须处于连接状态
  let msgs = lines $ (unpack . decodeUtf8) msg
  
  case a of 
    1 ->  case addrs of
              SockAddrInet portNumber hostAddress ->
                traceM("sssssssssssssssssssssssssssssssssssssssssssss" ++ show msgs)
                    --traceM((show "port11:") ++ (show portNumber) ++ (show "==========host11:") ++ (show hostAddress) ++ (show $ "***one:    " ++ msg))
    2 ->  case addrs of
              SockAddrInet portNumber hostAddress ->
                    traceM((show "port22:") ++ (show portNumber) ++ (show "==========host22:") ++ (show hostAddress) ++ (show $ "***tow:    " ++ msg)   )


  unless (S.null msg) $ do
    
    Nsb.sendAllTo destSock1 msg addrs -- 将数据发送到服务端套接字
    --S.hPut destSock2 msg -- 将 msg 输出到指定的Handle
    forwardData srcSock destSock1  a addrs




getSockAddr :: (MonadIO m, MonadThrow m) => ByteString -> m SockAddr

getSockAddr msg  = do
  
  let httpUrl = Dbc.words msg List.!! 1
  case Dbc.take 5 httpUrl of

    "http:" -> do
      
      let removePrefix =  Dbc.drop 7 httpUrl
      --获取域名
      domainName <- case Dbc.elemIndex '/'  removePrefix of
            Just index -> 
              return $ Dbc.take index removePrefix

            Nothing -> throwString $ "http 域名解析错误"

      -- 获取域名对应的IP地址      
      rs <- liftIO $ makeResolvSeed defaultResolvConf
      domainIP <- liftIO $  withResolver rs $ \resolver -> lookupA resolver domainName
      ip <- case domainIP of
                Right ip -> return $ List.head ip

                Left err  ->  throwString $ "非法域名： " <> show err
      
              
      return $ SockAddrInet 80 $ toHostAddress ip
      
    "https" -> do
      
      let removePrefix =  Dbc.drop 8 httpUrl
      --获取域名
      domainName <- case Dbc.elemIndex '/'  removePrefix of
            Just index -> 
              return $ Dbc.take index removePrefix

            Nothing -> throwString $ "http 域名解析错误"

      -- 获取域名对应的IP地址      
      rs <- liftIO $ makeResolvSeed defaultResolvConf
      domainIP <- liftIO $  withResolver rs $ \resolver -> lookupA resolver domainName
      ip <- case domainIP of
                Right ip -> return $ List.head ip

                Left err  ->  throwString $ "非法域名： " <> show err
      
                
      return $ SockAddrInet 443 $ toHostAddress ip

    _ ->    
      
      return(SockAddrInet 443 $ tupleToHostAddress (118,89,204,200)) 




proxySocket :: Socket ->  SockAddr -> IO()

proxySocket clientSock  addrs = do

  msg <- Nsb.recv clientSock 4096 --从客户端的套接字接收数据。套接字必须处于连接状态

  --let msgs = Dbc.lines.Dbc.unwords $ Dbc.split '\r' msg
  let msgs = Dbc.lines msg

  serverAddr <- getSockAddr $ List.head msgs

  traceM(show "===================================================================================")
  traceM(show serverAddr)

  let serverAI = defaultHints { addrSocketType = Stream, addrAddress = serverAddr, addrFamily = getSockAddrFamily serverAddr  }

  serverSock <- socket (addrFamily serverAI) (addrSocketType serverAI) (addrProtocol serverAI)

  connect serverSock serverAddr

  forkFinally (forwardData clientSock serverSock  1 serverAddr) (\_ -> close clientSock >> close serverSock)

  forkFinally (forwardData serverSock clientSock  2 addrs) (\_ -> close clientSock >> close serverSock)
  
  return ()

loopPack :: Clock -> IO ()
loopPack clo = do
  a <- getTime clo
  print(a)
main :: IO ()

main = do

  forkFinally (asyncRepeatedly' 0.03 Realtime  (loopPack  Realtime)) (\_ -> print("打印268632325包的线程结束"))
  --cmdArgs <- getArgs

  runProxy "3000"  