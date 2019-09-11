{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Client (main) where
import ClassyPrelude
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import qualified Network.Socket.ByteString as Nsb

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "127.0.0.1" "3000"
    E.bracket (open addr) close talk
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talk sock = do
        Nsb.sendAll sock "Hello, world!"
        msg <- Nsb.recv sock 1024
        putStr "Received: "
        C.putStrLn msg