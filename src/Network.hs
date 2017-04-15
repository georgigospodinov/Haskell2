module Network where

import Board
import Recording


import Debug.Trace
import System.IO.Unsafe

import Network.BSD
import System.IO
import Data.List
import Data.Bits
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

posToString :: (Int, Int) -> String
posToString (x, y) = "(" ++ show x ++ "," ++ show y ++ ")"

stringToPos :: String -> (Int, Int)
stringToPos s = read s::(Int, Int)

sendMove :: World -> (Int, Int) -> IO World
sendMove w (x, y) = do sendMsg (eliminate (Board.socket (net_data w))) ( posToString (x, y) )
                       let s = "sent " ++ posToString (x, y)
                       putStrLn s
                       return w

sendMsg :: Socket -> String -> IO Int
sendMsg sock msg = send sock $ C.pack msg

rcvMsg :: Socket -> String
rcvMsg sock = C.unpack $ unsafeDupablePerformIO $ Network.Socket.ByteString.recv sock 1024

rcvMove :: World -> Socket -> World
rcvMove w sock = wrmv w (pos, c) $
                    w { board = (board w) {pieces = (pieces (board w)) ++ [(pos, c)]},
                        turn = (other $ turn w)} --trace ("Received " ++ posToString pos)
                 where pos = stringToPos msg
                       msg = rcvMsg sock
                       c   = turn w

setupNetworking :: World -> IO World
setupNetworking w = do if (useNet (net_data w)) then
                          if (isServ (net_data w)) then
                             do sock <- serverSetup (port $ net_data w)
                                let w' = w { net_data = (net_data w) { Board.socket = Just sock } }
                                return w'
                          else do sock <- clientSetup (addr $ net_data w)  (port $ net_data w)
                                  let w' = w { net_data = (net_data w) { Board.socket = Just sock } }
                                  return w'
                       else return w

unsafeSetupNetworking :: World -> World
unsafeSetupNetworking w = unsafeDupablePerformIO $ setupNetworking w

serverSetup :: String -> IO Socket
serverSetup prt = do
               addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just prt)
               let serveraddr = head addrinfos
               sock <- Network.Socket.socket (addrFamily serveraddr) Stream defaultProtocol
               bind sock (addrAddress serveraddr)
               listen sock 1
               putStrLn "OK - Socket Ready"
               (conn, _) <- accept sock
               putStrLn "OK - Connected to peer."
               putStrLn "INFO - Acting as Server."
               return conn

clientSetup :: String -> String -> IO Socket
clientSetup addr prt = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just addr) (Just prt)
        putStrLn ("INFO - Attempting to connect to " ++ addr ++ ":" ++ prt )
        let serveraddr = head addrinfos
        sock <- Network.Socket.socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        putStrLn ("OK - Connected to '" ++ addr ++ ":" ++ prt ++ "'")
        putStrLn "INFO - Acting as Client."
        return sock
