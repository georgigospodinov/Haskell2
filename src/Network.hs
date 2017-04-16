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

-- | Converts a (x,y) position to a String to send over the network "(x,y)"
posToString :: (Int, Int) -> String
posToString (x, y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- | Reads a String into a (x,y) position.
stringToPos :: String -> (Int, Int)
stringToPos s = read s::(Int, Int)

-- | Sends a move over a connected socket. Converts the position to string first.
sendMove :: World -> (Int, Int) -> IO World
sendMove w (x, y) = do sendMsg (eliminate (Board.socket (net_data w))) ( posToString (x, y) )
                       return w
                       -- eliminate is safe to use because this function is only called if there is
                       -- definitely a socket connection.

-- | Sends a given string over a connected socket.
sendMsg :: Socket -> String -> IO Int
sendMsg sock msg = send sock $ C.pack msg

{- | Waits on an incoming message from a given Socket. Returns when a string has
     been recieved. Limit is currently 1024 bytes which is more than enough for
     th strings we are sending. -}
rcvMsg :: Socket -> String
rcvMsg sock = C.unpack $ unsafeDupablePerformIO $ Network.Socket.ByteString.recv sock 1024
          -- unsafeDupablePerformIO is used to contain the IO() polution

{- | Uses rcvMsg to recieve a string from an connected socket and converts it
     into a (x,y) position. Then adds the position with the turn's colour to the
     pieces of the board.
     Note: Does not check for validity of move. This is the responsibility of the
           other socket endpoint. For this practical we assume that we are only
           connected to other instances of our game. -}
rcvMove :: World -> Socket -> World
rcvMove w sock = wrmv w (pos, c) $
                    w { board = (board w) {pieces = (pieces (board w)) ++ [(pos, c)]},
                        turn  = (other $ turn w)}
                 where pos = stringToPos msg
                       msg = rcvMsg sock
                       c   = turn w

-- Sets up sockets if required or just returns the world. Sets up client/server if needed.
setupNetworking :: World -> IO World
setupNetworking w = do if (useNet (net_data w)) then      -- if network is used
                          if (isServ (net_data w)) then   -- if server
                             do sock <- serverSetup (port $ net_data w) -- run serverSetup and add socket to world
                                let w' = w { net_data = (net_data w) { Board.socket = Just sock } }
                                return w'
                                                          -- else client
                          else do sock <- clientSetup (addr $ net_data w)  (port $ net_data w)
                                                                -- run clientSetup and add socket to world
                                  let w' = w { net_data = (net_data w) { Board.socket = Just sock } }
                                  return w'
                       else return w                      -- else no networking, so just return worlds

{- | Runs setupNetworking discarding IO() for when the game is started with
     networking from the Menu -}
unsafeSetupNetworking :: World -> World
unsafeSetupNetworking w = unsafeDupablePerformIO $ setupNetworking w

{- | Sets up the server and waits on an incoming connection when the socket is
     ready. Returns a successfully connected Socket. Fails if port is already in
     use or permissions are not sufficient. -}
serverSetup :: String -> IO Socket
serverSetup prt = do
               addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just prt)              -- set up serveraddr with specified port
               let serveraddr = head addrinfos
               sock <- Network.Socket.socket (addrFamily serveraddr) Stream defaultProtocol
                                                  -- iniitalises socket
               bind sock (addrAddress serveraddr)
               listen sock 1                      -- only allow 1 simultanious connection
               putStrLn "OK - Socket Ready"
               (conn, _) <- accept sock           -- accept connections
               putStrLn "OK - Connected to peer."
               putStrLn "INFO - Acting as Server."
               return conn

{- | Sets up the client and attempts to connect to an endpoint with the specified
     address and port. Fails if endpoint doesnt exist or refuses connection. -}
clientSetup :: String -> String -> IO Socket
clientSetup addr prt = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just addr) (Just prt) -- sets up addrinfos with specified address and port
        putStrLn ("INFO - Attempting to connect to " ++ addr ++ ":" ++ prt )
        let serveraddr = head addrinfos
        sock <- Network.Socket.socket (addrFamily serveraddr) Stream defaultProtocol -- initialise socket
        connect sock (addrAddress serveraddr) -- connect to socket
        putStrLn ("OK - Connected to '" ++ addr ++ ":" ++ prt ++ "'")
        putStrLn "INFO - Acting as Client."
        return sock
