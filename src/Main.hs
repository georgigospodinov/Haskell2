module Main where

import Graphics.Gloss
import System.Environment
import Data.String.Utils
import Network.Socket
import Network.BSD
import System.IO
import Data.List
import Data.Bits
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move
gray = dark(dark white) -- gray is not predefined


parseArgument :: String -> World -> World
parseArgument str w =   if startswith "size=" str then
                            w{ board = b {size = read $ drop (length "size=") str} }
                        else if startswith "target=" str then
                            w{ board = b {target = read $ drop (length "target=") str} }
                        else if startswith "server=" str then
                            w{ net_data = (net_data w) { useNet = True , isServ = read $ drop (length "server=") str }, board = b { human = (if (read $ drop (length "server=") str) then Black else White) } }
                        else if startswith "fair=" str then
                            w{ board = b {fair = read $ drop (length "fair=") str} }
                        else if startswith "human=" str then
                            w{ board = b {human = read $ drop (length "human=") str} }
                        else if startswith "ai=" str then
                            w{ board = b {human = other $ read $ drop (length "ai=") str} }
                        else if startswith "computer=" str then
                            w{ board = b {human = other $ read $ drop (length "computer=") str} }
                        else w  -- argument not recognized
                          where b = board w

serverSetup :: IO Socket
serverSetup = do
                putStrLn "setting up socket"
                sock <- socket AF_INET Stream 0     -- create socket
                setSocketOption sock ReuseAddr 1    -- make socket immediately reusable - eases debugging.
                bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
                putStrLn "listening"
                listen sock 1                              -- set a max of 1 queued connections
                putStrLn "accepting"
                conn <- accept sock     -- accept a connection and handle it
                return sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, saddr) = do
    Network.Socket.send sock "Hello!\n"
    msg <- Network.Socket.recv sock 1024
    putStrLn msg
    sendAll sock $ C.pack "Hello, world!"
    msg <- Network.Socket.ByteString.recv sock 1024
    putStr "Received "
    C.putStrLn msg
    close sock

clientSetup :: IO Socket
clientSetup = withSocketsDo $
      do addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
         let serveraddr = head addrinfos
         sock <- socket (addrFamily serveraddr) Stream defaultProtocol
         connect sock (addrAddress serveraddr)
         return sock

toString :: Bool -> String {-  T = Bool (It was a type defined by him-}
toString x = if x then "True" else "False"

setupNetworking :: World -> IO World
setupNetworking w = do
                      if (useNet (net_data w))
                        then
                          if (isServ (net_data w))
                            then
                              do sock <- serverSetup
                                 w { net_data = (net_data w) { socket = sock } }
                                 return w
                          else
                            do sock <- clientSetup
                               w { net_data = (net_data w) { socket = sock } }
                               return w
                      else
                        return w

main :: IO ()
main = do
          x <- getArgs
          putStrLn "Gumoku started"
          w <- setupNetworking (wrld x)
          -- run our server's logic
          white_piece <- loadBMP "src/img/white.bmp"
          black_piece <- loadBMP "src/img/black.bmp"
          cell_pic <- loadBMP "src/img/gomoku-part.bmp"
          putStrLn "HERE1"
          play
            (InWindow "Gomoku"  -- window title
                (ws x, ws x)
                (100, 100)  -- window starting position on screen
            )  --(FullScreen (1,1))  -- currently fails
            gray  -- background color
            2  -- 'updateWorld' is called 2 times per second
            (w {blacks=black_piece,whites=white_piece,cell=cell_pic})
            drawWorld -- in Draw.hs
            handleInput -- in Input.hs
            updateWorld -- in AI.hs
            where ws w = win_size $ size $ board $ wrld w
                  wrld x = foldr parseArgument initWorld x



-- play:: Display -> Color -> Int
-- -> world
-- -> (world -> Picture)
-- -> (Event -> world -> world)
-- -> (Float -> world -> world)
-- -> IO ()
