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
import System.IO.Unsafe

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

port = "5013"

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
                addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
                let serveraddr = head addrinfos
                sock <- Network.Socket.socket (addrFamily serveraddr) Stream defaultProtocol
                bind sock (addrAddress serveraddr)
                putStrLn "listening"
                listen sock 1
                putStrLn "accepting"
                (conn, _) <- accept sock
                putStrLn "connected"
                return conn

clientSetup :: IO Socket
clientSetup = withSocketsDo $
      do addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just port)
         putStrLn ("connecting to 127.0.0.1:" ++ port )
         let serveraddr = head addrinfos
         sock <- Network.Socket.socket (addrFamily serveraddr) Stream defaultProtocol
         connect sock (addrAddress serveraddr)
         putStrLn "connected successfully"
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
                                 let w' = w { net_data = (net_data w) { Board.socket = Just sock } }
                                 return w'
                          else
                            do sock <- clientSetup
                               let w' = w { net_data = (net_data w) { Board.socket = Just sock } }
                               return w'
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
