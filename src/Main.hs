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
import Recording

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

parseArgument :: String -> World -> World
parseArgument str w =   if startswith "size=" str then
                            w{ board = b {size = read $ drop (length "size=") str} }
                        else if startswith "target=" str then
                            w{ board = b {target = read $ drop (length "target=") str} }
                        else if startswith "server=" str then
                            w{ net_data = (net_data w) { useNet = True , isServ = read $ drop (length "server=") str }, board = b { human = (if (read $ drop (length "server=") str) then Black else White) } }
                        else if startswith "addr=" str then
                            w{ net_data = (net_data w) { addr = read $ drop (length "addr=") str} }
                        else if startswith "port=" str then
                            w{ net_data = (net_data w) { port = read $ drop (length "port=") str} }
                        else if startswith "fair=" str then
                            w{ board = b {fair = read $ drop (length "fair=") str} }
                        else if startswith "human=" str then
                            w{ board = b {human = read $ drop (length "human=") str} }
                        else if startswith "ai=" str then
                            w{ board = b {human = other $ read $ drop (length "ai=") str} }
                        else if startswith "computer=" str then
                            w{ board = b {human = other $ read $ drop (length "computer=") str} }
                        else if startswith "ai-on=" str then
                            w{ aion = read $ drop (length "ai-on=") str }
                        else if str == "replay" then
                            w{ replayon = True }
                        else w  -- argument not recognized
                          where b = board w

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
                putStrLn "OK - Connected to '"
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

toString :: Bool -> String {-  T = Bool (It was a type defined by him-}
toString x = if x then "True" else "False"

setupNetworking :: World -> IO World
setupNetworking w = do
                      if (useNet (net_data w))
                        then
                          if (isServ (net_data w))
                            then
                              do sock <- serverSetup (port $ net_data w)
                                 let w' = w { net_data = (net_data w) { Board.socket = Just sock } }
                                 return w'
                          else
                            do sock <- clientSetup (addr $ net_data w)  (port $ net_data w)
                               let w' = w { net_data = (net_data w) { Board.socket = Just sock } }
                               return w'
                      else
                        return w

gray = dark(dark white) -- gray is not predefined
main :: IO ()
main =  do
          x <- getArgs
          w <- setupNetworking (wrld x)
          putStrLn ( "INFO - Gumoku Game started with size=" ++ (show $ size $ board w) ++ " and target=" ++ (show $ target $ board w) ++ ".")
          white_piece <- loadBMP "src/img/white.bmp"
          black_piece <- loadBMP "src/img/black.bmp"
          cell_pic <- loadBMP "src/img/gomoku-part.bmp"
          play
            (InWindow "Gomoku"  -- window title
                (ws x, ws x)
                (100, 100)  -- window starting position on screen
            )
            gray  -- background color
            (if replayon w then 1 else 2)  -- times per second 'updateWorld' is called
            (startreplay $ w {blacks=black_piece,whites=white_piece,cell=cell_pic})
            drawWorld -- in Draw.hs
            handleInput -- in Input.hs
            updateWorld -- in AI.hs
            where ws w = win_size $ size $ board $ wrld w
                  wrld x = foldr parseArgument initWorld x
                  startreplay w = if replayon w then sequenceStart w
                                  else w



-- play:: Display -> Color -> Int
-- -> world
-- -> (world -> Picture)
-- -> (Event -> world -> world)
-- -> (Float -> world -> world)
-- -> IO ()
