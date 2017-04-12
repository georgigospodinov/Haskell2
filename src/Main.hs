module Main where

import Graphics.Gloss
import System.Environment
import Data.String.Utils
import Network.Socket
import System.IO

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
                            w{ isServer = read $ drop (length "server=") str }
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

networkSetup :: IO()
networkSetup = do
                putStrLn "networkSetup"
                sock <- socket AF_INET Stream 0     -- create socket
                setSocketOption sock ReuseAddr 1    -- make socket immediately reusable - eases debugging.
                bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
                listen sock 2                              -- set a max of 2 queued connections
                mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    putStrLn "mainLoop"
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    mainLoop sock           -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hello!\n"
    close sock

toString :: Bool -> String {-  T = Bool (It was a type defined by him-}
toString x = if x then "True" else "False"

main :: IO ()
main = do
          x <- getArgs
          putStrLn "Hi"
          putStrLn $ toString (eliminate (isServer (wrld x)) )
          if (isServer (wrld x) == Just True) then
            networkSetup
          else if(isServer (wrld x) == Just False) then

          else
            putStrLn "No networking"
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
            ((wrld x) {blacks=black_piece,whites=white_piece,cell=cell_pic})
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
