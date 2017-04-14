module Main where

import Graphics.Gloss
import System.Environment
import Data.String.Utils

import Board
import Draw
import Input
import Recording
import Network
import Update

{-| Function parses an argument and modifies the world accordingly if the argument
    is recognized. Ignores undrecognised arguments. Please consult the
    README.md for further details. -}
parseArgument :: String -> World -> World
parseArgument str w =   if startswith "size=" str then
                            w{ board = b {size = read $ drop (length "size=") str} }
                        else if startswith "target=" str then
                            w{ board = b {target = read $ drop (length "target=") str} }
                        else if startswith "server=" str then
                            w{ net_data = (net_data w) { useNet = True , isServ = read $ drop (length "server=") str }, board = b { human = (if (read $ drop (length "server=") str) then Black else White) } }
                        else if startswith "addr=" str then
                            w{ net_data = (net_data w) { addr = drop (length "addr=") str} }
                        else if startswith "port=" str then
                            w{ net_data = (net_data w) { port = drop (length "port=") str} }
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
                        else w
                          where b = board w -- argument not recognized

{- | Main method sets up world using any program arguments, sets up networking
     if needed, loads bmp pictures and configures the Gloss play function. -}
main :: IO ()
main =  do
          x <- getArgs                      -- get arguments from the Environment
          w <- setupNetworking (wrld x)     -- setup netowrking if needed
          putStrLn ( "INFO - Gumoku Game started with size=" ++ (show $ size $ board w) ++ " and target=" ++ (show $ target $ board w) ++ ".")
          white_piece <- loadBMP "src/img/white.bmp"    -- white picture (white tiger)
          black_piece <- loadBMP "src/img/black.bmp"    -- black picture (black tiger)
          cell_pic <- loadBMP "src/img/gomoku-part.bmp" -- cell picture  (wood)
          play
            (InWindow "Gomoku"              -- window title
                (ws x, ws x)                -- window size
                (100, 100))                 -- window starting position on screen
            (light blue)                    -- background color
            (if replayon w then 1 else 2)   -- 1 or 2 times per second 'updateWorld' is called
            (startreplay $ w {blacks=black_piece,whites=white_piece,cell=cell_pic})
                                            -- start replaying if replayon otherwise returns world
            drawWorld                       -- in Draw.hs : converts world to Gloss Picture
            handleInput                     -- in Input.hs: handles user input (eg. ctr+s, ...)
            updateWorld                     -- in AI.hs   : updates the world
            where ws w = win_size $ size $ board $ wrld w   -- calculates world size
                  wrld x = foldr parseArgument initWorld x  -- parses arguments
                  startreplay w = if replayon w then sequenceStart w -- starts replay
                                  else w
