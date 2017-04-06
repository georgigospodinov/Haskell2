module Main where

import Graphics.Gloss
import System.Environment
import Data.String.Utils

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

parseArgument :: String -> Board -> Board
parseArgument str b =   if startswith "size=" str then
                            b {size = read $ drop (length "size=") str}
                        else if startswith "target=" str then
                            b {target = read $ drop (length "target=") str}
                        else if startswith "fair=" str then
                            b {fair = read $ drop (length "fair=") str}
                        else if startswith "human=" str then
                            b {human = read $ drop (length "human=") str}
                        else if startswith "ai=" str then
                            b {human = other $ read $ drop (length "ai=") str}
                        else if startswith "computer=" str then
                            b {human = other $ read $ drop (length "computer=") str}
                        else b  -- argument not recognized

main :: IO ()
main = do x <- getArgs
          white_piece <- loadBMP "src/img/white.bmp"
          black_piece <- loadBMP "src/img/black.bmp"
          play
            (InWindow "Gomoku"  -- window title
                (ws x, ws x)
                (100, 100)  -- window starting position on screen
            )  --(FullScreen (1,1))  -- currently fails
            gray  -- background color
            10  -- 'updateWorld' is called 10 times per second
            ((wrld x) {blacks=black_piece,whites=white_piece})
            drawWorld -- in Draw.hs
            handleInput -- in Input.hs
            updateWorld -- in AI.hs
            where ws x = win_size $ size $ board $ wrld x
                  wrld x = initWorld {board = foldr parseArgument initBoard x}

-- play:: Display -> Color -> Int
-- -> world
-- -> (world -> Picture)
-- -> (Event -> world -> world)
-- -> (Float -> world -> world)
-- -> IO ()
