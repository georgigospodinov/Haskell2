module Main where

import Graphics.Gloss

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
main :: IO ()
main = play
            (InWindow "Gomoku"  -- window title
                (window_width, window_height)
                (500, 600)  -- window starting position on screen
            )
--            (FullScreen (1,1))  -- currently fails
            gray  -- background color
            200
            initWorld -- in Board.hs
            drawWorld -- in Draw.hs
            handleInput -- in Input.hs
            updateWorld -- in AI.hs


-- play:: Display -> Color -> Int
-- -> world
-- -> (world -> Picture)
-- -> (Event -> world -> world)
-- -> (Float -> world -> world)
-- -> IO ()