module Draw where

import Graphics.Gloss
import Board

import Debug.Trace


-- Constants
sq_color = black
sq_side = 100::Float
-- BS = Board Size
win_size :: Int -> Int
win_size bs = bs * (round sq_side::Int)
wwh bs = - fromIntegral (win_size bs) / 2  -- window width halved

-- Piece Color
pc :: Col -> Color
pc Black = black
pc White = white

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
--drawWorld w = loadBMP "img/tiger.bmp" -- Monad
drawWorld w = Pictures [grid $ size $ board w,
                        tiles (size $ board w) $ pieces $ board w,
                        winmsg (size $ board w) (won (board w))
                       ]

grid :: Int -> Picture
grid bs = Pictures [square (x, y) |
                               x <- [wwh bs, wwh bs+sq_side..wwh bs+sq_side*(fromIntegral bs -1)],
                               y <- [wwh bs, wwh bs+sq_side..wwh bs+sq_side*(fromIntegral bs -1)]
                       ]

-- square drawing :: starting position -> side -> picture drawn
square :: Point -> Picture
square (x, y) = Color sq_color $ Line [(x,y), (x,y+sq_side), (x+sq_side,y+sq_side), (x+sq_side,y)]


tiles :: Int -> [(Position, Col)] -> Picture
tiles bs ts = Pictures [tile bs t | t <- ts]

tile :: Int -> (Position, Col) -> Picture
tile bs ((x, y), c) = translate xtranslation ytranslation $ Color (pc c) $ circleSolid (sq_side/2)
                      where xtranslation = (fromIntegral x*sq_side+wwh bs+sq_side/2)
                            ytranslation = (fromIntegral y*sq_side+wwh bs+sq_side/2)

winmsg :: Int -> (Maybe Col) -> Picture
winmsg bs Nothing = Text ""
winmsg bs (Just c)
  | c == Black = translate (wwh bs) 0 $ Text "Black Wins"
  | c == White = translate (wwh bs) 0 $ Text "White Wins"