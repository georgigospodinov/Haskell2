module Draw where

import Graphics.Gloss
import Board

-- Constants?
window_width = 600::Int
window_height = 600::Int
wwh = - fromIntegral window_width / 2  -- window width halved
whh = - fromIntegral window_height / 2  -- window height halved
sq_color = black
sq_side = 100::Float

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
--drawWorld w = Color blue $ Circle 100
--drawWorld w = loadBMP "img/tiger.bmp" -- Monad
drawWorld w = Pictures [grid $ size $ board w, tiles $ pieces $ board w]
--drawWorld w = Pictures [grid $ size $ board w, tiles [((0,0), White), ((1,1), Black)]]  -- testing example

grid :: Int -> Picture
grid b_size = Pictures [square (x, y) |
                               x <- [wwh, wwh+sq_side..wwh+sq_side*(fromIntegral b_size -1)],
                               y <- [whh, whh+sq_side..whh+sq_side*(fromIntegral b_size -1)]
                       ]

-- square drawing :: starting position -> side -> picture drawn
square :: Point -> Picture
square (x, y) = Color sq_color $ Line [(x,y), (x,y+sq_side), (x+sq_side,y+sq_side), (x+sq_side,y)]


tiles :: [(Position, Col)] -> Picture
tiles ts = Pictures [tile t | t <- ts]

tile :: (Position, Col) -> Picture
tile ((x, y), c) = translate xtranslation ytranslation $ Color (pc c) $ circleSolid (sq_side/2)
                   where xtranslation = (fromIntegral x*sq_side+wwh+sq_side/2)
                         ytranslation = (fromIntegral y*sq_side+whh+sq_side/2)