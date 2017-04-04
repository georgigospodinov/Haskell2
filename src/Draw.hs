module Draw where

import Graphics.Gloss
import Board

import Debug.Trace

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = Pictures [grid $ board w,
                        tiles w,
                        winmsg (size $ board w) (won (board w))
                       ]

grid :: Board -> Picture
grid b = Pictures [square (x, y) |
                        x <- [wwh bs, wwh bs+sq_side..wwh bs+sq_side*(fromIntegral bs -1)],
                        y <- [wwh bs, wwh bs+sq_side..wwh bs+sq_side*(fromIntegral bs -1)]
                  ]
                  where bs = size b


-- square drawing :: starting position -> side -> picture drawn
square :: Point -> Picture
square (x, y) = Color sq_border $ Line
                    [(x,y), (x,y+sq_side), (x+sq_side,y+sq_side), (x+sq_side,y), (x,y)]


tiles :: World -> Picture
tiles w = Pictures [tile w t | t <- pieces $ board w]

tile :: World -> (Position, Col) -> Picture
tile w ((x, y), c) = translate xtranslation ytranslation $ pic w c
                     where xtranslation = (fromIntegral x*sq_side+wwh bs+sq_side/2)
                           ytranslation = (fromIntegral y*sq_side+wwh bs+sq_side/2)
                           bs = size $ board w

winmsg :: Int -> (Maybe Col) -> Picture
winmsg bs Nothing = Text ""
winmsg bs (Just c)
  | c == Black = translate (wwh bs) 0 $ Text "Black Wins"
  | c == White = translate (wwh bs) 0 $ Text "White Wins"