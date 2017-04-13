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
drawWorld w = if isMenu w then Pictures [menu w]
              else if (won $ board w) /= Nothing
                then Pictures [winmsg (size $ board w) (won (board w))]
                else Pictures [grid w,
                            tiles w
                            ]

grid :: World -> Picture
grid w = Pictures [square (x, y) w |
                        x <- [wwh bs, wwh bs+sq_side..wwh bs+sq_side*(fromIntegral bs -1)],
                        y <- [wwh bs, wwh bs+sq_side..wwh bs+sq_side*(fromIntegral bs -1)]
                  ]
                  where bs = size $ board w



-- square drawing :: starting position -> side -> picture drawn
square :: Point -> World -> Picture
square (x, y) w = translate (x+sq_side/2) (y+sq_side/2) $ cell w
                --Color sq_border $ Line
                --    [(x,y), (x,y+sq_side), (x+sq_side,y+sq_side), (x+sq_side,y), (x,y)]

menuBar :: Point -> World -> String -> Picture
menuBar (x, y) w str = Pictures [translate x y $ (lineLoop (rectanglePath (bar_side1)
                              (bar_side2))), (translate ((x - bar_side1/2) + bar_margin)
                              ((y - bar_side2/2) + bar_margin)
                              $ scale bar_text_scale bar_text_scale $ Text str)]



--[(x,y), (x,y+sq_side), (x+sq_side,y+sq_side), (x+sq_side,y), (x,y)])


menu :: World -> Picture
menu w = Pictures [(menuBar (0,50) w "Single Player - AI"), (menuBar (0,20) w "Multiplayer - Online"), (menuBar (0,-10) w "Multiplayer - Local"), (menuBar (0, -40) w "Quiterini")]

tiles :: World -> Picture
tiles w = Pictures [tile w t | t <- pieces $ board w]

tile :: World -> (Position, Col) -> Picture
tile w ((x, y), c) = translate xtranslation ytranslation $ pic w c
                     where xtranslation = (fromIntegral x*sq_side+wwh bs+sq_side)
                           ytranslation = (fromIntegral y*sq_side+wwh bs+sq_side)
                           bs = size $ board w

winmsg :: Int -> (Maybe Col) -> Picture
winmsg bs Nothing = Text ""
winmsg bs (Just c)
  | c == Black = translate (wwh bs) 0 $ Text "Black Wins"
  | c == White = translate (wwh bs) 0 $ Text "White Wins"
