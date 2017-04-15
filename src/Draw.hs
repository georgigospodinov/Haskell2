module Draw where

import Graphics.Gloss
import Board
import Menu

import Debug.Trace

{-| Given a world state, return a Picture which will render the world state.
    This will need to extract the Board from the world state and draw it
    as a grid plus pieces. -}
drawWorld :: World -> Picture
drawWorld w = if is_menu w then Pictures [drawMenu currentMenu]
              else if (won $ board w) /= Nothing then
                    Pictures [winmsg (size $ board w) (won (board w))]
              else Pictures [grid w,
                            tiles w
                            ]

-- | Draws the grid as an 2d array of squares
grid :: World -> Picture
grid w = Pictures [square (x, y) w |
                        x <- [wwh bs, wwh bs+sq_side..wwh bs+sq_side*(fromIntegral bs -1)],
                        y <- [wwh bs, wwh bs+sq_side..wwh bs+sq_side*(fromIntegral bs -1)]
                  ]
                  where bs = size $ board w



-- | Draws a square by using the bmp picture and translating it to the right coordinate
square :: Point -> World -> Picture
square (x, y) w = translate (x+sq_side/2) (y+sq_side/2) $ cell w



drawMenu :: Menu -> Picture
drawMenu m = Pictures ([menu_draw x | x <- (entries m)] ++ [decoration_draw x | x <- (decorations m)])

tiles :: World -> Picture
tiles w = Pictures [tile w t | t <- pieces $ board w]

tile :: World -> (Position, Col) -> Picture
tile w ((x, y), c) = translate xtranslation ytranslation $ pic w c
                     where xtranslation = (fromIntegral x*sq_side+wwh bs+sq_side)
                           ytranslation = (fromIntegral y*sq_side+wwh bs+sq_side)
                           bs = size $ board w


winBox :: Point -> Picture
winBox (x, y) = translate x y $ Pictures [(Color (makeColor 190 200 255 100) $ polygon (rectanglePath (win_side1) (win_side2))), lineLoop (rectanglePath (win_side1) (win_side2))]

winmsg :: Int -> (Maybe Col) -> Picture
winmsg bs Nothing = Text ""
winmsg bs (Just c)
  | c == Black = Pictures [winBox (0, 0), (winText (0, 0) "Black Wins")]
  | c == White = Pictures [winBox (0, 0), (winText (0, 0) "White Wins")]


winText :: Point -> String -> Picture
winText (x, y) str = (translate ((x - win_side1/2) + win_margin)
                              ((y - win_side2/2) + win_margin)
                              $ scale win_text_scale win_text_scale $ Text str)
