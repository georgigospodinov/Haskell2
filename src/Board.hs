module Board where

import Graphics.Gloss
import Data.Foldable

data Col = Black | White
  deriving (Show, Eq)

instance Read Col where
    readsPrec d "black" = [(Black, "")]
    readsPrec d "Black" = [(Black, "")]
    readsPrec d "white" = [(White, "")]
    readsPrec d "White" = [(White, "")]

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- Constants
sq_border :: Color
sq_border = black
sq_side = 50::Float
-- BS = Board Size
win_size :: Int -> Int
win_size bs = bs * (round sq_side::Int)
wwh bs = - fromIntegral (win_size bs) / 2  -- window width halved

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     human :: Col,
                     pieces :: [(Position, Col)],
                     won :: (Maybe Col)
                   }
  deriving Show

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 Black [] Nothing

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
                     cmd :: String,
                     blacks :: Picture,
                     whites :: Picture
                   }

pic :: World -> Col -> Picture
pic w Black = blacks w
pic w White = whites w

initWorld = World initBoard Black ""
            (Color black $ circleSolid (sq_side/2))
            (Color white $ circleSolid (sq_side/2))

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c (x,y) =  if won b /= Nothing then Nothing  -- Do not accept new moves, once there is a winner.
                      else if ((lookup (x,y) (pieces b)) == Nothing)
                            then Just b'{won = checkWon b'}  -- Update winner after pieces.
                           else Nothing
                            where b' = b{pieces = pieces b++[((x,y),c)]}  -- Update pieces first.

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won

--eliminate :: Maybe a -> a  -- does not get called anywhere
--eliminate (Just a) = a


checkWon :: Board -> Maybe Col
--checkWon b = checkAllPos b  -- Does not seem to detect properly.
checkWon b = if longest b Black == target b then Just Black
             else if longest b White == target b then Just White
             else Nothing

checkAllPos :: Board -> Maybe Col
checkAllPos board =
  case ls of
    []  -> Nothing
    [c] -> Just c
    --_   -> throw "sth wrogngngns"
    where ls = map woJ (filter js (map f (pieces board)))
          js x  = x /= Nothing
          woJ (Just x) = x
          f e = if (checkPos (fst e) (pieces board) "e" (snd e)) == target board
            then Just (snd e)
            else Nothing

checkPos :: Position -> [(Position, Col)] -> String -> Col -> Int
checkPos lstPos []  d c = 0
checkPos lstPos xs  d c = case d of
                            "nw" -> 0
                            "n"  -> 0
                            "ne" -> 0
                            "e"  ->
                              if (lookup newPos xs) == Just c
                                then 1 + checkPos newPos xs d c
                                else 1
                                where newPos = ((fst lstPos) + 1, (snd lstPos))
                            "w"  -> 0
                            "se" -> 0
                            "sw" -> 0

{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}
data Direction = N | S | E | W | NE | SE | NW | SW
    deriving (Enum, Show)
next :: Position -> Direction -> Position
next (x,y) N = (x+1, y)
next (x,y) S = (x-1, y)
next (x,y) E = (x, y+1)
next (x,y) W = (x, y-1)
next (x,y) NE = (x+1, y+1)
next (x,y) NW = (x+1, y-1)
next (x,y) SE = (x-1, y+1)
next (x,y) SW = (x-1, y-1)

descend :: Board -> Col -> Direction -> Position -> (Int, Bool)  -- length, (blocked?)
descend b c dir (x,y) = if outOfBounds then (0, True)
                        else if colOf (x,y) == Just (other c) then (0, True)
                        else if colOf (x,y) == Just c then (l+1, bl)
                        else (0, False)  -- empty
                        where (l, bl) = descend b c dir $ next (x,y) dir
                              colOf :: Position -> Maybe Col
                              colOf pos = case piece_at pos of
                                            [] -> Nothing
                                            [piece_found] -> Just $ snd piece_found
                              piece_at :: Position -> [(Position, Col)]
                              piece_at p = filter (\ (pos, c) -> pos == p) (pieces b)
                              outOfBounds = x < 0 || y < 0 || x >= size b || y >= size b

longest :: Board -> Col -> Int  -- the longest when descending from every square in all directions
longest b c = max' $ map fst $  -- take the maximum length
                -- take only lengths that: are not blocked OR are the target (victory)
                filter (\ (l, bl) -> bl==False || l == target b)
                    [descend b c dir (x,y) | dir <- [N ..], x <- [0..size b -1], y <- [0..size b -1]]
                where max' [] = 0
                      max' xs = Prelude.maximum xs

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate b c = if lc == target b then target b  -- if c can win from here = best
               else if loc == target b then 0- target b  -- else if !c can win from here = worst
               else lc-loc  -- otherwise longest c - longest !c
               where lc = longest b c
                     loc = longest b $ other c
