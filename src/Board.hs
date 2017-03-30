module Board where

import Data.Foldable

data Col = Black | White
  deriving (Show, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)],
                     won :: (Maybe Col)
                   }
  deriving Show

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 [] Nothing

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
                     cmd :: String
                   }

initWorld = World initBoard Black ""

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
eliminate :: Maybe a -> a
eliminate (Just a) = a


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
data Direction = N | E | NE
descend :: Board -> Col -> Position -> Direction -> Int
descend b c (x,y) dir = if isC (x,y) then  -- descend north, east, north-east
                            (1::Int) + descend b c next dir
                        else 0
                        where isC :: Position -> Bool
                              isC pos = case piece_at pos of
                                            [] -> False
                                            [piece_found] -> snd piece_found == c
                              piece_at :: Position -> [(Position, Col)]
                              piece_at p = filter (\ (pos, c) -> pos == p) (pieces b)
                              next = case dir of
                                        N -> (x+1,y)
                                        E -> (x,y+1)
                                        NE -> (x+1,y+1)

longest :: Board -> Col -> Int
longest b c = Prelude.maximum [max nd (max ed ned) |  -- the largest of the three descensions
                                nd <- [descend b c (x,y) N | x <- [0..size b -1], y <- [0..size b -1]],  -- descend north
                                ed <- [descend b c (x,y) E | x <- [0..size b -1], y <- [0..size b -1]],  -- descend east
                                ned <- [descend b c (x,y) NE | x <- [0..size b -1], y <- [0..size b -1]]  -- descend north-east
                              ]

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate b c = if lc == size b -1 then size b -1  -- if c can win from here = best
               else if loc == size b -1 then 1- size b  -- else if !c can win from here = worst
               --TODO update the two ifs to acknowledge blocked rows.
               else lc-loc-- otherwise longest c - longest !c
               where lc = longest b c
                     loc = longest b $ other c
