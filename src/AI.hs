module AI where

import Board

import Debug.Trace

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Col
               -> Position
getBestMove maxdepth gt c = snd $ recurse maxdepth gt c

recurse :: Int -> GameTree -> Col -> (Int, Position)  -- takes forever...
recurse md gt c = if md == 0 then
                    if  curr_vals_movs /= [] then
                        Prelude.maximum $ trace (show $ curr_vals_movs) curr_vals_movs
                    else trace "something horrible has happened" (0,(0,0))
                  else if next_vals_movs == [] then trace "something terrible has happened" (0, (0,0))
                  else Prelude.maximum $ trace (show next_vals_movs) next_vals_movs
                  where val_mov_nt = [(evalMove (game_board g) p c, g) | (p, g) <- next_moves gt]
                        next_value nt = fst (recurse (md-1) nt c)
                        curr_vals_movs = map fst val_mov_nt
                        next_vals_movs = [(min current_value $ next_value nt, move) | ((current_value, move), nt) <- val_mov_nt]

-- How good will the board be after this move? (a valid move is assumed)
evalMove :: Board -> Position -> Col -> (Int, Position)
evalMove b p c = (evaluate b c, p)
{- Edwin suggested that we look for shapes when evaluating
    (BB_BB) is a great position for Black, terrible for White
    also, represent the board in a different way will likely
        make it easier to implement an evaluation function
    our current evalution takes forever because its n^2 at every node in the tree

    Also, we can probably move some of the evaluation from Board.hs to AI.hs
-}
data Shape = Flower | Half_Flower | One_Missing

-- Looks for a shape on the board by the given color and returns the position where the AI needs to play
findShape :: Shape -> Board -> Col -> Maybe Position
findShape Flower b c = undefined
findShape Half_Flower b c = undefined
findShape One_Missing b c = undefined

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w = w

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either
 player has won and display a message if so.
-}

--gen :: Board -> Col -> [Position]
--gen board colour = m

-- need a generator that follows some rules

-- Given a board will return a list of empty cells beside other cells
besideFilledCells :: Board -> Col -> [Position]
besideFilledCells bd c = filter (isBesideFilledCell bd) (emptyCells bd)

isBesideFilledCell :: Board -> Position -> Bool
isBesideFilledCell bd cell
                    | not (isEmptyCell bd (next cell N)) = True
                    | not (isEmptyCell bd (next cell NE)) = True
                    | not (isEmptyCell bd (next cell E)) = True
                    | not (isEmptyCell bd (next cell SE)) = True
                    | not (isEmptyCell bd (next cell S)) = True
                    | not (isEmptyCell bd (next cell SW)) = True
                    | not (isEmptyCell bd (next cell W)) = True
                    | not (isEmptyCell bd (next cell NW)) = True
                    | otherwise = False

-- last resort generator, when the above generator finds nothing
-- Given a board will return list of positions that are empty
emptyCells :: Board -> [Position]
emptyCells bd = filter (isEmptyCell bd) boardCells
                  where boardCells = [(x,y) | x <- [0 .. ((size bd) - 1)], y <- [0 .. ((size bd) - 1)] ]

-- Given a board and a position will check if given position is empty
isEmptyCell :: Board -> Position -> Bool
isEmptyCell bd cell = let cells = filter ((==cell).fst) (pieces bd)
                       in if length cells == 0 then True
                       else False
