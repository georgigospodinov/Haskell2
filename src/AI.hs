module AI where

import Board

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
               -> Position
getBestMove maxdepth gt = undefined
-- if maxdepth == 0 then pick best move of current list
-- else for every move in the list -> recurse with maxdepth -1

asdf_recurse md gt = if md == 0 then maximum val_moves
                      else (0,(0,0))  -- some recursive call with val_moves, probably
                      where next_positions = map fst $ next_moves gt
                            val_moves = [evalMove (game_board gt) p (game_turn gt) | p <- next_positions]

-- How good will the board be after this move?
evalMove :: Board -> Position -> Col -> (Int, Position)
evalMove b p c = case makeMove b c p of Just b' -> (evaluate b' c, p)

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


emptyCells :: Board -> [Position]
emptyCells bd = filter (isEmptyCell bd) boardCells
                where boardCells = [(x,y) | x <- [0 .. ((size bd) - 1)], y <- [0 .. ((size bd) - 1)] ]

isEmptyCell :: Board -> Position -> Bool
isEmptyCell bd cell = let cells = filter ((==cell).fst) (pieces bd)
                       in if length cells == 0 then True
                       else False
