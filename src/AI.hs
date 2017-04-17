module AI where

import GameWorld
import Recording

import Debug.Trace
import Data.List
import System.IO.Unsafe

{-| The game tree consisting of the board, the current turn and a list of
    possible moves and the resulting sub-gametree. -}
data GameTree = GameTree { game_board :: Board,
                           game_turn  :: Col,
                           next_moves :: [(Position, GameTree)] }

{-| A function to generate moves (i.e. board positions) for a player (Col)
    on a particular board, resulting in a (potentially) infinite game tree.
    Makes calls to the move generator to construct the tree of positions.
    relies on lazy evaluation to not construct more nodes than necessary.
    This means that even though the tree is huge,
    only the nodes accessed get created. -}
buildTree :: (Board -> Col -> [Position]) -- Move generator
             -> Board                     -- board state
             -> Col                       -- player to play next
             -> GameTree                  -- resulting game tree
buildTree gen b c = let moves = gen b c in  -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of          -- try making the suggested move
               Nothing -> mkNextStates xs   -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                                            -- successful => make move and build tree from here for opposite player

{- | Get the best next move from a (possibly infinite) game tree. This
     traverses the game tree up to a certain depth, and pick the move which
     leads to the position with the best score for the player whose turn it
     is at the top of the game tree. -}
getBestMove :: Int          -- Maximum search depth
               -> GameTree  -- Initial game tree
               -> Col       -- whose turn it is
               -> Position  -- the resulting best move
getBestMove maxdepth gt c = snd $ recurse maxdepth "b" gt c

-- | Selector type selects a value-move tuple from a list of such tuples.
type Selector = [(Int, Position)] -> (Int, Position)

{- | A helper function for finding the best move. Returns a value-move tuple.
     This recursion goes down the game tree nodes, using the Selector to make predictions about
     the moves that will be taken. This function changes its selector every time it calls itself.
     This reflects the presumption that the AI will select the best move and the human player -
     the worst move (one that puts the AI in the worst position). -}
recurse :: Int -> String-> GameTree -> Col -> (Int, Position)
recurse 0 s gt c = getSelector s [(evaluate (game_board g) c, p) | (p, g) <- next_moves gt]
recurse d s gt c = getSelector s options
                        where   curr_values :: [(Int, Position)]      -- result of evaluating every possible board
                                curr_values = [(evaluate (game_board g) c, p) | (p, g) <- next_moves gt]
                                next_values :: [(Int, Position)]      -- result of recursing down every node
                                next_values = [recurse (d-1) (s) nt (other c) | (p, nt) <- next_moves gt]
                                options :: [(Int, Position)]          -- taking the lower of the sub-tree and the current node
                                options = [(min' cv nv, p) | ((nv, _),(cv, p)) <- zip next_values curr_values]
                                min' a b = if a == 0 - (target $ game_board gt) then a    -- if the game is already lost, it cannot get better
                                           else if a == (target $ game_board gt) then a   -- if a means winning, then a
                                           else min a b                                   -- else, the lower of the two
                                           -- this means it will detect if a move will eventually lead to a lost game

{- | Function swaps the selectors best and worst as every level in the gametree
     will need a different selector (my best move vs opponents worst move) -}
getSelector :: String -> Selector
getSelector "w" = worst
getSelector "b" = best

switch :: String -> String
switch "b" = "w"
switch "w" = "b"

-- | Selector selects the best move from a list of moves and corresponding values.
best :: Selector
best []  = (0, (0,0))                         -- empty list = end of game or board is full
best [x] = x                                  -- only 1 possible move
best ips = foldr greater (head ips) (tail ips)-- do a comparison, starting from the head and continuing to the tail

-- | Selector selects the worst move from a list of moves and corresponding values.
worst :: Selector
worst []  = (0, (0,0))                        -- empty list = end of game or board is full
worst [x] = x                                 -- only 1 possible move
worst ips = foldr lesser (head ips) (tail ips)-- do a comparison, starting from the head and continuing to the tail

-- | Comparator for value-move touples. Returns the 'greater' move.
greater :: (Int, Position) -> (Int, Position) -> (Int, Position)
greater (i1, p1) (i2, p2) = if i1 > i2 then (i1, p1)
                         else (i2, p2)

-- | Comparator for value-move touples. Returns the 'lesser' move.
lesser :: (Int, Position) -> (Int, Position) -> (Int, Position)
lesser (i1, p1) (i2, p2) = if i1 < i2 then (i1, p1)
                         else (i2, p2)

-- | The AI makes a move by calling getBestMove to determine where to put a piece.
aiturn :: World -> World
aiturn w = if turn w == c then  -- if its the ai's turn
              w {board = case makeMove b c move  of
                              Just b'-> wrmv w (move, c) b' -- write the move to the recording and return the new board
                              Nothing -> trace ("ERROR - Cannot make move: " ++ show move) b,
                 turn = human b, prev = Just w}
           else w -- otherwise do no change
           where b    = board w         -- the board
                 c    = other $ human b -- color of ai
                 gt   = buildTree besideFilledCells b c     -- game tree
                 turnsToThinkAhead = (ai_level w)  -- drastic slow down at 3, fast at 1
                 move = getBestMove turnsToThinkAhead gt c  -- the best move

-- | Given a board will return a list of empty cells beside filled cells
besideFilledCells :: Board -> Col -> [Position]
besideFilledCells bd c = removeDuplicates list_of_all             -- list without duplicates
                         where list_of_all = foldr (++) [] lists  -- concatenate all lists
                               lists = [emptyNeighbors bd p | (p, _) <- pieces bd]
                                                                  -- list of lists of empty neighbors for each piece

-- | Function checks if a given cell is beside a filled cell
isBesideFilledCell :: Board -> Position -> Bool
isBesideFilledCell bd cell
                    | not (isEmptyCell bd (next cell N))  = True
                    | not (isEmptyCell bd (next cell NE)) = True
                    | not (isEmptyCell bd (next cell E))  = True
                    | not (isEmptyCell bd (next cell SE)) = True
                    | not (isEmptyCell bd (next cell S))  = True
                    | not (isEmptyCell bd (next cell SW)) = True
                    | not (isEmptyCell bd (next cell W))  = True
                    | not (isEmptyCell bd (next cell NW)) = True
                    | otherwise = False

{- | Given a board, this function will return list of positions that are empty
     This is very slow for large boards. Also this generator of moves is the last
     if the "besideFilledCells" generator fails. -}
emptyCells :: Board -> [Position]
emptyCells bd = filter (isEmptyCell bd) boardCells
                  where boardCells = [(x,y) | x <- [0 .. ((size bd) - 1)], y <- [0 .. ((size bd) - 1)] ]

-- | Given a board and position, function will check if given position is empty
isEmptyCell :: Board -> Position -> Bool
isEmptyCell bd cell = colOf bd cell == Nothing

-- | Given a board and position, function will return a list of empty neighbor cells
emptyNeighbors :: Board -> Position -> [Position]
emptyNeighbors b pos = filter (isEmptyCell b) $ neighbors b pos

-- | Function will return all neighbors to a cell (takes care of cells on the edge)
neighbors :: Board -> Position -> [Position]
neighbors b pos = filter (\ p -> outOfBounds b p == False) [next pos dir | dir <- [N ..]]

-- | Function removes duplicates from a list of positions
removeDuplicates :: [Position] -> [Position]
removeDuplicates [] = []
removeDuplicates (x:xs) | x `elem` xs = removeDuplicates xs
                        -- for every element, if it is present in the remaining elements, ignore it
                        | otherwise = x : removeDuplicates xs

-- | Displays a hint based on the ai algorithm in the console for the user.
hint :: World -> World
hint w = trace ("HINT - " ++ (show $ getBestMove 0 gt c)) w
         where gt = buildTree besideFilledCells b c
               b = board w
               c = human b
