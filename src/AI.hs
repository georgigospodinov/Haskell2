module AI where

import Board

import Debug.Trace
import Data.List
import System.IO.Unsafe

import Network.BSD
import System.IO
import Data.List
import Data.Bits
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

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

recurse :: Int -> GameTree -> Col -> (Int, Position)
recurse 0 gt c = best [(evaluate (game_board g) c, p) | (p, g) <- next_moves gt]
recurse d gt c = if (d `div` 2)* 2 == d then best options  -- ai plays on even depth
                 else if (fst $ best options) == (target $ game_board gt) then best options  -- if a winning move is found
                 else worst options  -- assumes the opponent's move will be the worst (for it) option
                 where curr_values :: [(Int, Position)]  -- resultt of evaluating every possible board
                       curr_values = [(evaluate (game_board g) c, p) | (p, g) <- next_moves gt]
                       next_values :: [(Int, Position)]  -- result of recursing down every node
                       next_values = [recurse (d-1) nt c | (p, nt) <- next_moves gt]
                       options :: [(Int, Position)]  -- taking the lower of the sub-tree and the current node
                       options = [(min' cv nv, p) | ((nv, _),(cv, p)) <- zip next_values curr_values]
                       min' a b = if a == 0- (target $ game_board gt) then a  -- if the game is already lost, it cannot get better
                                  else if a == (target $ game_board gt) then a  -- if a means winning, then a
                                  else min a b  -- else, the lower of the two
                       -- this way it will detect if a move will eventually lead to a loss

worst :: [(Int, Position)] -> (Int, Position)
worst [] = trace "worst:NO MOVES???" (0, (0,0))
worst [x] = trace "worst:One move??" x
worst ips = foldr comp2 (head ips) (tail ips)

comp2 :: (Int, Position) -> (Int, Position) -> (Int, Position)
comp2 (i1, p1) (i2, p2) = if i1 < i2 then (i1, p1)
                          else (i2, p2)

best :: [(Int, Position)] -> (Int, Position)
best [] = trace "best:NO MOVES???" (0, (0,0))
best [x] = trace "best:One move??" x
best ips = foldr comp (head ips) (tail ips)  -- do a comparison, starting from the head and continuing with the tail

comp :: (Int, Position) -> (Int, Position) -> (Int, Position)
comp (i1, p1) (i2, p2) = if i1 > i2 then (i1, p1)
                         else (i2, p2)

posToString :: (Int, Int) -> String
posToString (x, y) = "(" ++ show x ++ "," ++ show y ++ ")"

stringToPos :: String -> (Int, Int)
stringToPos s = read s::(Int, Int)

rcvMsg :: Socket -> String
rcvMsg sock = trace "rcv msg" $ C.unpack $ unsafeDupablePerformIO $ Network.Socket.ByteString.recv sock 1024

rcvMove :: World -> Socket -> World
rcvMove w sock =  w { board = (board w) {pieces = (pieces (board w)) ++ [((x, y), c)]}, turn = (other $ turn w)} --trace ("Received " ++ posToString (x, y))
         where (x, y) = stringToPos msg --(trace ("<<" ++ msg)
               msg    = rcvMsg sock
               c      = trace ("waiting for move from " ++ show (turn w)) turn w

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w = waitForNet( w {board = (board w) {won = checkWon (board w)}, checked = True} )
                        where waitForNet w = if (useNet (net_data w)) && ((human $ board w) /= turn w)      -- if we use the network and it is not my turn (w is world with switched turn)
                                              then trace "waiting" rcvMove w (eliminate (Board.socket (net_data w)))        --    then wait and recieve the next move
                                             else trace "updating world without wait" $ w                                                         --  else just return the world
--if not $ checked w then  -- check winner
--                     w {board = (board w) {won = checkWon (board w)},
--                        checked = True}
--                  else (aiturn w) {checked = False}  -- otherwise the ai takes turn

aiturn :: World -> World
aiturn w = if turn w == c then  -- if the ai is supposed to take turn
              w {board = case makeMove b c move  of
                              Just b'-> b'
                              Nothing -> trace ("failed to make move:"++show move) $ board w,
                 turn = human b, prev = Just w}
           else w -- otherwise do no change
           where b = board w
                 c = other $ human b
                 gt = buildTree besideFilledCells b c
                 turnsToThinkAhead = 1  -- drastic slow down at 3, fast at 1
                 move = getBestMove turnsToThinkAhead gt c

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

-- A generator that follows some rules?

-- All the generators generate much more cells as the game goes on
-- So, the AI slows down with every turn
-- currently 'evaluate' is of constant complexity to test the generators

-- Given a board will return a list of empty cells beside other cells
besideFilledCells :: Board -> Col -> [Position]
besideFilledCells bd c = removeDuplicates list_of_all
                         where list_of_all = foldr (++) [] lists  -- concatenate all lists
                               lists = [emptyNeighbors bd p | (p, _) <- pieces bd]
                               -- list of lists of empty neighbors for each piece

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
-- Given a board will return list of positions that are empty  -- very slow for large boards
emptyCells :: Board -> [Position]
emptyCells bd = filter (isEmptyCell bd) boardCells
                  where boardCells = [(x,y) | x <- [0 .. ((size bd) - 1)], y <- [0 .. ((size bd) - 1)] ]

-- Given a board and a position will check if given position is empty
isEmptyCell :: Board -> Position -> Bool
isEmptyCell bd cell = colOf bd cell == Nothing

emptyNeighbors :: Board -> Position -> [Position]
emptyNeighbors b pos = filter (isEmptyCell b) $ neighbors b pos

neighbors :: Board -> Position -> [Position]
neighbors b pos = filter (\ p -> outOfBounds b p == False) [next pos dir | dir <- [N ..]]

removeDuplicates :: [Position] -> [Position]
removeDuplicates [] = []
removeDuplicates (x:xs) | x `elem` xs = removeDuplicates xs
                        -- for every element, if it is present in the remaining elements, ignore it
                        | otherwise = x : removeDuplicates xs
