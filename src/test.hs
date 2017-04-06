--import Graphics.Gloss
--
--myPic = Pictures [  Color white (Line [(-250,0),(250,0)]),
--                    Color yellow (Translate (-100) 100 (Text "Hello!"))
--                 ]
--
--
--main = do tiger <- loadBMP "img/tiger.bmp"
--          display (InWindow "Hello" (500,500)(100,100))
--                  blue
--                  tiger


import Board

import Debug.Trace

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

buildTree :: (Board -> Col -> [Position]) -> Board -> Col -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of
               Nothing -> mkNextStates xs
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs


recurse :: Int -> GameTree -> Col -> (Int, Position)
recurse 0 gt c = best [evalMove (game_board g) p c | (p, g) <- next_moves gt]
recurse d gt c = best options  --recurse d-1, next tree, same color

best :: [(Int, Position)] -> (Int, Position)
best [] = trace "NO MOVES???" (0, (0,0))
best [x] = trace "One move??" x
best ips = foldr comp (head best) (tail best)

comp :: (Int, Position) -> (Int, Position) -> (Int, Position)
comp (i1, p1) (i2, p2) = if i1 > i2 then (i1, p1)
                         else (i2, p2)

evalMove :: Board -> Position -> Col -> (Int, Position)
evalMove b p c = (evaluate b c, p)