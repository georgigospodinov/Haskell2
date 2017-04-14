module Recording where

--import Board
{-
Format:
<size>\<color><row><column>...
Example:
08adeo
Means Board Size 8  (thus max board size is 99)
BLACK plays in row a (=0), column d (=3)
WHITE plays in row d (=4), column o (=14)

No need to save Black/White. Black goes first and they take turns.
No need to save the target as we can assume that the game is complete at end.
If not complete -> load from save.
-}

app :: String -> ()
app move = unsafeDupablePerformIO $ appendFile "recording.sgf" move

readAll :: String
readAll = unsafeDupablePerformIO $ readFile "recording.sgf"

encodeMove :: Position -> String
encodeMove (x,y) = [r, c]  -- Player, Row, Column
                   where  r = toEnum (x+97) :: Char
                          c = toEnum (y+97) :: Char

decodeMove :: Col -> String -> (Position, Col)
decodeMove color [r, c] = ((x, y), color)
                          where x = fromEnum r -97  -- may need :: Int
                                y = fromEnum c -97 -- :: Int

decode :: String -> Col -> [(Position, Col)]
decode [] _ = []
decode str c = (decodeMove c (take 2 str)) : (decode (drop 2 str) (other c))

decodeSize :: String -> Int
decodeSize [a, b] = (read a :: Int)*10 + (read b :: Int)

data Record = History { size :: Int,
                        moves :: [(Position, Col)],
                        moves_read :: Int  -- How many moves have been replayed
                      }
initRecord = History (decodeSize $ take 2 readAll) (decode (drop 2 readAll) Black) 0

nextmove :: Record -> ((Position, Col), Record)
nextmove r = (moves r !!moves_read, r{moves_read=moves_read r +1})  -- return the next move and increment the counter


-- build worlds until a victory is reached
-- use prev as 'next'
buildSequence :: Record -> World -> World
buildSequence r w = if won b then w
                    else w{prev = nextw}
                    where b = board w
                          nextw = w' {prev = buildSequence r' w'}
                          w' = w {board = case makeMove b c move of
                                                  Just b' -> b'
                                                  Nothing -> trace ("failed to replay:" ++ show c ++ show move) b
                                 }
                          ((move, c), r') = nextmove r

sequenceStart :: World -> World
sequenceStart w = buildSequence initRecord w

-- call updateWorld while decode length is not 0
createFile = undefined  -- also wipes previous contents