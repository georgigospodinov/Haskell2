module Recording where

import Board

import Debug.Trace
import System.IO.Unsafe

{-
--Format:
--<size>\<color><row><column>...
--Example:
--08adeo
--Means Board Size 8  (thus max board size is 99)
--BLACK plays in row a (=0), column d (=3)
--WHITE plays in row d (=4), column o (=14)
--
--No need to save Black/White. Black goes first and they take turns.
--No need to save the target as we can assume that the game is complete at end.
--If not complete -> load from save.

Edwin said that the recording should be somewhat readable by another program.
SZ[05]\nAB[df]\nAW[gh]\n
-}

readAll :: String -> String
readAll filepath = unsafeDupablePerformIO $ readFile filepath

encodeMove :: Col -> Position -> String
encodeMove color (x,y) = ['A',p,'[',r, c,']','\n']  -- Player, Row, Column
                         where  r = toEnum (x+97) :: Char
                                c = toEnum (y+97) :: Char
                                p = if color == Black then 'B' else 'W'

decodeMove :: String -> (Position, Col)
decodeMove ('A':p:'[':r:c:']':'\n':[]) = ((x, y), color)
                                         where x = fromEnum r -97  -- may need :: Int
                                               y = fromEnum c -97 -- :: Int
                                               color = if p == 'B' then Black else White

decode :: String -> [(Position, Col)]
decode [] = []
decode str = (decodeMove (take 7 str)) : (decode (drop 7 str))

encodeSize :: Int -> String
encodeSize x = "SZ["++a++b++"]\n"
               where a = show $ x `div` 10
                     b = show $ x `mod` 10

decodeSize :: String -> Int
decodeSize ('S':'Z':'[':a:b:']':'\n':[]) = (read [a] :: Int)*10 + (read [b] :: Int)

data Record = History { bsize :: Int,
                        moves :: [(Position, Col)],
                        moves_read :: Int  -- How many moves have been replayed
                      }
initRecord filepath = History
                        (decodeSize $ take 7 $ readAll filepath)
                        (decode (drop 7 $ readAll filepath))
                        0

-- return the next move and increment the counter
nextmove :: Record -> ((Position, Col), Record)
nextmove r = ((moves r) !!(moves_read r), r{moves_read=moves_read r +1})


-- build worlds until a victory is reached
-- use prev as 'next'
buildSequence :: Record -> World -> World
buildSequence r w = if won b /= Nothing then w
                    else w{prev = Just nextw}
                    where b = board w
                          nextw = w' {prev = Just $ buildSequence r' w'}
                          w' = w {board = case makeMove b c move of
                                                  Just b' -> b'
                                                  Nothing -> trace ("failed to replay:" ++ show c ++ show move) b
                                 }
                          ((move, c), r') = nextmove r

sequenceStart :: World -> World
sequenceStart w = buildSequence (initRecord path) w
                  where path = case replay w of Just p' -> p'

-- Writes the move to file and returns IO (second arg)
writeMove :: String -> (Position, Col) -> t -> IO t
writeMove filepath (pos, c) arg = do ignore <- appendFile filepath $ encodeMove c pos
                                     return arg

-- Writes the move to file and retuns its second argument.
wrmv :: World -> (Position, Col) -> t -> t
wrmv w m arg = case recording w of
                    Just path -> unsafeDupablePerformIO $ writeMove path m arg
                    Nothing -> arg

writeSize :: String -> World -> IO World
writeSize filepath w = do ignore <- writeFile filepath $ encodeSize $ size $ board w
                          return w

wrsz :: World -> World
wrsz w = case recording w of
              Just path ->  unsafeDupablePerformIO $ writeSize path w
              Nothing ->w