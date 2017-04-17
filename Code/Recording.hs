module Recording where

import GameWorld

import Debug.Trace
import System.IO.Unsafe

{-
Format: SZ[05]\nAB[df]\nAW[gh]\n
SZ = size
follwed by two-digit number in square brackets
AB = add black
AW = add white
followed by two lowercase letters in square brackets that represent the row and the column
'a' stands for 1, 'b' stands for 2, ... 'z' stands for 26
(so size must be between 01 and 26)
This format is part of SGF and so another SGF parsing program can read this file.
-}

-- | Reads a file into a string.
readAll :: String -> String
readAll filepath = unsafeDupablePerformIO $ readFile filepath

-- | Encodes a given move into a string.
encodeMove :: Col -> Position -> String
encodeMove color (x,y) = ['A',p,'[',r, c,']','\n']  -- Player, Row, Column
                         where  r = toEnum (x+97) :: Char
                                c = toEnum (y+97) :: Char
                                p = if color == Black then 'B' else 'W'

-- | Decodes a String into a move (Position, Col).
decodeMove :: String -> (Position, Col)
decodeMove ('A':p:'[':r:c:']':'\n':[]) = ((x, y), color)
                                         where x = fromEnum r -97
                                               y = fromEnum c -97
                                               color = if p == 'B' then Black else White

{- | Decodes a string into a list of moves.
     Uses decodeMove until the whole string is parsed. -}
decode :: String -> [(Position, Col)]
decode [] = []
decode str = (decodeMove (take 7 str)) : (decode (drop 7 str))

-- | Function converts a board size (given as an int) into a string.
encodeSize :: Int -> String
encodeSize x = "SZ["++a++b++"]\n"
               where a = show $ x `div` 10
                     b = show $ x `mod` 10

-- | Function parses a string into a board size.
decodeSize :: String -> Int
decodeSize ('S':'Z':'[':a:b:']':'\n':[]) = (read [a] :: Int)*10 + (read [b] :: Int)

{- | Contains information about parsed recording.
     Size of the board, a list of the move-color tuples,
     and how many moves have been read from the list.-}
data Record = History { bsize :: Int,               -- board size
                        moves :: [(Position, Col)], -- List of moves
                        moves_read :: Int           -- Counter of how many moves have been replayed
                      }

{- | The initial record to start from.
     Given a path to file, decode the size and then the moves.
     Start with 0 mvoes read.-}
initRecord filepath = History
                        (decodeSize $ take 7 $ readAll filepath)
                        (decode (drop 7 $ readAll filepath))
                        0

-- | Returns the next move and increments the number of moves read.
nextmove :: Record -> ((Position, Col), Record)
nextmove r = ((moves r) !!(moves_read r), r{moves_read=moves_read r +1})


-- | Function builds worlds until a victory state is reached.
buildSequence :: Record -> World -> World
buildSequence r w = if won b /= Nothing then w
                    else w{prev = Just nextw}
                    where b = board w
                          nextw = w' {prev = Just $ buildSequence r' w'}
                          w'    = w {board = case makeMove b c move of
                                                  Just b' -> b'
                                                  Nothing -> trace ("ERROR - Failed to replay:" ++ show c ++ show move) b
                                    }
                          ((move, c), r') = nextmove r

-- | Starts the buildSequence to replay a game from a filepath.
sequenceStart :: World -> World
sequenceStart w = buildSequence (initRecord path) w
                  where path = case replay w of Just p' -> p'

-- | Writes a given move to the file specifed and returns IO (second arg)
writeMove :: String -> (Position, Col) -> t -> IO t
writeMove filepath (pos, c) arg = do ignore <- appendFile filepath $ encodeMove c pos
                                     return arg

-- | Wraps writeMove so IO is not spread.
wrmv :: World -> (Position, Col) -> t -> t
wrmv w m arg = case recording w of
                    Just path -> unsafeDupablePerformIO $ writeMove path m arg -- recording is switched on
                    Nothing   -> arg                                           -- no recording. Do Nothing

-- | Writes the size of a world to the file and returns IO(World)
writeSize :: String -> World -> IO World
writeSize filepath w = do ignore <- writeFile filepath $ encodeSize $ size $ board w
                          return w

-- | Wraps writeSize so IO is not spread.
wrsz :: World -> World
wrsz w = case recording w of
              Just path ->  unsafeDupablePerformIO $ writeSize path w -- recording is switched on
              Nothing   -> w                                          -- no recording. Do Nothing
