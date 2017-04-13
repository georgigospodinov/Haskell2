module Recording where

--import Board
{-
Format:
<size>\<color><row><column>...
Example:
08addo
Means Board Size 8  (thus max board size is 99)
BLACK plays in row a (=0), column d (=3)
WHITE plays in row d (=3), column o (=14)

No need to save Black/White. Black goes first and they take turns.
No need to save the target as we can assume taht the game is complete.
If not complete -> load from save.
-}

app :: String -> ()
app move = unsafeDupablePerformIO $ appendFile "recording.sgf" move

readAll :: FilePath -> String
readAll path = unsafeDupablePerformIO $ readFile "recording.sgf"

encodeMove :: Col -> Position -> String
encodeMove color (x,y) = [color, r, c]  -- Player, Row, Column
                         where  r = toEnum (x+97) :: Char
                                c = toEnum (y+97) :: Char


decodeMove :: Col -> String -> (Col, Position)
decodeMove color [r, c] = (color, (x, y))
                          where    x = fromEnum r -97  -- may need :: Int
                                   y = fromEnum c -97 -- :: Int

decode :: String -> game??
decode [] = undefined
-- decode first two characters as

createFile = undefined  -- also wipes previous contents