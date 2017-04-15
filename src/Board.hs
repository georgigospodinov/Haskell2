{-# LANGUAGE DeriveGeneric #-}

module Board where

import Graphics.Gloss
import Data.Foldable
import Data.Serialize
import Data.ByteString (writeFile, readFile)
import GHC.Generics
import Network.Socket
import Data.Either.Unwrap

import System.IO.Unsafe

import Debug.Trace

data Col = Black | White
  deriving (Show, Eq, Generic)

instance Read Col where
    readsPrec d "black" = [(Black, "")]
    readsPrec d "Black" = [(Black, "")]
    readsPrec d "white" = [(White, "")]
    readsPrec d "White" = [(White, "")]

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- Constants
sq_border :: Color
sq_border = black
sq_side = 50::Float
bar_side1 = 210::Float
bar_side2 = 20::Float
bar_text_scale = 0.008*bar_side2
bar_margin = 2::Float

win_side1 = 140::Float
win_side2 = 25::Float
win_text_scale = 0.008*win_side2
win_margin = 2::Float

-- BS = Board Size
win_size :: Int -> Int
win_size bs = bs * (round sq_side::Int)
wwh bs = - fromIntegral (win_size bs) / 2  -- window width halved

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Net_Data = Net_Data { useNet :: Bool,          -- if the network should be used
                           isServ :: Bool,          -- if this instance of the game is the server or client
                           socket :: Maybe Socket,  -- connected socket to use when sending and recv msgs
                           addr   :: String,        -- the ip address to connect to
                           port   :: String         -- the port to use/connect to
                         }
  deriving (Show, Generic)

initNet_Data = Net_Data False False Nothing "127.0.0.1" "5234"

data Board = Board { size :: Int,
                     target :: Int,
                     human :: Col,
                     pieces :: [(Position, Col)],
                     won :: (Maybe Col),
                     fair :: Bool
                   }
  deriving (Show, Generic)

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 Black [] Nothing False

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
                     cmd :: String,
                     aion :: Bool,
                     replay :: Maybe String,
                     recording :: Maybe String,
                     is_menu :: Bool,
                     blacks :: Picture,
                     whites :: Picture,
                     cell :: Picture,
                     prev :: Maybe World,
                     net_data :: Net_Data
                   }
    deriving (Show, Generic)

pic :: World -> Col -> Picture
pic w Black = blacks w
pic w White = whites w


initWorld = World initBoard Black "" True Nothing Nothing True
            (Color black $ circleSolid (sq_side/2))
            (Color white $ circleSolid (sq_side/2))
            (Color sq_border $ Line
                [(x,y), (x,y+sq_side), (x+sq_side,y+sq_side), (x+sq_side,y), (x,y)])
            Nothing
            initNet_Data
              where (x,y) = (-sq_side/2,-sq_side/2)


outOfBounds :: Board -> Position -> Bool
outOfBounds b (x,y) = x < 0 || y < 0 || x >= size b -1 || y >= size b -1

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c (x,y) =  (if outOfBounds b (x, y) || invalid then Nothing
                      else if won b /= Nothing then Nothing  -- Do not accept new moves, once there is a winner.
                      else if colOf b (x,y) == Nothing then
                        Just b'{won = checkWon b'}  -- Update winner after pieces.
                      else Nothing)
                            where b' = b{pieces = pieces b++[((x,y),c)]}  -- Update pieces first.
                                  invalid = not (checkRules b')

eliminate :: Maybe a -> a
eliminate (Just a) = a

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon b = if longest b Black == target b then Just Black
             else if longest b White == target b then Just White
             else Nothing

-- returns true if ok, false if rule is broken
checkRules :: Board -> Bool
checkRules b = if (fair b) then (check3and3 b Black 3) && (check4and4 b Black 4)
               else (check3and3 b Black 3) && (check3and3 b White 3) && (check4and4 b Black 4) && (check4and4 b White 4)

-- returns true if ok, false if rule is broken
check4and4 :: Board -> Col -> Int -> Bool
check4and4 b c i = num_rows_of_4 < 2
                where num_rows_of_4 = Prelude.sum ( all_rows_of_4 )
                      all_rows_of_4 = map (\p -> length (rows_of_4 p) ) (pieces b)
                      rows_of_4 piece = filter (\ x -> ( (fst x) == i ) ) ( map ( \d -> descend b c d (fst piece) ) [N, NE, E, SE] )

-- returns true if ok, false if rule is broken
check3and3 :: Board -> Col -> Int -> Bool
check3and3 b c i = num_all_u_i_rows < 2
                where num_all_u_i_rows = Prelude.sum $ all_unblock_i_rows
                      all_unblock_i_rows = map (\p -> length (unblock_i_rows p) ) (pieces b)
                      unblock_i_rows piece = filter (\ x -> ( (fst x) == i ) && not (snd x) ) ( map ( \d -> descend b c d (fst piece) ) [N, NE, E, SE] )

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
data Direction = N | S | E | W | NE | SE | NW | SW
    deriving (Enum, Show)
next :: Position -> Direction -> Position
next (x,y) N = (x+1, y)
next (x,y) S = (x-1, y)
next (x,y) E = (x, y+1)
next (x,y) W = (x, y-1)
next (x,y) NE = (x+1, y+1)
next (x,y) NW = (x+1, y-1)
next (x,y) SE = (x-1, y+1)
next (x,y) SW = (x-1, y-1)

opposite :: Direction -> Direction
opposite N = S
opposite S = N
opposite W = E
opposite E = W
opposite NE = SW
opposite NW = SE
opposite SE = NW
opposite SW = NE
opp = opposite

colOf :: Board -> Position -> Maybe Col
colOf b pos = lookup pos (pieces b)

descend :: Board -> Col -> Direction -> Position -> (Int, Bool)  -- length, (blocked?)
descend b c dir (x,y) = if outOfBounds b (x,y) then (0, True)
                        else if colOf b (x,y) == Just (other c) then (0, True)
                        else if colOf b (x,y) == Just c then (l+1, bl)
                        else (0, False)  -- empty
                        where (l, bl) = descend b c dir $ next (x,y) dir


longest :: Board -> Col -> Int  -- the longest when descending from every square in all directions
longest b c = max' $ map fst $  -- take the maximum length
                -- take only lengths that: are not blocked OR are the target (victory)
                filter (\ (l, bl) -> bl==False || l == target b)
                    [if copp (x,y) dir /= Just c then descend b c dir (x,y)
                     -- The previous piece must not be of the same color
                     else (-target b -1, True)  -- if it is, ignore it
                     | dir <- [N ..], (x, y) <- map fst $ pieces b]
                where max' [] = 0
                      max' xs = Prelude.maximum xs
                      copp p d = colOf b $ next p (opp d)

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate b c = if won b == Just c then target b
               else if won b == Just (other c) then -target b
               else longest b c


instance Serialize Board
instance Serialize Col
-- Manually Serialize World because Picture serialization is not automatic, not needed.
instance Serialize World where
    put w   = put (board w, turn w, cmd w, aion w, recording w, prev w)
    get     = do board  <- get
                 turn   <- get
                 cmd    <- get
                 aion   <- get
                 recording  <- get
                --  blacks <- get unsafeDupablePerformIO $ loadBMP "src/img/black.bmp"
                --  whites <- get unsafeDupablePerformIO $ loadBMP "src/img/white.bmp"
                --  cell <- get unsafeDupablePerformIO $ loadBMP "src/img/gomoku-part.bmp"
                 prev   <- get
                 return (World board turn cmd aion Nothing recording False
                            (blacks initWorld) (whites initWorld) (cell initWorld) prev (net_data initWorld))

-- update to save world
save :: FilePath -> World -> IO World
save pth wd = do Data.ByteString.writeFile pth (encode wd)
                 putStrLn "OK - Game Saved."
                 return wd

-- if isRight w' then w' -- Ctrl+l loades game
-- else trace (fromLeft w') w
-- where w' = trace "INFO - Loading Game from 'save.dat'" $ unsafeDupablePerformIO $ load "save.dat"

-- update to load world and to accept a world.
-- Keep the images from the accepted world, change the other components to what was loaded
load:: World -> FilePath -> IO World
load w pth = do serWorld <- Data.ByteString.readFile pth
                bl <- loadBMP "src/img/black.bmp"
                wh <- loadBMP "src/img/white.bmp"
                cl <- loadBMP "src/img/gomoku-part.bmp"
                putStrLn "OK - Game Loaded."
                let w' = (decode serWorld) in
                  if isRight w' then
                    return (fromRight w') {blacks=bl, whites=wh, cell=cl}
                  else return w
