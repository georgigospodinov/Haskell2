{-# LANGUAGE DeriveGeneric #-}

module GameWorld where


import Graphics.Gloss
import Data.Foldable
import Data.Serialize
import Data.ByteString (writeFile, readFile)
import GHC.Generics
import Network.Socket
import Data.Either.Unwrap
import Data.List.Split

import System.IO.Unsafe

import Debug.Trace

-- | Colour data - either Black or White
data Col = Black | White
  deriving (Show, Eq, Generic)

-- | Read instance for Col accepts lower or uppercase strings ad returns Col
instance Read Col where
    readsPrec d "black" = [(Black, "")]
    readsPrec d "Black" = [(Black, "")]
    readsPrec d "white" = [(White, "")]
    readsPrec d "White" = [(White, "")]

-- | Swaps a Col and returns the opposite colour of the one given
other :: Col -> Col
other Black = White
other White = Black

-- | Position data type consisting of 2 Integers (Row, Column)
type Position = (Int, Int)

-- | Constants like sizes, line-colours, margins, etc
sq_border = black::Color
sq_side   = 50::Float

win_side1  = 140::Float
win_side2  = 25::Float
win_text_scale = 0.008*win_side2
win_margin = 2::Float
win_size :: Int -> Int
win_size bs = bs * (round sq_side::Int)
wwh bs     = - fromIntegral (win_size bs) / 2  -- window width halved

{- | Record for Network data stored inside the world record. -}
data Net_Data = Net_Data { useNet :: Bool,          -- if the network should be used
                           isServ :: Bool,          -- if this instance of the game is the server or client
                           socket :: Maybe Socket,  -- connected socket to use when sending and recv msgs
                           addr   :: String,        -- the ip address to connect to
                           port   :: String         -- the port to use/connect to
                         }
  deriving (Show, Generic)

-- | default data (addr of localhost)
initNet_Data = Net_Data False False Nothing "127.0.0.1" "5234"

{- | A Board record containing the board size (a board is a square grid,
     n * n), the number of pieces in a row required to win, and a list
     of pairs of position and the colour at that position.  -}
data Board = Board { size   :: Int,               -- the size of the board
                     target :: Int,               -- the target required to win
                     human  :: Col,               -- the colour of the human player
                     pieces :: [(Position, Col)], -- the filled cells (cell position and colour)
                     won    :: (Maybe Col),       -- Nothing if noone has won, Col if a player has won
                     fair   :: Bool               -- if the game should be "fair" according to rules in specification
                                                  --    (fair = only Black player has 3and3 and 4and4 rules; unfair = both players have rules)
                   }
  deriving (Show, Generic)

-- | Default board is 6x6, target is 3 in a row, no initial pieces and not fair
initBoard = Board 9 5 Black [] Nothing False

{- | Overall state is the board and whose turn it is, plus any further
     information about the world (this may later include, for example, player
     names, timers, information about rule variants, etc) -}
data World = World { board        :: Board,         -- the board
                     curr_menu    :: Menu,
                     turn         :: Col,           -- the colour of the player to play now
                     cmd          :: String,        -- the last command
                     ai_on        :: Bool,          -- if the ai is used
                     replay       :: Maybe String,  -- if the game is a replay of a recorded game - filename of rec file
                     recording    :: Maybe String,  -- if the game should be recorded - filename or rec file
                     is_menu      :: Bool,          -- if the menu should be displayed
                     to_network   :: Bool,          -- if the game has to set up networking
                     taking_add   :: Bool,          -- if the game is taking the address and port for networking
                     taking_port  :: Bool,          -- if the game is taking the port for networking
                     blacks       :: Picture,       -- the Gloss Picture representing a black stone
                     whites       :: Picture,       -- the Gloss Picture representing a white stone
                     cell         :: Picture,       -- the Gloss Picture representing a cell in board
                     prev         :: Maybe World,   -- the last world state (for undo action)
                     net_data     :: Net_Data       -- the network data to use
                   }
    deriving (Generic)

-- | Function returns the Gloss Picture for blacks or whites from the workd
pic :: World -> Col -> Picture
pic w Black = blacks w
pic w White = whites w

-- | Defualt initial world using initBoard and initNet_Data
initWorld = World initBoard initMenu Black "" True Nothing Nothing True False False False
            (Color black $ circleSolid (sq_side/2)) -- black circle (bmp pics loaded in main method replacing these)
            (Color white $ circleSolid (sq_side/2)) -- white circle
            (Color sq_border $ Line                 -- black box
                [(x,y), (x,y+sq_side), (x+sq_side,y+sq_side), (x+sq_side,y), (x,y)])
            Nothing
            initNet_Data
              where (x,y) = (-sq_side/2,-sq_side/2)

-- | checks if a position is out of bounds of the board
outOfBounds :: Board -> Position -> Bool
outOfBounds b (x,y) = x < 0 || y < 0 || x >= size b -1 || y >= size b -1

{- | Play a move on the board; return 'Nothing' if the move is invalid
     (e.g. outside the range of the board, or there is a piece already there
     or the move breaks the rules) -}
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c (x,y) =  if outOfBounds b (x, y) || invalid then Nothing -- out of bounds
                      else if won b /= Nothing then Nothing   -- Do not accept new moves, once there is a winner.
                      else if colOf b (x,y) == Nothing then   -- if the cell is empty
                        Just b'{won = checkWon b'}            -- then Update winner after pieces and return board
                      else Nothing
                            where b' = b{pieces = pieces b++[((x,y),c)]}  -- board with updated pieces.
                                  invalid = not (checkRules b')           -- checks rules

-- | Function eliminates a Maybe
eliminate :: Maybe a -> a
eliminate (Just a) = a

{- | Check whether the board is in a winning state for either player.
     Returns 'Nothing' if neither player has won yet
     Returns 'Just c' if the player 'c' has won -}
checkWon :: Board -> Maybe Col
checkWon b = if longest b Black == target b then Just Black
             else if longest b White == target b then Just White
             else Nothing

-- | returns true if board follows rules, false if rule is broken
checkRules :: Board -> Bool
checkRules b = if (fair b) then (check3and3 b Black 3) && (check4and4 b Black 4) -- fair
               else (check3and3 b Black 3) && (check3and3 b White 3) && (check4and4 b Black 4) && (check4and4 b White 4) -- not fair

-- | returns true if board follows rules, false if rule is broken
check4and4 :: Board -> Col -> Int -> Bool
check4and4 b c i = num_rows_of_4 < 2
                where num_rows_of_4 = Prelude.sum ( all_rows_of_4 )
                      all_rows_of_4 = map (\p -> length (rows_of_4 p) ) (pieces b)
                      rows_of_4 piece = filter (\ x -> ( (fst x) == i ) ) ( map ( \d -> descend b c d (fst piece) ) [N, NE, E, SE] )

-- | returns true if board follows rules, false if rule is broken
check3and3 :: Board -> Col -> Int -> Bool
check3and3 b c i = num_all_u_i_rows < 2
                where num_all_u_i_rows = Prelude.sum $ all_unblock_i_rows
                      all_unblock_i_rows = map (\p -> length (unblock_i_rows p) ) (pieces b)
                      unblock_i_rows piece = filter (\ x -> ( (fst x) == i ) && not (snd x) ) ( map ( \d -> descend b c d (fst piece) ) [N, NE, E, SE] )

-- | Direction data record
data Direction = N | S | E | W | NE | SE | NW | SW
    deriving (Enum, Show)

-- | Funcion returns the next cell given a direction and starting cell
next :: Position -> Direction -> Position
next (x,y) N = (x+1, y)
next (x,y) S = (x-1, y)
next (x,y) E = (x, y+1)
next (x,y) W = (x, y-1)
next (x,y) NE = (x+1, y+1)
next (x,y) NW = (x+1, y-1)
next (x,y) SE = (x-1, y+1)
next (x,y) SW = (x-1, y-1)

-- | Flips the directions
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

-- | Retruns the colour of a position on the board, or Nothing if empty
colOf :: Board -> Position -> Maybe Col
colOf b pos = lookup pos (pieces b)

-- | Continues along the board in a direction along a line of pieces on the board
descend :: Board -> Col -> Direction -> Position -> (Int, Bool)  -- length of line, if it is blocked by the opponent
descend b c dir (x,y) = if outOfBounds b (x,y) then (0, True)
                        else if colOf b (x,y) == Just (other c) then (0, True)
                        else if colOf b (x,y) == Just c then (l+1, bl)
                        else (0, False)  -- empty
                        where (l, bl) = descend b c dir $ next (x,y) dir

{- | Finds the longest row of pieces of 1 colour that are not blocked by enemy
     pieces or are a victory row. Descends from every square into all directions. -}
longest :: Board -> Col -> Int
longest b c = max' $ map fst $  -- take the maximum length
                filter (\ (l, bl) -> bl==False || l == target b)
                    [if copp (x,y) dir /= Just c then descend b c dir (x,y)
                     -- The previous piece must not be of the same color
                     else (-target b -1, True)  -- if it is, ignore it
                     | dir <- [N ..], (x, y) <- map fst $ pieces b]
                where max' [] = 0
                      max' xs = Prelude.maximum xs
                      copp p d = colOf b $ next p (opp d)

{- | An evaluation function for a minimax search. Given a board and a colour
     returns an integer indicating how good the board is for that colour. -}
evaluate :: Board -> Col -> Int
evaluate b c = if won b == Just c then target b
               else if won b == Just (other c) then -target b
               else longest b c

-- SAVE / LOAD :

-- | Serialization instance to save/load for Board
instance Serialize Board
-- | Serialization instance to save/load for Col
instance Serialize Col
-- | Serialization instance to save/load for World
instance Serialize World where
    put w   = put (board w, turn w, cmd w, ai_on w, recording w, prev w)
    get     = do board  <- get
                 turn   <- get
                 cmd    <- get
                 ai_on   <- get
                 recording  <- get
                 prev   <- get
                 return (World board initMenu turn cmd ai_on Nothing recording False False False False
                            (blacks initWorld) (whites initWorld) (cell initWorld) prev (net_data initWorld))

-- | Saves the world to a given file path.
save :: FilePath -> World -> IO World
save pth wd = do Data.ByteString.writeFile pth (encode wd)
                 putStrLn "OK - Game Saved."
                 return wd

{- | Loads a world from a specified filepath.
     Keeps the images from the given world but changes the other components to
     what was loaded -}
load:: World -> FilePath -> IO World
load w pth = do serWorld <- Data.ByteString.readFile pth
                putStrLn "OK - Game Loaded."
                let w' = (decode serWorld) in
                  if isRight w' then
                    return (fromRight w') {blacks=(blacks w), whites=(whites w), cell=(cell w)}
                  else return w


----------------------------------------------------{
-- ##   ##  ######  ##    #  #     #
-- # # # #  #       # #   #  #     #
-- #  #  #  ###     #  #  #  #     #
-- #     #  #       #   # #   #   #
-- #     #  ######  #    ##    ###
----------------------------------------------------}

{- | Menu data containing MenuEntries and decorations (unclickable Gloss
     pictures like lines or titles) -}
data Menu = Menu { entries      :: [MenuEntry],
                   decorations  :: [MenuEntryUnclickable]}

data MenuEntry = MenuEntry { menu_draw  :: Picture,
                             func       :: (World -> World),
                             location   :: Point
                           }

data MenuEntryUnclickable = MenuEntryUnclickable { decoration_draw :: Picture }

menuClick :: Point -> World -> Maybe World
menuClick (x, y) w = case isInBounds (x, y) (curr_menu w) of
                        Just mb -> Just ((func mb) w)
                        Nothing -> Nothing


{- |  -}
singlePlayerChoice:: World -> World
singlePlayerChoice w = w {ai_on = True, is_menu = False}

{- |  -}
multiPlayerChoiceLocal:: World -> World
multiPlayerChoiceLocal w = w {ai_on = False, is_menu = False}

multiPlayerChoiceHost:: World -> World
multiPlayerChoiceHost w = (changeMenu w connectHostChoiceMenu) {taking_port = True}

multiPlayerChoiceJoin:: World -> World
multiPlayerChoiceJoin w = (changeMenu w connectJoinChoiceMenu) {taking_add = True}

-- Online multiplayer setup

setToHost:: World -> World
setToHost w = w {to_network = True, is_menu = False, net_data = (net_data w) { useNet = True , isServ = True}, board = b { human = Black }}
                  where b = board w

setToJoin:: World -> World
setToJoin w = w {to_network = True, is_menu = False, net_data = (net_data w) { useNet = True , isServ = False}, board = b { human = White }}
                  where b = board w


-- Taking input at menu

takePort:: World -> String -> World
takePort w str = w { taking_add = False, net_data = (initNet_Data {addr= (str)}) }

takeAdd:: World -> String -> Maybe World
takeAdd w str = let split = splitOn ":" str in
                  if (length split == 2) then
                    Just w { taking_add = False, net_data = (initNet_Data {addr= (head split), port = (last split)}) }
                  else Nothing


-- Submitting input at menu

submitAddressAndPort:: World -> World
submitAddressAndPort w = case takeAdd w (cmd w) of
                          Just w' -> setToJoin $ w'{cmd=""}
                          Nothing -> w

submitPort:: World -> World
submitPort w = setToHost $ (takePort w (cmd w))

{- |  -}
-- multiPlayerChoiceJoin:: World -> World
-- multiPlayerChoiceJoin w = w {to_network = True, is_menu = False, net_data = (net_data w) { useNet = True , isServ = False}, board = b { human = White }}
--                           where b = board w

{- | Resets world to initial world -}
resetWorld:: World -> World
resetWorld w = World initBoard initMenu Black "" True Nothing Nothing True False False False (blacks w) (whites w) (cell w) Nothing initNet_Data

-- List of possible menus
initMenu = Menu [singlePlayerEntry, multiPlayerLocalEntry, multiPlayerHostEntry, multiPlayerJoinEntry] []
connectJoinChoiceMenu = Menu [cancelEntry, submitTextEntry] [getPortTextDecoration]
connectHostChoiceMenu = Menu [cancelEntry, submitTextEntry2] [getPortTextDecoration2]

--Initial Menu
singlePlayerEntry = MenuEntry (menuBar (0,50) "Single Player - AI") singlePlayerChoice (0,50)
multiPlayerLocalEntry = MenuEntry (menuBar (0,20) "Multiplayer - Local") multiPlayerChoiceLocal (0,20)
multiPlayerHostEntry = MenuEntry (menuBar (0,-10) "Multiplayer - Host") multiPlayerChoiceHost (0,-10)
multiPlayerJoinEntry = MenuEntry (menuBar (0,-40) "Multiplayer - Join") multiPlayerChoiceJoin (0,-40)

-- Getting Port and Address
submitTextEntry = MenuEntry (menuBar (0, -50) "Setup Connection!") submitAddressAndPort (0, -50)
getPortTextDecoration = MenuEntryUnclickable (menuBar (0, 20) "Address and Port?")

-- Getting Port
submitTextEntry2 = MenuEntry (menuBar (0, -50) "Setup Connection!") submitPort (0, -50)
getPortTextDecoration2 = MenuEntryUnclickable (menuBar (0, 20) "Port?")

-- General Purpose
cancelEntry = MenuEntry (menuBar(0, -80) "Cancel") resetWorld (0, -80)


changeMenu :: World -> Menu -> World
changeMenu w m = w {curr_menu = m};


{- | MenuEntry data containing dimensions for drawing menu boxes and taking input-}
bar_side1 = 210::Float
bar_side2 = 20::Float
bar_text_scale = 0.008*bar_side2
bar_margin = 2::Float

{- |  -}
isInBounds :: Point -> Menu -> Maybe MenuEntry
isInBounds (x, y) m = case (length $ filter (isInBar (x,y)) $ entries m) of
                        0 -> Nothing
                        _ -> Just (head (filter (isInBar (x, y)) $ entries m))

{- |  -}
isInBar :: Point -> MenuEntry -> Bool
isInBar (x, y) b = ((inRangeX (x) (fst (location b))) && (inRangeY (y) (snd (location b))))

{- |  -}
inRangeX :: Float -> Float -> Bool
inRangeX x1 x2 = ((x1 < (x2 + bar_side1)) && (x1) > (x2 - bar_side1))

{- |  -}
inRangeY :: Float -> Float -> Bool
inRangeY y1 y2 = ((y1 < (y2 + bar_side2)) && (y1) > (y2 - bar_side2))

{- |  -}
menuBar :: Point -> String -> Picture
menuBar (x, y) str = Pictures [(menuBox (x, y)), (menuText (x, y) str)]

{- |  -}
menuBox :: Point -> Picture
menuBox (x, y) = translate x y $ Pictures [(Color (makeColor 190 200 255 100) $ polygon (rectanglePath (bar_side1) (bar_side2))), lineLoop (rectanglePath (bar_side1) (bar_side2))]

{- |  -}
menuText :: Point -> String -> Picture
menuText (x, y) str = (translate ((x - bar_side1/2) + bar_margin)
                              ((y - bar_side2/2) + bar_margin)
                              $ scale bar_text_scale bar_text_scale $ Text str)
