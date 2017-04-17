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
import Data.List.Utils
import Data.List
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
                     ai_level     :: Int,
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
initWorld = World initBoard initMenu Black "" False 2 Nothing Nothing True False False False
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
checkRules b = if (fair b) then (check3and3 b Black) && (check4and4 b Black 4) -- fair
               else (check3and3 b Black) && (check3and3 b White) && (check4and4 b Black 4) && (check4and4 b White 4) -- not fair

-- | returns true if board follows rules, false if rule is broken
check4and4 :: Board -> Col -> Int -> Bool
check4and4 b c i = num_rows_of_4 < 2
                where num_rows_of_4 = Prelude.sum ( all_rows_of_4 )
                      all_rows_of_4 = map (\p -> length (rows_of_4 p) ) (pieces b)
                      rows_of_4 piece = filter (\ x -> ( (fst x) == i ) ) ( map ( \d -> descend b c d (fst piece) ) [N, NE, E, SE] )

-- | returns true if board follows rules, false if rule is broken
check3and3 :: Board -> Col -> Bool
check3and3 b c = trace ("sum: " ++ show num_all_unblocked_rows) $ num_all_unblocked_rows < 2
                where num_all_unblocked_rows = Prelude.sum $ all_unblocked_rows
                      all_unblocked_rows = map (\p -> length (filtered_descend_list (fst p))) (pieces b)

                      filtered_descend_list :: Position -> [Street]
                      filtered_descend_list pos = printStreets $ filter filterNonBlocked (descend_list_per_piece pos)

                      descend_list_per_piece :: Position -> [Street]
                      descend_list_per_piece pos = map ( \d -> (pos, d, (descend b c d pos)) ) [N, NE, E, SE]            -- produces [Street]

                      filterNonBlocked :: Street -> Bool
                      filterNonBlocked (pos, d, (len, bl)) = (len == 3) && ((not bl) && (not $ snd (descendOpp pos d))) -- length==3 && ((not blocked E) || (not blocked W))

                      descendOpp :: Position -> Direction -> (Int, Bool)
                      descendOpp pos dir = trace ("start=" ++ show pos ++ ">" ++ show (findEnd b c dir pos pos) ) $descend b c (opp dir) (findEnd b c dir pos pos)

type Street = (Position, Direction, (Int, Bool))

printStreet :: Street -> Street
printStreet (p,d,(i,b)) = (trace ("street: " ++ show p ++ "," ++ show d ++ ",(" ++ show i ++ "," ++ show b ++ ")")) (p,d,(i,b))

printStreets :: [Street] -> [Street]
printStreets xs = map (\x -> printStreet x) xs

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
descend :: Board -> Col -> Direction -> Position -> (Int, Bool)  -- (length of line, if it is blocked by the opponent)
descend b c dir (x,y) = if outOfBounds b (x,y) then (0, True)                   -- (x,y) is out of bounds
                        else if colOf b (x,y) == Just (other c) then (0, True)  -- (x,y) has piece of other col
                        else if colOf b (x,y) == Just c then (l+1, bl)          -- (x,y) has my piece => continue
                        else (0, False)                                         -- empty
                        where (l, bl) = descend b c dir $ next (x,y) dir

-- | Finds the end of a row of pieces on the board
findEnd :: Board -> Col -> Direction
                        -> Position -- ^ last position
                        -> Position -- ^ current position to check
                        -> Position -- ^ end of row
findEnd b c dir (lx,ly) (x,y) = if outOfBounds b (x,y) then (lx,ly)
                        else if colOf b (x,y) == Just (other c) then (lx,ly)
                        else if colOf b (x,y) == Just c then (x, y)
                        else (lx,ly)  -- empty
                        where (nx, ny) = findEnd b c dir (x,y) $ next (x,y) dir

{- | Finds the longest row of pieces of 1 colour that are not blocked by enemy
     pieces or are a victory row. Descends from every square into all directions. -}
longest :: Board -> Col -> Int
longest b c = max' $ map fst $  -- take the maximum length
                filter (\ (l, bl) -> bl==False || l == target b || l < target b)
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
    put w   = put (board w, turn w, cmd w, ai_on w, ai_level w, recording w, prev w)
    get     = do board  <- get
                 turn   <- get
                 cmd    <- get
                 ai_on   <- get
                 ai_level <- get
                 recording  <- get
                 prev   <- get
                 return (World board initMenu turn cmd ai_on ai_level Nothing recording False False False False
                            (blacks initWorld) (whites initWorld) (cell initWorld) prev (net_data initWorld))


{- | Takes a world and, if playing against an AI will return world of player's previous move, if
playing another player it will return the world of the previous turn, if there is no previous world it will return
last possible world-}
undo :: World -> World
undo w = case prev w of                         -- Ctrl+z ("Undo")
                Nothing -> w                      -- if there is nothing to undo then keep same world
                Just w' -> if ai_on w then case prev w' of        -- attempt to revert 2 worlds
                                            Nothing -> w'
                                            Just w'' -> w''
                           else w'
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


----------------------------------------------------
-- ##   ##  ######  ##    #  #     #
-- # # # #  #       # #   #  #     #
-- #  #  #  ###     #  #  #  #     #
-- #     #  #       #   # #   #   #
-- #     #  ######  #    ##    ###
----------------------------------------------------

{- | Menu data containing MenuEntries and decorations (unclickable Gloss
     pictures like lines or titles) -}
data Menu = Menu { entries      :: [MenuEntry],
                   options      :: [MenuEntryOption],
                   decorations  :: [MenuEntryUnclickable]}

data MenuEntry = MenuEntry { menu_draw  :: Picture,
                             func       :: (World -> World),
                             location   :: Point
                           }



data MenuEntryOption = MenuEntryOption { option_draw       :: Picture,
                                         option_func       :: ((World) -> (World)),
                                         option_name       :: String,
                                         option_location   :: Point,
                                         var               :: Bool
                                       }

instance Eq MenuEntryOption where
  x == y = (option_location x == option_location y)

data MenuEntryUnclickable = MenuEntryUnclickable { decoration_draw :: Picture }

menuClick :: Point -> World -> Maybe World
menuClick (x, y) w = case isInBounds (x, y) (curr_menu w) of
                        Just mb -> Just ((func mb) w)
                        Nothing -> case isInBoundsOptions (x, y) (curr_menu w) of
                                      Just mo -> Just (handleOption w mo)
                                      Nothing -> Nothing

handleOption :: World -> MenuEntryOption -> World
handleOption w mo = ((option_func mo) w) {curr_menu = (replaceMenuOption (curr_menu w) (mo))}

flipMenuOption :: MenuEntryOption -> MenuEntryOption
flipMenuOption mo = mo {var = (not (var mo)),  option_draw = optionBar (option_location mo) (option_name mo) (not (var mo))}

replaceMenuOption :: Menu -> MenuEntryOption -> Menu
replaceMenuOption m mo = m {options = (replace ([mo]) ([flipMenuOption mo]) (options m)) }


singlePlayerOptions:: World -> World
singlePlayerOptions w = changeMenu w optionMenu

localPlayChoice:: World -> World
localPlayChoice w = w {is_menu = False}

--Single Player Option Handlers

{- | Sets game rules to include the rules of three and three and four and four -}
setThreeAndThree:: (World) -> (World)
setThreeAndThree w = w {board = (board w) {fair = (not (fair (board w)))}}
--
setAI:: (World) -> (World)
setAI w = w {ai_on = (not (ai_on w))}

setHardAI:: (World) -> (World)
setHardAI w = (w {ai_on = True, ai_level = 3}) {curr_menu = (replaceMenuOption (curr_menu w) (aiOption))}


{- | Given a world will return world ready to take an address and port for connecting to a host-}
multiPlayerChoiceLocal:: World -> World
multiPlayerChoiceLocal w = w {ai_on = False, is_menu = False}

{- | Given a world will return world ready to take an port for becoming a host-}
multiPlayerChoiceHost:: World -> World
multiPlayerChoiceHost w = (changeMenu w connectHostChoiceMenu) {taking_port = True}

{- | Given a world will return world ready to take an address and port for connecting to a host-}
multiPlayerChoiceJoin:: World -> World
multiPlayerChoiceJoin w = (changeMenu w connectJoinChoiceMenu) {taking_add = True}

-- Online multiplayer setup

{- | Given a world will prepare it for becoming a host-}
setToHost:: World -> World
setToHost w = w {to_network = True, is_menu = False, net_data = (net_data w) { useNet = True , isServ = True}, board = b { human = Black }}
                  where b = board w

{- | Given a world will prepare it for connecting to a host-}
setToJoin:: World -> World
setToJoin w = w {to_network = True, is_menu = False, net_data = (net_data w) { useNet = True , isServ = False}, board = b { human = White }}
                  where b = board w

-- Taking input at menu

{- | Given a World and a String representing a port will return given world prepared
for hosting networking -}
takePort:: World -> String -> World
takePort w str = w { taking_add = False, net_data = (initNet_Data {addr= (str)}) }

{- | Take World and string as representative of ip address and port, returns world if
prepared for connecting networking if network/port is valid else returns nothing-}
takeAdd:: World -> String -> Maybe World
takeAdd w str = let split = splitOn ":" str in
                  if (length split == 2) then
                    Just w { taking_add = False, net_data = (initNet_Data {addr= (head split), port = (last split)}) }
                  else Nothing


-- Submitting input at menu

{- | Takes World where command is representative of ip address and port, returns world if
prepared for networking if network/port is valid else returns current world-}
submitAddressAndPort:: World -> World
submitAddressAndPort w = case takeAdd w (cmd w) of
                          Just w' -> setToJoin $ w'{cmd=""}
                          Nothing -> w

{- | Takes current command stored in the world as a port, returns world with port
variable and other variables set for networking -}
submitPort:: World -> World
submitPort w = setToHost $ (takePort w (cmd w))


{- | Resets world to initial world, keeps images for pieces and board -}
resetWorld:: World -> World
resetWorld w = World initBoard initMenu Black "" False 2 Nothing Nothing True False False False (blacks w) (whites w) (cell w) Nothing initNet_Data

-- List of possible menus
initMenu = Menu [localEntry, multiPlayerHostEntry, multiPlayerJoinEntry] [] [mainMenuTitle]
optionMenu = Menu [localPlayBegin, cancelEntry] [fairOption, aiOption, aiHardOption] [localOptionsTitle]
connectJoinChoiceMenu = Menu [cancelEntry, submitTextEntry] [] [getPortTextDecoration]
connectHostChoiceMenu = Menu [cancelEntry, submitTextEntry2] [] [getPortTextDecoration2]

--Main Menu Entries

mainMenuTitle = MenuEntryUnclickable (titleBar (0, 100) "GOMOKU")

localEntry = MenuEntry (menuBar (0,50) "Solo/Local Play") singlePlayerOptions (0,50)
multiPlayerHostEntry = MenuEntry (menuBar (0,-10) "Multiplayer - Host") multiPlayerChoiceHost (0,-10)
multiPlayerJoinEntry = MenuEntry (menuBar (0,-40) "Multiplayer - Join") multiPlayerChoiceJoin (0,-40)

-- Getting Port and Address
submitTextEntry = MenuEntry (menuBar (0, -70) "Setup Connection!") submitAddressAndPort (0, -70)
getPortTextDecoration = MenuEntryUnclickable (menuBar (0, 20) "Address and Port?")

-- Getting Port
submitTextEntry2 = MenuEntry (menuBar (0, -70) "Setup Connection!") submitPort (0, -70)
getPortTextDecoration2 = MenuEntryUnclickable (menuBar (0, 20) "Port?")

-- Local Play Options
localOptionsTitle = MenuEntryUnclickable (titleBar (0, 100) "OPTIONS")

localPlayBegin = MenuEntry (menuBar (0, -70) "Play!") localPlayChoice (0, -70)
fairOption = MenuEntryOption (optionBar (0, 50) "Handicap P1" False) setThreeAndThree "Handicap P1" (0, 50) False
aiOption = MenuEntryOption (optionBar (0, 20) "AI On" False) setAI "AI On" (0, 20) False
aiHardOption = MenuEntryOption (optionBar (0, -10) "AI - Hard Mode" False) setHardAI "AI - Hard Mode" (0, -10) False

-- General Purpose
cancelEntry = MenuEntry (menuBar(0, -100) "Main Menu") resetWorld (0, -100)

{- | Takes world and menu, returns world with current menu set to given menu-}
changeMenu :: World -> Menu -> World
changeMenu w m = w {curr_menu = m};


{- | MenuEntry data containing dimensions for drawing menu boxes and taking input-}
bar_side1 = 210::Float
bar_side2 = 20::Float
bar_text_scale = 0.008*bar_side2
bar_margin = 2::Float

title_side1 = 230::Float
title_side2 = 30::Float
title_text_scale = 0.008*title_side2

menu_box_R = 190::Float
menu_box_G = 200::Float
menu_box_B = 255::Float
menu_box_A = 100::Float

{- | Given a point and a menu will return menuEntry, in menu, point is in, if it is in one, otherwise returns nothing -}
isInBounds :: Point -> Menu -> Maybe MenuEntry
isInBounds (x, y) m = case (length $ filter (isInBar (x,y)) $ entries m) of
                        0 -> Nothing
                        _ -> Just (head (filter (isInBar (x, y)) $ entries m))

{- | Given a point and menu will return a MenuEntryOption if the given point is inside
the boundaries of any picture of the menu -}
isInBoundsOptions :: Point -> Menu -> Maybe MenuEntryOption
isInBoundsOptions (x, y) m = case (length $ filter (isInBarOption (x,y)) $ options m) of
                        0 -> Nothing
                        _ -> Just (head (filter (isInBarOption (x, y)) $ options m))

{- | Given a menu entry will return boolean representing if the point is in the menu entry's location in the gui -}
isInBar :: Point -> MenuEntry -> Bool
isInBar (x, y) b = ((inRangeX (x) (fst (location b))) && (inRangeY (y) (snd (location b))))

{- | Given a point and MenuEntryOption will return Boolean representing whether the point is in the picture for the MenuEntryOption-}
isInBarOption :: Point -> MenuEntryOption -> Bool
isInBarOption (x, y) b = ((inRangeX (x) (fst (option_location b))) && (inRangeY (y) (snd (option_location b))))

{- | Given two floats representing x positionswith one representing x position of point and other
representing x position of menuBar will return if the value is in the menu bar's x coordinates -}
inRangeX :: Float -> Float -> Bool
inRangeX x1 x2 = ((x1 < (x2 + bar_side1/2)) && (x1) > (x2 - bar_side1/2))

{- | Given two floats representing y positions with one representing x position of point and other
representing x position of menuBar will return if the value is in the menu bar's x coordinates -}
inRangeY :: Float -> Float -> Bool
inRangeY y1 y2 = ((y1 < (y2 + bar_side2/2)) && (y1) > (y2 - bar_side2/2))

{- | Given a point and a string generates a menu bar picture containing given text -}
menuBar :: Point -> String -> Picture
menuBar (x, y) str = Pictures [(menuBox (x, y)), (menuText (x, y) str)]

{- | Given a point will return  -}
menuBox :: Point -> Picture
menuBox (x, y) = translate x y $ Pictures [(Color (makeColor menu_box_R menu_box_G
                    menu_box_B menu_box_A) $ polygon (rectanglePath (bar_side1)
                    (bar_side2))), lineLoop (rectanglePath (bar_side1) (bar_side2))]

{- | Given a point and String will produce a picture of the text as would fit within
a text bar -}
menuText :: Point -> String -> Picture
menuText (x, y) str = (translate ((x - bar_side1/2) + bar_margin)
                              ((y - bar_side2/2) + bar_margin)
                              $ scale bar_text_scale bar_text_scale $ Text str)



{- | Given a point, text and a value representing an option location, name and value will produce a picture representing
an option and it's current value at that point -}
optionBar :: Point -> String -> Bool -> Picture
optionBar (x, y) str b = Pictures [menuBar (x, y) str, optionStatusDisplay (x, y) b]

{- | Given a point and Bool will return a box with a text value representing the boolean value
at given point -}
optionStatusDisplay :: Point -> Bool -> Picture
optionStatusDisplay (x, y) b = Pictures [optionStatusBox (x, y), (optionStatusText (x, y) b)]

{- | Given a point will produce a picture of an box at that point -}
optionStatusBox :: Point -> Picture
optionStatusBox (x, y) = translate (x + bar_side1/2 + bar_side2/2) y $ Pictures [(Color (makeColor menu_box_R menu_box_G
                    menu_box_B menu_box_A) $ polygon (rectanglePath (bar_side2*2)
                    (bar_side2))), lineLoop (rectanglePath (bar_side2*2) (bar_side2))]

{- | Given a point and a value will return a text picture of that given value as on or off (True or False)-}
optionStatusText :: Point -> Bool -> Picture
optionStatusText (x, y) b = (translate ((x - bar_side2) + bar_margin + bar_side1/2 + bar_side2/2)
                              ((y - bar_side2/2) + bar_margin)
                              $ scale bar_text_scale bar_text_scale $ Text (if b then"On" else "Off"))



{- | Given a point and a string generates a title bar picture containing given text -}
titleBar :: Point -> String -> Picture
titleBar (x, y) str = Pictures [(titleBox (x, y)), (titleText (x, y) str)]

{- | Given a point will return  -}
titleBox :: Point -> Picture
titleBox (x, y) = translate x y $ Pictures [(Color (makeColor menu_box_R menu_box_G
                    menu_box_B menu_box_A) $ polygon (rectanglePath (title_side1)
                    (title_side2))), lineLoop (rectanglePath (title_side1) (title_side2))]

{- | Given a point and String will produce a picture of the text as would fit within
a text bar -}
titleText :: Point -> String -> Picture
titleText (x, y) str = (translate ((x - title_side1/2) + bar_margin + title_side1/4)
                              ((y - title_side2/2) + bar_margin)
                              $ scale title_text_scale title_text_scale $ Text str)
