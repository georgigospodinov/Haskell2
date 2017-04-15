module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.IO.Unsafe
import Data.Either.Unwrap
import Debug.Trace

import Board
import Menu
import Network
import Recording

{- | Function updates the world state given an input event.
      Mouse-click     : converts the x,y coordinates to the corresponding row,col
                        and calls makeMove with the world, colour and coordinates.
                        If makeMove returns Nothing, the move was invalid and the
                        world is not updated. If a new world is returned then the
                        move was valid and it is sent over the network if
                        necessary. Also switches the turn.
      KeyboardiInpput : "Ctrl+z" undos the 2 last moves (own move and opponent)
                        "Ctrl+s" saves the game to a file 'game.dat'
                        "Ctrl+l" loades a game from a file 'game.dat'
                        _ other commands unimplimented. -}
handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
    = if is_menu w
        then case menuClick (x, y) w of
          Just w' -> w'
          Nothing -> w
      else case makeMove (board w) (turn w) (convx, convy) of  -- try to make the move
          Just b ->  sendNet (w' b w) (convx, convy)        -- valid move =>   return updated world and send over network
          Nothing -> w                                      -- invalid move => return same world
          where
              sendNet w (x, y) = if (useNet (net_data w)) && ((human $ board w) /= turn w)  -- if we use the network and it was previously my turn
                                  then unsafeDupablePerformIO $ sendMove w (x, y)           --    then send the move over the network
                                 else w                                                     --  else just return the world
              w' b w = World b (other (turn w)) "" (aion w) Nothing (recording w) False (blacks w) (whites w) (cell w) (Just w) (net_data w)
                                                                                            -- updates the world, switching the turn and using the new board
              convx = round $ (x-wwh (size $ board w)-sq_side)/sq_side                      -- convert graphics x coords to board coords
              convy = round $ (y-wwh (size $ board w)-sq_side)/sq_side                      -- convert graphics y coords to board coords

handleInput (EventKey (Char k) Down _ _) w
    = case k of
        '.'     -> trace ("cmd: ") $ w {cmd=""}           -- clear command
        '\b'    -> trace ("cmd: " ++ del) $ w{cmd=del}
        '\SUB'  -> case prev w of                         -- Ctrl+z ("Undo")
                        Nothing -> w                      -- if there is nothing to undo then keep same world
                        Just w' -> case prev w' of        -- attempt to revert 2 worlds
                                        Nothing -> w'
                                        Just w'' -> w''
        '\DC3'  -> trace "INFO - Saving Game in 'save.dat'" $ unsafeDupablePerformIO $ save "save.dat" w
                                                          -- Ctrl+s saves game
        '\f'    -> if isRight $ b then w {board=fromRight b} -- Ctrl+l loades game
                   else trace (fromLeft b) w
                   where b = trace "INFO - Loading Game from 'save.dat'" $ unsafeDupablePerformIO $ load "save.dat"
        _       -> trace ("cmd: " ++ app) $ w {cmd=app}
        where
            app = cmd w ++ [k]                            -- append character
            del = init' $ cmd w                           -- delete last character
            init' [] = []                                 -- get safe first item of a list
            init' xs = init xs
handleInput (EventKey (Char k) Up _ _) w = trace ("Key " ++ show k ++ " up") w
handleInput e w = w
