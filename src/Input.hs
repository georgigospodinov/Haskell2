module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.IO.Unsafe
import Data.Either.Unwrap
import Debug.Trace

import GameWorld
import Network
import Recording
import AI

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
          Just w' -> if(to_network w') then (unsafeSetupNetworking (w' {to_network = False}))
                     else w'
          Nothing -> w
      else case makeMove (board w) (turn w) (convx, convy) of  -- try to make the move
          -- valid move =>   return updated world and send over network
          Just b -> wrmv w ((convx, convy), turn w) $ sendNet (w' b w) (convx, convy)
          Nothing -> w  -- invalid move => return same world
          where
                                 -- if we use the network and it was previously my turn
              sendNet w (x, y) = if (useNet (net_data w)) && ((human $ board w) /= turn w)
                                   -- then send the move over the network
                                  then unsafeDupablePerformIO $ sendMove w (x, y)
                                 else w  --  else just return the world
              w' b w = World b initMenu (other (turn w)) "" (ai_on w) Nothing (recording w) False False False False (blacks w) (whites w) (cell w) (Just w) (net_data w)
                                                                                            -- updates the world, switching the turn and using the new board
              convx = round $ (x-wwh (size $ board w)-sq_side)/sq_side  -- convert graphics x coords to board coords
              convy = round $ (y-wwh (size $ board w)-sq_side)/sq_side  -- convert graphics y coords to board coords

handleInput (EventKey (Char k) Down _ _) w
    = case k of
        '~'     -> trace ("INFO - cmd: ") $ command w
        '\b'    -> trace ("INFO - cmd: " ++ del) $ w{cmd=del}
        '\SUB'  -> case prev w of                         -- Ctrl+z ("Undo")
                        Nothing -> w                      -- if there is nothing to undo then keep same world
                        Just w' -> case prev w' of        -- attempt to revert 2 worlds
                                        Nothing -> w'
                                        Just w'' -> w''
        '\DC3'  -> trace "INFO - Saving Game in 'save.dat'" $ unsafeDupablePerformIO $ save "save.dat" w
                                                          -- Ctrl+s saves game
        '\f'    -> unsafeDupablePerformIO $ load w "save.dat"
        _       -> if(taking_add w) then trace ("INFO - Taking Address + Port: " ++ app) $ w {cmd=app}
                   else if (taking_port w) then trace ("INFO - Taking Port: " ++ app) $ w {cmd=app}
                   else trace ("INFO - unrecognised cmd: " ++ app) $ w {cmd=app}
        where
            app = cmd w ++ [k]                            -- append character
            del = init' $ cmd w                           -- delete last character
            init' [] = []                                 -- get safe first item of a list
            init' xs = init xs
handleInput e w = w

-- | hint command
command :: World -> World
command w = if (taking_add w) then
              case takeAdd w (cmd w) of
                Just w'' -> unsafeSetupNetworking $ setToHost $ w''{cmd=""}
                Nothing -> w'
            else if cmd w == "hint" then hint w'
            -- recognise other commands
            else w'
            where w' = w{cmd=""}  -- clear command
