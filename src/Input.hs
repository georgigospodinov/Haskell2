module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.IO.Unsafe
import Data.Either.Unwrap

import Network.BSD
import System.IO
import Data.List
import Data.Bits
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

import Board
import AI
import Draw

import Debug.Trace

sendMove :: World -> (Int, Int) -> IO World
sendMove w (x, y) = do sendMsg (eliminate (Board.socket (net_data w))) ( posToString (x, y) )
                       let s = "sent " ++ posToString (x, y)
                       putStrLn s
                       return w

sendMsg :: Socket -> String -> IO Int
sendMsg sock msg = send sock $ C.pack msg

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) w = trace ("Mouse moved to: " ++ show (x,y)) w
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
    = trace ("handleInput to " ++ posToString (convx, convy)) (case makeMove (board w) (turn w) (convx, convy) of
        Just b ->  sendNet (w' b w) (convx, convy) -- updated world sendNet
        Nothing -> w  )-- same world
        where
            sendNet w (x, y) = if (useNet (net_data w)) && ((human $ board w) /= turn w)  -- if we use the network and it was my turn
                                then trace "sending move" unsafeDupablePerformIO $ sendMove w (x, y)           --    then send the move over the network
                               else trace "not sending move" w                                                     --  else just return the world
            w' b w = World b (other (turn w)) "" (aion w) False (blacks w) (whites w) (cell w) False (Just w) (net_data w)
            -- convert graphics coords to board coords
            convx = round $ (x-wwh (size $ board w)-sq_side)/sq_side
            convy = round $ (y-wwh (size $ board w)-sq_side)/sq_side

handleInput (EventKey (Char k) Down _ _) w
--    = trace ("Key " ++ show k ++ " down") w
    = case k of
        '.'     -> trace ("cmd: ") $ w {cmd=""}  -- clear command
        '\b'    -> trace ("cmd: " ++ del) $ w{cmd=del}
        '\SUB'  -> case prev w of  -- Ctrl-Z ("Undo")
                        Nothing -> w
                        Just w' -> case prev w' of
                                        Nothing -> w'
                                        Just w'' -> w''
        '\DC3'  -> trace "Game Saved!" $ unsafeDupablePerformIO $ save "save.dat" w
        '\f'    -> if isRight $ b then trace "Game Loaded!" $ w {board=fromRight b}
                   else trace (fromLeft b) w
                   where b = unsafeDupablePerformIO $ load "save.dat"
        _       -> trace ("cmd: " ++ app) $ w {cmd=app}
        where
            app = cmd w ++ [k]  -- append character
            del = init' $ cmd w  -- delete last character
            init' [] = []
            init' xs = init xs
handleInput (EventKey (Char k) Up _ _) w = trace ("Key " ++ show k ++ " up") w
handleInput e w = w
