module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Draw

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) w = trace ("Mouse moved to: " ++ show (x,y)) w
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
    = case makeMove (board w) (turn w) (convx, convy) of
        Just b -> World b (other (turn w)) "" (blacks w) (whites w) (cell w) False (Just w) -- updated world
        Nothing -> w  -- same world
        where  -- convert graphics coords to board coords
            convx = round $ (x-wwh (size $ board w)-sq_side)/sq_side
            convy = round $ (y-wwh (size $ board w)-sq_side)/sq_side
handleInput (EventKey (Char k) Down _ _) w
--    = trace ("Key " ++ show k ++ " down") w
    = case k of
        '.'     -> trace ("cmd: ") $ w {cmd=""}  -- clear command
        '\b'    -> trace ("cmd: " ++ del) $ w{cmd=del}
        '\SUB'  -> case prev w of
                        Nothing -> w
                        Just w' -> w'
--        '\DC3'  -> case save "save.dat" w of
--                    IO wd -> wd  -- sad monad
        _       -> trace ("cmd: " ++ app) $ w {cmd=app}
        where
            app = cmd w ++ [k]  -- append character
            del = init' $ cmd w  -- delete last character
            init' [] = []
            init' xs = init xs
handleInput (EventKey (Char k) Up _ _) w = trace ("Key " ++ show k ++ " up") w
handleInput e w = w
