module Update where

import Board
import Network
import AI

import Debug.Trace
import System.IO.Unsafe

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w =   if replay w /= Nothing then
                        case prev w of
                              Just wrld -> wrld
                              Nothing -> trace "End of replay?" w
                    -- if we use the network and it is not my turn (w is world with switched turn)
                    else if (useNet (net_data w)) && ((human $ board w) /= turn w) then
                      rcvMove w (eliminate (Board.socket (net_data w))) -- then wait and recieve the next move
                    else if aion w then aiturn w
                    else w
