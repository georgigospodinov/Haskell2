module Menu where

import Graphics.Gloss
import Board
import Network

import Debug.Trace
-- :todo ALL
{- | Menu data containing MenuEntries and decorations (unclickable Gloss
     picutres like lines or titles) -}
data Menu = Menu { entries      :: [MenuEntry],
                   decorations  :: [MenuEntryUnclickable]}

{- | MenuEntry data containing ??? :todo -}
data MenuEntry = MenuEntry { menu_draw  :: Picture,
                             func       :: (World -> World),
                             location   :: Point
                           }
{- |  -}
data MenuEntryUnclickable = MenuEntryUnclickable { decoration_draw :: Picture }

{- |  -}
initMenu = Menu [singlePlayerEntry, multiPlayerLocalEntry, multiPlayerHostEntry, multiPlayerJoinEntry] []
hostMenu = Menu [] [findingPlayerEntry]

-- | initial Menu
currentMenu = initMenu

{- |  -}
singlePlayerEntry = MenuEntry (menuBar (0,50) "Single Player - AI") singlePlayerChoice (0,50)
multiPlayerLocalEntry = MenuEntry (menuBar (0,20) "Multiplayer - Local") multiPlayerChoiceLocal (0,20)
multiPlayerHostEntry = MenuEntry (menuBar (0,-10) "Multiplayer - Host") multiPlayerChoiceHost (0,-10)
multiPlayerJoinEntry = MenuEntry (menuBar (0,-40) "Multiplayer - Join") multiPlayerChoiceJoin (0,-40)

findingPlayerEntry = MenuEntryUnclickable (menuBar (0, -10) "Finding Player...")

{- |  -}
menuClick :: Point -> World -> Maybe World
menuClick (x, y) w = case isInBounds (x, y) currentMenu of
                        Just mb -> Just ((func mb) w)
                        Nothing -> Nothing

{- |  -}
isInBounds :: Point -> Menu -> Maybe MenuEntry
isInBounds (x, y) w = case (length $ filter (isInBar (x,y)) $ entries currentMenu) of
                        0 -> Nothing
                        _ -> Just (head (filter (isInBar (x, y)) $ entries currentMenu))

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

{- |  -}
singlePlayerChoice:: World -> World
singlePlayerChoice w = w {ai_on = True, is_menu = False}

{- |  -}
multiPlayerChoiceLocal:: World -> World
multiPlayerChoiceLocal w = w {ai_on = False, is_menu = False}

{- |  -}
multiPlayerChoiceHost:: World -> World
multiPlayerChoiceHost w = unsafeSetupNetworking (w { is_menu = False, net_data = (net_data w) { useNet = True , isServ = True}, board = b { human = Black }})
                             where b = board w
{- |  -}
multiPlayerChoiceJoin:: World -> World
multiPlayerChoiceJoin w = unsafeSetupNetworking  (w { is_menu = False, net_data = (net_data w) { useNet = True , isServ = False}, board = b { human = White }})
                          where b = board w
