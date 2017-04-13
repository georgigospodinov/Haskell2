--import Graphics.Gloss
--
--myPic = Pictures [  Color white (Line [(-250,0),(250,0)]),
--                    Color yellow (Translate (-100) 100 (Text "Hello!"))
--                 ]
--
--
--main = do tiger <- loadBMP "img/tiger.bmp"
--          display (InWindow "Hello" (500,500)(100,100))
--                  blue
--                  tiger

import System.IO.Unsafe

app :: String -> ()
app s = unsafeDupablePerformIO $ appendFile "asd" s

readAll :: FilePath -> String
readAll path = unsafeDupablePerformIO $ readFile "asd"

main :: IO ()
main = do return $ app "howdy\n"
