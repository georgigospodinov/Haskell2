import Graphics.Gloss

myPic = Pictures [  Color white (Line [(-250,0),(250,0)]),
                    Color yellow (Translate (-100) 100 (Text "Hello!"))
                 ]


main = do tiger <- loadBMP "img/tiger.bmp"
          display (InWindow "Hello" (500,500)(100,100))
                  blue
                  tiger
