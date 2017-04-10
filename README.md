# Haskell2
Gomoku Game for CS2006  
  
## Installation Instructions
cabal install MissingH  
cabal install gloss  
cabal configure  

## Running the Game
cabal run <options>  
  
### <options> include
fair=True    # Black Player has a disadvantage (3and3 and 4and4 rules apply only to them)  
fair=False   # All Players have a disadvantage (rules apply to all)  
  
size=9       # The size of the board (any integer greater than 0)
  
target=5     # The number of pieces required in a row to win the game (integer less than size)
  
human=Black  # Sets the human to use the colour Black  
human=White  # Sets the human to use the colour White  
  
ai=Black     # Sets up an AI opponent using the colour Black
ai=White     # Sets up an AI opponent using the colour White
   
computer=..  # Same as "ai=.."  
  
### Conflicting options 
When mutliple options were set with conflicting Colour assignments, only the last option is used. 
  
  
