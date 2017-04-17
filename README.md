# Haskell2
Gomoku Game for CS2006  
  
## Installation Instructions
cabal install MissingH  gloss Network cereal either-unwrap bytestring-0.10.4.0 split Unique  
  
cabal configure  
  
## Running the Game
cabal run [options]  
  
### [options] include
<pre>
fair=True    # Black Player has a disadvantage (3and3 and 4and4 rules apply only to them)  
fair=False   # All Players have a disadvantage (rules apply to all)  
> Default: fair=False  
  
size=9       # The size of the board (any integer greater than 0)  
> Default: size=6  
  
target=5     # The number of pieces required in a row to win the game (integer less than size)  
> Default: target=3  
  
human=Black  # Sets the human to use the colour Black  
human=White  # Sets the human to use the colour White  
> Default: human=Black  
  
ai=Black     # Sets up an AI opponent using the colour Black  
ai=White     # Sets up an AI opponent using the colour White  
computer=..  # Same as "ai=.."  
> Default: ai=White  
  
server=True  # Using a network opponent and starting this game as the server  
server=False # Using a network opponent and starting this game as the client  
             # leave server option out to launch non-network game  
> Default: no network play
  
addr=192.168.1.1  # specify ip address to connect to (if "server=False")  
> Default: addr: 127.0.0.1  
  
port=4242    # specifiy port to use (relevant to server and client)  
> Default: port= 5234  
  
ai-on=True   # switches ai to on. can be used with human=White/..  
ai-on=False  # switches ai to off and allows 2 humans to play  
> Default: ai-on=True  

replay=~/Documents/go_sv # records the game to a given filename  
record=~/Documents/go_sv # replays a game from a given filename  
> Default: no record/replay  
  
</pre>
#### Conflicting options 
When mutliple options were set with conflicting Colour assignments, only the first option is used.  
  
### Ingame commands
<pre>
Just press/type the keyboard commands while playing the game.  
Ctrl+z		# undo move
Ctrl+s		# save game to save.dat in working directory
Ctrl+l		# load game from a file save.dat in working directory
hint.		# (typed) prints a hint to the console showing a good next move
.		# to clear invalid commands
  

</pre>
### Networking
#### Notes:
if in server-mode, player is black  
if in client-mode, player is white  
--> this means server always starts  
  
! IMPORTANT ! do not use in conjunction with "human=..." or "ai=..." yet.  
  
#### Steps to setup a network game:
 1. On computer A run "cabal run server=True" (also specifying the size and target)  
   a) Wait until the message "OK - Socket Ready" appears in the terminal. Then continue  
 2. On computer B run "cabal run server=False" (specifying the exact same size and target as on computer A)  
   a) Confirm the successfull connection using the messages printed in the terminals of both computers  
 3. Make a move on computer A  
 4. Wait until player A's move appears on player B's board, only then make B move
   ... continue gameplay as usual.  
  
Note: It is important that you do not click on the board when it is not your turn!  
      This will send your (accidental) click immediately after the when the other  
      endpoint starts listening for your move.  

  
