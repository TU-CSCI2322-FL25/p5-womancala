--Main file for the Womancala game

------------- Story One ---------------

type Index = Int
type Pit = (Index, Int) --(Index, Num of Marbles)
storeOne = 7 --Store index for P1
storeTwo = 0 --Store index for P2
sideOne = [1..6] --Indexes for P1 pits
sideTwo = [8..13] -- Indexes for P2 pits
type Board = [Pit] -- Will only ever be 14 long
data Player = P1 | P2
type Turn = Player
currentTurn :: Turn
currentTurn = P1
type Winner = Maybe Player
type Move = Index
type Game = (Turn, Board)
---------------------------------------

------------- Story Two ---------------
-- Check who has won the game state, if anyone, with a function of type  Game -> Winner.
-- Without error handling for now (11-11)
checkWinner :: Game -> Winner
checkWinner (turn, board) =
let 
    pitP1 = lookup storeOne board
    pitP2 = lookup storeTwo board
in
    if pitP1 > pitP2 then return Just P1
    else if pit2 > pit1 then return Just P2
    else Nothing 
---------------------------------------