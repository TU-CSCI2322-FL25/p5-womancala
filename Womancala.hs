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

------------- Story Three ---------------

completeMove :: Game -> Move -> Game
completeMove (turn, board) move = 
    where
        distributeMarbles :: Game -> Index -> Int -> (Board,Index) --Outputs the board after and the Index it landed on
        distributeMarbles (turn, board) 
        
-----------------------------------------