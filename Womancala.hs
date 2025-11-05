--Main file for the Womancala game

------------- Story One ---------------

type Pit = (Int, Int) --(Index, Num of Marbles)
pitOne = 0 --Scoring pit index for P1
pitTwo = 7 --Scoring pit index for P2
sideOne = [1..6] --Indexes for P1 pits
sideTwo = [8..13] -- Indexes for P2 pits
type Board = [Pit] -- Will only ever be 14 long
data Player = P1 | P2
turn :: Player
turn = P1
winner :: Maybe Player
winner = Nothing

type Move = Pit

---------------------------------------