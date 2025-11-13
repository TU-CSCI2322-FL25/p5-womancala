--Main file for the Womancala game

------------- Story One ---------------

type Index = Int
type Pit = (Index, Int) --(Index, Num of Marbles)
storeOne = 7 --Store index for P1
storeTwo = 0 --Store index for P2
sideOne = [1..6] --Indexes for P1 pits
sideTwo = [8..13] -- Indexes for P2 pits
type Board = [Pit] -- Will only ever be 14 long
data Player = P1 | P2 deriving Eq
type Turn = Player
data Winner = Win Player | Tie
type Move = Index
type Game = (Turn, Board)
---------------------------------------

initialState :: Game
initialState = (P1,[(0,0),(1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)])


------------- Story Four ---------------
validMoves :: Game -> [Move]
validMoves (p, board) = 
    map (fst) $ filter (validPit) board
    where 
        side = if p == P1 then sideOne else sideTwo
        validPit :: Pit -> Bool
        validPit (idx, num) = (idx `elem` side) && (num > 0)
