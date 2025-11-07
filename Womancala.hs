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

------------- Story Three ---------------

completeMove :: Game -> Move -> Game
completeMove (turn, board) move = 
    where
        distributeMarbles :: Game -> Index -> Int -> (Board,Index) --Outputs the board after and the Index it landed on
        distributeMarbles (turn, board) 
        
-----------------------------------------

------------- Story Five ---------------
--To see correct indentation in ghci, put putStr before the prettyPrint call and pass in the game you want printed
--It should print well, if not, let me (Paige) know
--Template:
-- Current turn: P_
-- -----------------------------------------
-- |    |    |    |    |    |    |    |    |
-- |    |-----------------------------|    |
-- |    |    |    |    |    |    |    |    |
-- -----------------------------------------

prettyPrint :: Game -> String
prettyPrint (turn,board) = "Current turn: "++(printPlayer currentTurn)++"\n"++
                            "-----------------------------------------"++"\n"++
                            "|   "++(prettyPrintSide board (reverse sideTwo))++"\n"++
                            (prettyPrintMiddle board)++"\n"++
                            "|   "++(prettyPrintSide board sideOne)++"\n"++
                            "-----------------------------------------"++ "\n"
  where --
        prettyPrintSide :: Board -> [Index]-> String --Will print a side minus the first "|"
        prettyPrintSide board [] = " |    |"
        prettyPrintSide board (index:indexes) = " | "++(spacedLookup index board) ++ prettyPrintSide board indexes
        --
        prettyPrintMiddle :: Board -> String
        prettyPrintMiddle board = "| " ++
                                  (spacedLookup storeTwo board) ++
                                  " |-----------------------------| " ++
                                  (spacedLookup storeOne board) ++
                                  " |"
        --
        spacedLookup :: Int -> Board -> String
        spacedLookup key list = if result>9 then show result else " "++(show result)
            where (Just result) = lookup key list
        --
        printPlayer :: Player -> String
        printPlayer P1 = "P1" 
        printPlayer P2 = "P2"

----------------------------------------