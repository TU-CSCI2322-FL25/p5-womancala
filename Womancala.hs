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


------------- Story Five ---------------
--Pretty-print a game into a string, 
--with a function of type Game -> String.
--You should not override the Show typeclass.
--This is primarily for debugging purposes.
--Template:
-- Current turn: P_
-- -----------------------------------------
-- |    |    |    |    |    |    |    |    |
-- |    |-----------------------------|    |
-- |    |    |    |    |    |    |    |    |
-- -----------------------------------------

testBoard :: Board
testBoard = [(0,0),(1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)]
testBoardOne :: Board
testBoardOne = [(0,20),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,20),(8,8),(9,9),(10,10),(11,11),(12,12),(13,13)]

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