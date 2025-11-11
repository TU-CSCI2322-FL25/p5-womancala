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
type Winner = Maybe Player
type Move = Index
type Game = (Turn, Board)
---------------------------------------

initialState :: Game
initialState = (P1,[(0,0),(1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)])


---------------------------------------

------------- Story Three ---------------
-- Completes the move without any error handling (yet)
-- If it doesn't work right let me (Sydney) know
-- This function does not check if the game is over after a move or if a move is legal
completeMove :: Game -> Move -> Game
completeMove (turn, board) move 
        | landingIndex == 0 && turn == P2 = (P2, updatedBoard)
        | landingIndex == 7 && turn == P1 = (P1, updatedBoard)
        | amountAtIndex == 1 && isOnSide landingIndex turn = 
            let opposite = landingIndex + 2*(7-landingIndex)
                (Just oppositeMarbles) = lookup opposite updatedBoard
                boardClearOpposite = changeValue opposite 0 updatedBoard
                boardClearSame = changeValue landingIndex 0 boardClearOpposite
                (Just storeValue) = if turn == P1 then lookup 7 boardClearSame else lookup 0 boardClearSame 
                finalBoard = 
                    if turn == P1 
                    then changeValue 7 (storeValue+oppositeMarbles+1) boardClearSame
                    else changeValue 0 (storeValue+oppositeMarbles+1) boardClearSame
            in (if turn == P1 then P2 else P1, finalBoard)
        | otherwise = ((if turn == P1 then P2 else P1), updatedBoard)
    where   (Just numMarbles) = lookup move board
            (distributeBoard, landingIndex, amountAtIndex) = distributeMarbles (turn, board) (nextIndex move turn) numMarbles 
            updatedBoard = changeValue move 0 distributeBoard

            distributeMarbles :: Game -> Index -> Int -> (Board,Index,Int) --Outputs the board after, the index landed on, and how many marbles in that pit
            distributeMarbles (turn, board) index 1 = (addOneToIndex index board,index,numMarbles+1)
                where (Just numMarbles) = lookup index board  
            distributeMarbles (turn, board) index numMarbles = 
                distributeMarbles (turn, addOneToIndex index board) (nextIndex index turn) (numMarbles-1)

            nextIndex :: Index -> Turn -> Index 
            nextIndex index turn | index == 13 && turn == P1 = 1
                                | index == 6 && turn == P2 = 8
                                | index == 13 = 0
                                | otherwise = index + 1

            addOneToIndex :: (Eq a, Num b) => a -> [(a,b)] -> [(a,b)]
            addOneToIndex targetKey ((key,value):lst) = 
                if key == targetKey
                then (key,value+1):lst
                else (key,value):(addOneToIndex targetKey lst) 

            isOnSide :: Index -> Turn -> Bool
            isOnSide index turn = (turn == P1 && index `elem` sideOne) || (turn == P2 && index `elem` sideTwo)

            changeValue :: (Eq a, Num b) => a -> b -> [(a,b)] -> [(a,b)]
            changeValue targetKey newValue ((key,value):lst) = 
                if key == targetKey
                then (key, newValue):lst
                else (key, value):(changeValue targetKey newValue lst) 
        
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
prettyPrint (turn,board) = "Current turn: "++(printPlayer turn)++"\n"++
                            printTopLine ++"\n"++
                            "\x2551   "++(prettyPrintSide board (reverse sideTwo))++"\n"++
                            (prettyPrintMiddle board)++"\n"++
                            "\x2551   "++(prettyPrintSide board sideOne)++"\n"++
                            printBottomLine ++ "\n"
  where --
        prettyPrintSide :: Board -> [Index]-> String --Will print a side minus the first "|"
        prettyPrintSide board [] = " \x2502    \x2551"
        prettyPrintSide board (index:indexes) = " \x2502 "++(spacedLookup index board) ++ prettyPrintSide board indexes
        --
        prettyPrintMiddle :: Board -> String
        prettyPrintMiddle board = "\x2551 " ++
                                  (spacedLookup storeTwo board) ++
                                  printMiddleLine++
                                  (spacedLookup storeOne board) ++
                                  " \x2551"
        --
        spacedLookup :: Int -> Board -> String
        spacedLookup key list = if result>9 then show result else " "++(show result)
            where (Just result) = lookup key list
        --
        printPlayer :: Player -> String
        printPlayer P1 = "P1" 
        printPlayer P2 = "P2"
        --
        printTopLine :: String
        printTopLine = "\x2554"++(take 4 (repeat '\x2550'))++(aux ("\x2564"++(take 4 (repeat '\x2550'))) 7)++"\x2557"
            where aux string 1 = string
                  aux string num = string++(aux string (num-1))
        --
        printBottomLine :: String
        printBottomLine = "\x255A"++(take 4 (repeat '\x2550'))++(aux ("\x2567"++(take 4 (repeat '\x2550'))) 7)++"\x255D"
            where aux string 1 = string
                  aux string num = string++(aux string (num-1))
        --
        printMiddleLine :: String
        printMiddleLine = " \x251C"++(take 4 (repeat '\x2500'))++(aux ("\x253C"++(take 4 (repeat '\x2500'))) 5)++"\x2524 "
            where aux string 1 = string
                  aux string num = string++(aux string (num-1))

----------------------------------------
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
