import Data.Maybe
--Main file for the Womancala game

------------- Story One ---------------

type Index = Int
type Pit = (Index, Int) --(Index, Num of Marbles)
storeOne = 7 --Store index for P1
storeTwo = 0 --Store index for P2
sideOne = [1..6] --Indexes for P1 pits
sideTwo = [8..13] -- Indexes for P2 pits
type Board = [Pit] -- Will only ever be 14 long
data Player = P1 | P2 deriving (Eq,Show)
type Turn = Player
data Winner = Win Player | Tie deriving (Eq,Show)
type Move = Index
type Game = (Turn, Board)

initialState :: Game
initialState = (P1,[(0,0),(1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)])

---------------------------------------

------------- Story Two ----------------
-- Check who has won the game state, if anyone, with a function of type  Game -> Winner.

--Check if either player’s side is empty.
--If one side is empty:
--Add all marbles from the opposing side to that player’s store.
--Then compare the two stores to determine the winner, or a tie.
--If neither side is empty:
--The game should continue - return Nothing

-- I changed it to return Maybe Winner because if no side is empty there is no winner yet, so I return Nothing.  
-- Can also return NoWinner in this case, and reflect Winner above to be  Winner = Win Player | Tie | NoWinner so 
-- it can return a Winner instead of a Maybe Winner
checkWinner :: Game -> Maybe Winner
checkWinner (turn, board) 
    -- If one side is empty, game ends. Winner is determined by store total counts
    | isSideEmpty sideOne || isSideEmpty sideTwo =
        if p1Total > p2Total then Just (Win P1)
        else if p2Total > p1Total then Just (Win P2)
        else Just Tie
    -- Otherwise no sides are empty - no winner yet
    | otherwise = Nothing
    where
        -- Checks if pits on a side are all empty
        isSideEmpty :: [Index] -> Bool
        isSideEmpty side = null [pieces | (index, pieces) <- board, index `elem` side, pieces > 0]

        -- Total of marbles in each players Store 
        p1Store = fromMaybe 0 (lookup storeOne board)
        p2Store = fromMaybe 0 (lookup storeTwo board)

        -- Total of each players side pits
        p1SideTotal = sum [ numMarbles | (index,numMarbles) <- board, index `elem` sideOne]
        p2SideTotal = sum [ numMarbles | (index,numMarbles) <- board, index `elem` sideTwo]

        -- If one side is empty, the other player gets all marbles on their side added to their store
        p1Total = if isSideEmpty sideTwo then p1Store + p1SideTotal else p1Store
        p2Total = if isSideEmpty sideOne then p2Store + p2SideTotal else p2Store
    
-----------------------------------------

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
                (Just storeValue) = if turn == P1 then lookup 7 boardClearSame else lookup 0 boardClearSame --TODO Could be a seperate updateStore function 
                finalBoard = 
                    if turn == P1 
                    then changeValue 7 (storeValue+oppositeMarbles+1) boardClearSame
                    else changeValue 0 (storeValue+oppositeMarbles+1) boardClearSame
            in (if turn == P1 then P2 else P1, finalBoard) --TODO Opponent function
        | otherwise = ((if turn == P1 then P2 else P1), updatedBoard)
    where   (Just numMarbles) = lookup move board  --TODO bad pattern matching
            (distributeBoard, landingIndex, amountAtIndex) = distributeMarbles (turn, board) (nextIndex move turn) numMarbles 
            updatedBoard = changeValue move 0 distributeBoard

            distributeMarbles :: Game -> Index -> Int -> (Board,Index,Int) --Outputs the board after, the index landed on, and how many marbles in that pit
            distributeMarbles (turn, board) index 1 = (addOneToIndex index board,index,numMarbles+1) --TODO change implementation
                where (Just numMarbles) = lookup index board  
            distributeMarbles (turn, board) index numMarbles = 
                distributeMarbles (turn, addOneToIndex index board) (nextIndex index turn) (numMarbles-1)

            nextIndex :: Index -> Turn -> Index  --TODO pattern matching instead of guards
            nextIndex index turn | index == 13 && turn == P1 = 1
                                 | index == 6 && turn == P2 = 8
                                 | index == 13 = 0
                                 | otherwise = index + 1

            addOneToIndex :: (Eq a, Num b) => a -> [(a,b)] -> [(a,b)]  --Could be add k top level
            addOneToIndex targetKey ((key,value):lst) = --Add error case for empty list
                if key == targetKey
                then (key,value+1):lst
                else (key,value):(addOneToIndex targetKey lst) 

            isOnSide :: Index -> Turn -> Bool
            isOnSide index turn = (turn == P1 && index `elem` sideOne) || (turn == P2 && index `elem` sideTwo)

            changeValue :: (Eq a, Num b) => a -> b -> [(a,b)] -> [(a,b)]  --Could be top level
            changeValue targetKey newValue ((key,value):lst) =  --Add error case for empty list
                if key == targetKey
                then (key, newValue):lst
                else (key, value):(changeValue targetKey newValue lst) 
        
-----------------------------------------

------------- Story Four ---------------
--Creates the list of legal moves from a game state
--by checking if the pit is on the current player's side and not empty.
validMoves :: Game -> [Move]
validMoves (p, board) = 
    map (fst) $ filter validPit board    --TODO Might be able to be written in a list comprehension
    where 
        side = if p == P1 then sideOne else sideTwo
        validPit :: Pit -> Bool
        validPit (idx, num) = (idx `elem` side) && (num > 0)

--Extra: Checks if a move is valid given a game (Might be handy for error handling idk)
isValidMove :: Game -> Move -> Bool          --TODO Inefficient
isValidMove game@(p,board) move = move `elem` (validMoves game)

---------------------------------------

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
prettyPrint (turn,board) = "Current turn: "++(show turn)++"\n"++
                            (printLine "\x2554" '\x2550' "\x2564" 7 "\x2557")++"\n"++
                            "\x2551   "++(prettyPrintSide board (reverse sideTwo))++"\n"++
                            prettyPrintMiddle board++"\n"++
                            "\x2551   "++(prettyPrintSide board sideOne)++"\n"++
                            (printLine "\x255A" '\x2550' "\x2567" 7 "\x255D") ++ "\n"
  where --
        prettyPrintSide :: Board -> [Index]-> String --Will print a side minus the first "|"
        prettyPrintSide board [] = " \x2502    \x2551"
        prettyPrintSide board (index:indexes) = " \x2502 "++(spacedLookup index board) ++ prettyPrintSide board indexes
        --
        prettyPrintMiddle :: Board -> String
        prettyPrintMiddle board = "\x2551 " ++
                                  (spacedLookup storeTwo board) ++
                                  (printLine " \x251C" '\x2500' "\x253C" 5 "\x2524 ")++
                                  (spacedLookup storeOne board) ++
                                  " \x2551"
        --
        spacedLookup :: Int -> Board -> String
        spacedLookup key list = if result>9 then show result else " "++(show result)
            where result = case lookup key list of
                                Just value -> value
                                Nothing    -> error "There's a pit missing from the board?"
        --
        printLine :: String -> Char -> String -> Int ->String -> String
        printLine leftCorner middleLines middleTs repeatLength rightCorner =
            leftCorner++(take 4 (repeat middleLines))++(aux (middleTs++(take 4 (repeat middleLines))) repeatLength)++rightCorner
            where aux string 1 = string
                  aux string num = string++(aux string (num-1))

----------------------------------------
