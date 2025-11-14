--Main file for the Womancala game

------------- Story One ---------------

type Index = Int
type Pit = (Index, Int) --(Index, Num of Marbles)
type Board = [Pit] -- Will only ever be 14 long
data Player = P1 | P2 deriving (Eq,Show)
type Turn = Player
data Winner = Win Player | Tie deriving (Eq,Show)
type Move = Index
type Game = (Turn, Board)

storeOne = store P1
storeTwo = store P2
sideOne = pits P1
sideTwo = pits P2

store :: Player -> Index
store P1 = 7
store P2 = 0

pits :: Player -> [Index]
pits P1 = [1..6]
pits P2 = [8..13]

initialBoard :: Board
initialBoard = [(0,0),(1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)]

initialState :: Game
initialState = (P1,initialBoard)

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
        isSideEmpty [] = True
        isSideEmpty (index:indexes) = 
            let (Just numMarbles) = lookup index board        --TODO Basically fromJust, should change in error handling
            in if numMarbles > 0 then False else isSideEmpty indexes

        -- lookUp value to return an Int
        lookupValue :: Index -> Int
        lookupValue i = case lookup i board of
                            Just v  -> v
                            Nothing -> 0

        -- Total of marbles in each players Store
        p1Store = lookupValue storeOne 
        p2Store = lookupValue storeTwo

        -- Total of each players side pits
        p1SideTotal = sum [ numMarbles | (index,numMarbles) <- board, index `elem` sideOne]
        --p1SideTotal = sum [lookupValue i | i <- sideOne]
        p2SideTotal = sum [lookupValue i | i <- sideTwo] --TODO repeat sidetotal for p1 on p2

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
            in (opponent turn, finalBoard) --TODO Opponent function
        | otherwise = (opponent turn, updatedBoard)
    where   (Just numMarbles) = lookup move board  --TODO bad pattern matching
            (distributeBoard, landingIndex, amountAtIndex) = distributeMarbles (turn, board) (nextIndex move turn) numMarbles 
            updatedBoard = changeValue move 0 distributeBoard

            isOnSide :: Index -> Turn -> Bool
            isOnSide index turn = (turn == P1 && index `elem` sideOne) || (turn == P2 && index `elem` sideTwo)

            distributeMarbles :: Game -> Index -> Int -> (Board,Index,Int) --Outputs the board after, the index landed on, and how many marbles in that pit
            distributeMarbles (turn, board) startIndex

            nextIndex :: Index -> Turn -> Index  --TODO pattern matching instead of guards
            nextIndex index turn | index == 13 && turn == P1 = 1
                                 | index == 6 && turn == P2 = 8
                                 | index == 13 = 0
                                 | otherwise = index + 1

opponent :: Player -> Player
opponent P1 = P2
opponent P2 = P1

indexDistance :: Turn -> Index -> Index -> Int
indexDistance turn a b 
    | a <= b     = b - a
    | turn == P1 = 13+(b-a)
    | otherwise  = (14-a)+b-(b `div` 7)
            
updatePit :: Turn -> Index -> Int -> Pit -> Pit --Turn, starting index, nummarbles, base pit, new pit
updatePit turn startIndex numMarbles oldPit@(pitIndex, oldMarbles) 
    | startIndex == pitIndex            = (startIndex, numMarbles `div` 13)
    | pitIndex == store (opponent turn) = oldPit 
    | otherwise                         = newPit
    where distAdd = if indexDistance turn startIndex pitIndex <= (numMarbles `mod` 13) then 1 else 0
          newPit = (pitIndex, oldMarbles + (numMarbles `div` 13) + distAdd)

addToIndex :: (Eq a, Num b) => a -> b -> [(a,b)] -> [(a,b)]  
addToIndex targetKey numToAdd [] = error "Key not found in associated list."
addToIndex targetKey numToAdd ((key,value):lst) = 
    if key == targetKey
    then (key,value+numToAdd):lst
    else (key,value):(addToIndex targetKey numToAdd lst) 

changeValue :: (Eq a, Num b) => a -> b -> [(a,b)] -> [(a,b)]  
changeValue targetKey newValue [] = error "Key not found in association list."
changeValue targetKey newValue ((key,value):lst) =  
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

prettyPrint :: Game -> String --TODO Could use unline
prettyPrint (turn,board) = "Current turn: "++(show turn)++"\n"++
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
            where (Just result) = lookup key list     --TODO Unsafe pattern matching
        --
        printTopLine :: String   --TODO could be simplier w/ concat & replicate
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

    
