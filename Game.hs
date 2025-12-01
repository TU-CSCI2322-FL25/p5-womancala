module Game where

------------- Story One ---------------

type Index = Int
type Pit = (Index, Int) --(Index, Num of Marbles)
type Board = [Pit] -- Will only ever be 14 long
data Player = P1 | P2 deriving (Eq,Show)
type Turn = Player
data Winner = Win Player | Tie deriving (Eq,Show)
type Move = Index
type Game = (Turn, Board)

store :: Player -> Index
store P1 = 7
store P2 = 0

pits :: Player -> [Index]
pits P1 = [1..6]
pits P2 = [8..13]

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
-- Should only be run on a correct game board, will error out if game board is incorrect
checkWinner :: Game -> Maybe Winner
checkWinner (turn, board) 
    -- If one side is empty, game ends. Winner is determined by store total counts
    | isSideEmpty (pits P1) || isSideEmpty (pits P2) =
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
        p1Store = lookUpUnsafe (store P1) board
        p2Store = lookUpUnsafe (store P2) board

        -- Total of each players side pits
        p1SideTotal = sum [ numMarbles | (index,numMarbles) <- board, index `elem` (pits P1)]
        p2SideTotal = sum [ numMarbles | (index,numMarbles) <- board, index `elem` (pits P2)]

        -- If one side is empty, the other player gets all marbles on their side added to their store
        p1Total = if isSideEmpty (pits P2) then p1Store + p1SideTotal else p1Store
        p2Total = if isSideEmpty (pits P1) then p2Store + p2SideTotal else p2Store

-----------------------------------------

------------- Story Three ---------------
-- This function does not check if the game is over after a move or if a move is legal.
-- Only ever pass in a correct board, it will error if the board is invalid.

completeMove :: Game -> Move -> Maybe Game
completeMove game move = if isValidMove game move then Just (completeMoveUnsafe game move) else Nothing

completeMoveUnsafe :: Game -> Move -> Game
completeMoveUnsafe game@(turn, board) move 
        | landingIndex == 0 && turn == P2 = (P2, distributedBoard)
        | landingIndex == 7 && turn == P1 = (P1, distributedBoard)
        | amountAtIndex == 1 && landingIndex `elem` (pits turn) = 
            let opposite = landingIndex + 2*(7-landingIndex)
                oppositeMarbles = lookUpUnsafe opposite distributedBoard
                boardClearOpposite = changeValue opposite 0 distributedBoard
                boardClearSame = changeValue landingIndex 0 boardClearOpposite
                finalBoard = addToIndex (store turn) (oppositeMarbles+1) boardClearSame
            in if oppositeMarbles > 0 then (opponent turn, finalBoard) else (opponent turn, distributedBoard) 
        | otherwise = (opponent turn, distributedBoard)
    where   numMarbles = lookUpUnsafe move board
            distribute = distributeMarbles game move numMarbles
            distributedBoard = map snd distribute
            landingIndex = findLandingIndex distribute
            amountAtIndex = lookUpUnsafe landingIndex distributedBoard 

            distributeMarbles :: Game -> Index -> Int -> [(Bool,Pit)] 
            distributeMarbles (turn, board) startIndex numMarbles = map (updatePit turn startIndex numMarbles) board

            findLandingIndex :: [(Bool,Pit)] -> Index
            findLandingIndex [] = error "No Landing Index Reported"
            findLandingIndex ((isLandingIndex, (index,marbles)):xs) = if isLandingIndex then index else findLandingIndex xs

            indexDistance :: Index -> Index -> Int
            indexDistance a b 
                | a <= b     = b - a
                | turn == P1 = 13+(b-a)
                | otherwise  = (14-a)+b-(b `div` 7)

            updatePit :: Turn -> Index -> Int -> Pit -> (Bool, Pit) --Turn, starting index, nummarbles, base pit, new pit, returns the newpit and if its landing
            updatePit turn startIndex numMarbles oldPit@(pitIndex, oldMarbles) 
                | startIndex == pitIndex            = (isLandingPit, (startIndex, numMarbles `div` 13))
                | pitIndex == store (opponent turn) = (isLandingPit, oldPit) 
                | otherwise                         = (isLandingPit, newPit)
                where dist = indexDistance startIndex pitIndex
                      distAdd = if dist <= (numMarbles `mod` 13) then 1 else 0
                      newPit =  (pitIndex, oldMarbles + (numMarbles `div` 13) + distAdd)
                      isLandingPit = dist == (numMarbles `mod` 13)
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

lookUpUnsafe :: (Eq a, Show  a, Num b) => a -> [(a,b)] -> b 
lookUpUnsafe key alist = case lookup key alist of
    Just value -> value
    Nothing    -> error $ (show key) ++ " is not a valid key." 

opponent :: Player -> Player
opponent P1 = P2
opponent P2 = P1

-----------------------------------------

------------- Story Four ---------------
--Creates the list of legal moves from a game state
--by checking if the pit is on the current player's side and not empty.
-- Only ever pass in a correct board, will not return intended results if incorrect board.
validMoves :: Game -> [Move]
validMoves (p, board) = 
    [index | pit@(index,marbles) <- board, validPit pit]
    where 
        validPit :: Pit -> Bool
        validPit (idx, num) = (idx `elem` pits p) && (num > 0)

--Extra: Checks if a move is valid given a game (Might be handy for error handling idk)
isValidMove :: Game -> Move -> Bool          
isValidMove game@(turn,board) move 
    | move == 0 = False
    | move == 7 = False
    | move `notElem` (pits turn) = False
    | lookUpUnsafe move board == 0 = False
    | otherwise = True

---------------------------------------