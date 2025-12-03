module Womancala where

import Solving
import Game

import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

--Main file for the Womancala game

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
prettyPrint (turn,board@(sideOne,pitOne,sideTwo,pitTwo)) = "Current turn: "++(show turn)++"\n"++
                            (printLine "\x2554" '\x2550' "\x2564" 7 "\x2557")++"\n"++
                            "\x2551   "++(prettyPrintSide sideTwo (reverse (pits P2)))++"\n"++
                            prettyPrintMiddle [pitOne,pitTwo] ++"\n"++
                            "\x2551   "++(prettyPrintSide sideOne (pits P1))++"\n"++
                            (printLine "\x255A" '\x2550' "\x2567" 7 "\x255D") ++ "\n"
  where --
        prettyPrintSide :: [Pit] -> [Index]-> String --Will print a side minus the first "|"
        prettyPrintSide board [] = " \x2502    \x2551"
        prettyPrintSide board (index:indexes) = " \x2502 "++(spacedLookup index board) ++ prettyPrintSide board indexes
        --
        prettyPrintMiddle :: [Pit] -> String
        prettyPrintMiddle board = "\x2551 " ++
                                  (spacedLookup (store P2) board) ++
                                  (printLine " \x251C" '\x2500' "\x253C" 5 "\x2524 ")++
                                  (spacedLookup (store P1) board) ++
                                  " \x2551"
        --
        spacedLookup :: Int -> [Pit] -> String
        spacedLookup key list = if result>9 then show result else " "++(show result)
            where result = lookUpUnsafe key list
        --
        printLine :: String -> Char -> String -> Int ->String -> String
        printLine leftCorner middleLines middleTs repeatLength rightCorner =
            leftCorner++(take 4 (repeat middleLines))++(aux (middleTs++(take 4 (repeat middleLines))) repeatLength)++rightCorner
            where aux string 1 = string
                  aux string num = string++(aux string (num-1))

----------------------------------------

----------- Story Twelve ---------------

readGame :: String -> Game
readGame str = (turn, board)
      where (turnString:pitStrings) = lines str
            turn = case (stringToPlayer turnString) of 
                    Just value -> value
                    Nothing -> error "readGame couldn't find the turn"
            pits = makePits pitStrings
            board = makeBoard pits
            --
            makePits :: [String] -> [Pit]
            makePits [] = []
            makePits (str:strs) =
                let stringNums      = splitOn " " str
                    readIndex       = readMaybe (head stringNums)
                    Just index      = if readIndex==Nothing then error "Incorrect index in board" else readIndex
                    readNumMarbles  = readMaybe (last stringNums)
                    Just numMarbles = if readNumMarbles==Nothing then error "Incorrect marbles in board" else readNumMarbles 
                in ((index,numMarbles):(makePits strs))
            --
            makeBoard :: [Pit] -> Board
            makeBoard sourcePits = ((getPits sourcePits [1..6]), (head (getPits sourcePits [7])),(getPits sourcePits [8..13]), (head (getPits sourcePits [0])))
                where getPits :: [Pit] -> [Index] -> [Pit]
                      getPits _ [] = []
                      getPits [] _ = []
                      getPits (pit@(id,nm):ps) indexes = if id `elem` indexes
                                                       then (pit:(getPits ps indexes))
                                                       else (getPits ps indexes)
            --
            stringToPlayer :: String -> Maybe Player
            stringToPlayer str = case str of 
                                "P1" -> Just P1
                                "P2" -> Just P2
                                otherwise -> error ("readGame: Turn isn't in correct format - " ++ str)





----------------------------------------

----------- Story Thirteen -------------

showGame :: Game -> String
showGame game@(turn,board@(pitsOne,storeOne,pitsTwo,storeTwo)) = unlines ((show turn):((aux pitsOne)++(aux [storeOne])++(aux pitsTwo)++(aux [storeTwo])))
    where aux :: [Pit] -> [String]
          aux [] = []
          aux ((index,numMarbles):ps) = (((show index)++" "++(show numMarbles)):aux ps)

----------------------------------------

----------- Story Fourteen -------------

writeGame :: Game -> FilePath -> IO ()
writeGame game filepath = writeFile filepath $ showGame game

loadGame :: FilePath -> IO Game
loadGame filepath = 
    do  game <- readFile filepath
        return $ readGame game 

putBestMove :: Game -> IO ()
putBestMove game = 
        case bestMove game of 
            Just move -> do putStr $ "Best Move: " ++ show move ++ "\n" 
                            let newGame = completeMoveUnsafe game move
                            case checkWinner newGame of 
                                Just (Win player) -> putStr ("Game over, winner is " ++ show player ++ "!\n")
                                Just (Tie)        -> putStr "Game over, tie!\n"
                                Nothing           -> putStr $ prettyPrint $ newGame 
            Nothing   -> do putStr $ "Game is already complete.\n" 
                            putStr $ prettyPrint game

main :: IO ()
main = do
    args <- getArgs
    game <- loadGame $ head args
    putBestMove game

----------------------------------------