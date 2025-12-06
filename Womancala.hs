module Womancala where

import Solving
import Game

import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

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

----------- Story Twenty Two -----------
-- Support the  -w, --winner flag. When passed, the program should print out the best move, 
-- using the exhaustive search from the Story 9/Story 10 in the second sprint with no cut-off depth.
winnerFlag :: IO ()
winnerFlag = do
    args <- getArgs
    let filepath = head [arg | arg <- args, arg /= "-w" && arg /= "--winner"]
    game <- loadGame filepath
    case bestMove game of 
        Just move -> putStrLn $ "Best Move: " ++ show move
        Nothing   -> putStrLn "Game is already complete."
----------------------------------------

----------- Story Twenty Three ---------
-- Support the -d <num>, --depth <num> flag, allowing the user to specify <num> as a cutoff depth, 
-- instead of your default. This has no effect when combined with the -w flag. You may print a warning if both are provided.
depthFlag :: IO ()
depthFlag = do
    args <- getArgs
    let filepath = head [arg | arg <- args, arg /= "-d" && arg /= "--depth" && not (all (`elem` ['0'..'9']) arg)]
    let depthStr = head [arg | arg <- args, all (`elem` ['0'..'9']) arg]
    let depth = case readMaybe depthStr of 
                    Just num -> num
                    Nothing  -> error "Depth flag provided without a valid number."
    game <- loadGame filepath
    putStrLn $ "Using depth: " ++ show depth
    -- Should we call a depth-limited best move function instead of regular bestMove?
    -- ex: putBestMoveWithDepth game depth ??
    putStrLn "Depth-limited bestMove functionality not yet decided..."
----------------------------------------

----------- Story Twenty Four ----------
-- Support the -h, --help flag, which should print out a good help message and quit the program.
helpFlag :: IO ()
helpFlag = do
    putStrLn "Womancala Help:"
    putStrLn "How to use: womancala [options] <gamefile>"
    putStrLn "Options:"
    putStrLn "  -w, --winner        Print the best move."
    putStrLn "  -d <num>, --depth <num>  Specify cutoff depth for best move calculation."
    putStrLn "  -h, --help          Show this help message."
----------------------------------------

----------- Story Twenty Five ----------
-- Support the -m <move>, --move <move> flag, which should <move> and print out the resulting board to stdout.
-- You should print in the input format. If the -v flag is provided, you may pretty-print instead.
-- The move should be 1-indexed. If a move requires multiple values, 
-- the move should be a tuple of numbers separated by a comma with no space. 
-- You may use letters instead of numbers if you choose, which should be lower-cased and 'a'-indexed.
moveFlag :: IO ()
moveFlag = do
    args <- getArgs
    let filepath = head [arg | arg <- args, arg /= "-m" && arg /= "--move" && arg /= "-v" && arg /= "--verbose"]
    let moveStr = head [arg | arg <- args, arg /= filepath, arg /= "-v", arg /= "--verbose", arg /= "-m", arg /= "--move"]
    let move = case readMaybe moveStr of 
                    Just num -> num
                    Nothing  -> error "Move flag provided without a valid number."
    game <- loadGame filepath
    let newGame = case completeMove game move of  -- completetMove or completeMoveUnsafe??
                    Just g -> game
                    Nothing -> error "Move was invalid."
    putStrLn $ showGame newGame
----------------------------------------

----------- Story Twenty Sixth ---------
-- Support the -v, --verbose flag, 
-- which should output both the move and a description of how good it is: win, lose, tie, or a rating.
verboseFlag :: IO ()
verboseFlag = do
    args <- getArgs
    let filepath = head [arg | arg <- args, arg /= "-v" && arg /= "--verbose"]
    game <- loadGame filepath
    case bestMove game of 
        Just move -> do 
            putStrLn $ "Best Move: " ++ show move
            let newGame = case completeMove game move of -- could i just do completeMoveUnsafe here?
                    Just g -> g
                    Nothing -> error "bestMove chose invalid move."
            case checkWinner newGame of 
                Just (Win player) -> putStrLn ("This move results in a win for " ++ show player ++ "!")
                Just Tie         -> putStrLn "This move results in a tie!"
                Nothing          -> putStrLn ("This move leads to a rating of " ++ show (rateGame newGame) ++ ".")
        Nothing   -> putStrLn "Game is already complete."
----------------------------------------