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

----------------------------------------
------------------MAIN------------------
main :: IO ()
main = do
    (flags, nonOpts) <- parseFlags
    case nonOpts of
        [filepath] -> do
            game <- loadGame filepath
            dispatchFlags flags game
        _ -> do
            putStrLn "Please provide a game file."
            putStrLn helpMessage
            exitFailure

----------------------------------------
----------------------------------------

----------- Story Twenty Two -----------
-- Support the  -w, --winner flag. Uses Story 9/Story 10 in the second sprint with no cut-off depth.
winnerFlag :: IO ()
winnerFlag = do
    (flags, nonOpts) <- parseFlags
    case nonOpts of
        [filepath] -> do
            game <- loadGame filepath
            dispatchFlags (Winner : flags) game
        _ -> do
            putStrLn "Please provide a single game file."
            putStrLn helpMessage
            exitFailure

----------------------------------------

----------- Story Twenty Three ---------
-- Support the -d <num> and --depth <num> 
-- Shouldn't have effect when combined with the -w flag.
depthFlag :: IO ()
depthFlag = do
    (flags, nonOpts) <- parseFlags
    case nonOpts of
        [filepath] -> do
            game <- loadGame filepath
            dispatchFlags flags game
        _ -> do
            putStrLn "Please provide a game file."
            putStrLn helpMessage
            exitFailure

----------------------------------------

----------- Story Twenty Four ----------
-- Supports the -h and --help 
helpFlag :: IO ()
helpFlag = do
    putStrLn helpMessage
    exitSuccess

----------------------------------------

----------- Story Twenty Five ----------
-- Supports the -m <move> and --move <move> 
moveFlag :: IO ()
moveFlag = do
    (flags, nonOpts) <- parseFlags
    case nonOpts of
        [filepath] -> do
            game <- loadGame filepath
            dispatchFlags flags game
        _ -> do
            putStrLn "Please provide a game file."
            putStrLn helpMessage
            exitFailure

----------------------------------------

----------- Story Twenty Sixth ---------
-- Supports the -v and --verbose 
verboseFlag :: IO ()
verboseFlag = do
    (flags, nonOpts) <- parseFlags
    case nonOpts of
        [filepath] -> do
            game <- loadGame filepath
            dispatchFlags flags game
        _ -> do
            putStrLn "Please provide a game file."
            putStrLn helpMessage
            exitFailure

----------------------------------------

------- Command Line Interface ----------

-- Possible flags for Womancala
data Flag
    = Winner        -- -w, --winner  (story 22)
    | Verbose       -- -v, --verbose (story 26)
    | Depth String  -- -d <number>, --depth <number> (story 23)
    | Move String   -- -m <move>, --move <move> (story 25)
    | Help          -- -h, --help (story 24)
    deriving (Eq, Show)

-- Flags recognized by GetOpt
options :: [OptDescr Flag]
options =
    [ Option ['w'] ["winner"] (NoArg Winner) "Print the ultimate best move."
    , Option ['v'] ["verbose"] (NoArg Verbose) "Verbose output / pretty print in -m mode."
    , Option ['d'] ["depth"] (ReqArg Depth "NUMBER") "Specify cutoff depth for best move calculation."
    , Option ['m'] ["move"] (ReqArg Move "MOVE") "Make the specified move and print the resulting board."
    , Option ['h'] ["help"] (NoArg Help) "Show help message."
    ]

-- Displayed when -h / --help is used or when invalid input is given
helpMessage :: String
helpMessage = usageInfo "Womancala Help:\n\nUsage: womancala [options] <gamefile>\n\nOptions:" options

-- Parses flags, returns them with their arguments
parseFlags :: IO ([Flag], [String])
parseFlags = do
    args <- getArgs
    let (flags, nonOpts, errs) = getOpt Permute options args
    if not (null errs)
        then do
            putStrLn (concat errs ++ "\n" ++ helpMessage)
            exitFailure
        else return (flags, nonOpts)

-- Helpers
firstDepth :: [Flag] -> Maybe String
firstDepth [] = Nothing
firstDepth (Depth d : _) = Just d
firstDepth (_ : fs) = firstDepth fs

firstMoveStr :: [Flag] -> Maybe String
firstMoveStr [] = Nothing
firstMoveStr (Move m : _) = Just m
firstMoveStr (_ : fs) = firstMoveStr fs

hasFlag :: (Flag -> Bool) -> [Flag] -> Bool
hasFlag _ [] = False
hasFlag p (f:fs) = p f || hasFlag p fs

parseMove :: String -> Maybe Move
parseMove s =
    case splitOn "," s of
        [_] -> readMaybe s 
        _   -> Nothing 

-- Turns winner result into "win/lose/tie" for the current player.
resultString :: Player -> Winner -> String
resultString turn result =
    case result of
        Tie       -> "tie"
        Win p     -> if p == turn then "win" else "lose"

-- Executes the given flags
dispatchFlags :: [Flag] -> Game -> IO ()
dispatchFlags flags game@(turn, _) = do
    -- Story 24 - Help flag
    if hasFlag (== Help) flags
        then do putStrLn helpMessage
                exitSuccess
        else return ()

    -- Story 23 - Depth flag: UNIMPLEMENTED
    -- I believe we'll need Story 18 to complete this
    -- case firstDepth flags of
    --     Just ds ->
    --         case readMaybe ds of
    --             Nothing -> do
    --             Just _ ->
    --     Nothing -> return ()

    -- Story 25 - Move flag
    case firstMoveStr flags of
        Just mvStr ->
            case parseMove mvStr of
                Nothing -> do
                    putStrLn "Invalid input for move."
                    exitFailure
                Just mv ->
                    case completeMove game mv of
                        Nothing -> do
                            putStrLn "Move was invalid."
                            exitFailure
                        Just newGame -> do
                            if hasFlag (== Verbose) flags
                                then putStr (prettyPrint newGame)
                                else putStrLn (showGame newGame)
                            exitSuccess
        Nothing -> return ()

    -- Story 22 - Winner flag
    case bestMove game of
        Nothing -> do
            putStrLn "Game is already complete."
            exitSuccess
        Just mv -> do
            putStrLn (show mv)

            -- Story 26 - Verbose
            if hasFlag (== Verbose) flags
                then do
                    let nextGame = completeMoveUnsafe game mv
                    if hasFlag (== Winner) flags
                        then do
                            let outcome = whoWillWin nextGame
                            putStrLn ("Quality: " ++ resultString turn outcome)
                        else do
                            putStrLn ("Rating: " ++ show (rateGame nextGame))
                else return ()
