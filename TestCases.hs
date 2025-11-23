module TestCases where

import System.IO
import Womancala
import System.Environment
import System.IO

main :: IO ()
main = 
    do  putStrLn "Testing Story 2 - checkWinner:"
        game1 <- loadGame "FinalStateP1.txt"
        game2 <- loadGame "FinalStateP2.txt"
        game3 <- loadGame "FinalStateTie.txt"
        game4 <- loadGame "OneStepP2.txt"
        game5 <- loadGame "InitialBoard.txt"
        game6 <- loadGame "ForceP1Win.txt"
        gameString5 <- readFile "InitialBoard.txt"
        putStrLn ("Final State, P1 wins: "++(show (checkWinner game1 == (Just (Win P1)))))
        putStrLn ("Final State, P2 wins: "++(show (checkWinner game2 == (Just (Win P2)))))
        putStrLn ("Final State, Tie:     "++(show (checkWinner game3 == (Just Tie))))
        putStrLn ("Unfinished Game:      "++(show (checkWinner game4 == Nothing)))
        putStrLn "Testing Story 3 - completeMove:"
        putStrLn ("Normal move:    "++(show (completeMove game5 1 == (Just (P2,[(0,0),(1,0),(2,5),(3,5),(4,5),(5,5),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)])))))
        putStrLn ("Landing in pit: "++(show (completeMove game5 3 == (Just (P1,[(0,0),(1,4),(2,4),(3,0),(4,5),(5,5),(6,5),(7,1),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)])))))
        putStrLn ("Scoring:        "++(show (completeMove (P1,[(0,0),(1,1),(2,0),(3,5),(4,5),(5,5),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)]) 1 ==
                                                  (Just (P2,[(0,0),(1,0),(2,0),(3,5),(4,5),(5,5),(6,4),(7,5),(8,4),(9,4),(10,4),(11,4),(12,0),(13,4)])))))
        putStrLn ("Wrapping        "++(show (completeMove (P1,[(0,0),(1,13),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)]) 1 ==
                                                  (Just (P2,[(0,0),(1,0),(2,5),(3,5),(4,5),(5,5),(6,5),(7,7),(8,5),(9,5),(10,5),(11,5),(12,5),(13,0)])))))
        putStrLn "Testing Story 4 - validMoves:"
        putStrLn ("Initial state: "++(show (validMoves game5 == [1,2,3,4,5,6])))
        putStrLn ("One step:      "++(show (validMoves game4 == [13])))
        putStrLn "Testing Story 9 - whoWillWin:"
        putStrLn ("Final State:    "++(show (whoWillWin game1 == Win P1)))
        putStrLn ("Variable State: "++(show (whoWillWin game6 == Win P1)))
        putStrLn "Testing Story 10 - bestMove:"
        putStrLn ("Final State:   "++(show (bestMove game1 == Nothing)))
        putStrLn ("VariableState: "++(show (bestMove game6 == Just 5)))
        putStrLn "Testing Story 13 - showGame:"
        putStrLn ("Input equals output: "++(show ((showGame game5) == gameString5)))