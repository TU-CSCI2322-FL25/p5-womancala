module Solving where

import Game

import Control.Applicative ((<|>))
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

type Rating = Int 

------------- Story Nine ---------------

whoWillWin :: Game -> Winner -- Doesn't need to consider no moves in validmoves because that is caught in checkwinner 
whoWillWin game@(turn, board) = case checkWinner game of -- I wonder if we can optimize this by checking if one player has too many marbles to catch up?
        (Just winstate) -> winstate
        Nothing         -> bestOutcome [whoWillWin (completeMoveUnsafe game move) | move <- validMoves game] 
    where   bestOutcome :: [Winner] -> Winner
            bestOutcome winlist
                | (Win turn) `elem` winlist = (Win turn)
                | Tie `elem` winlist = Tie
                | otherwise = Win (opponent turn)

----------------------------------------

------------- Story 18/19 --------------

whoMightWin :: Game -> Int -> (Rating, Move)
whoMightWin game 0 = error "Move depth can't be 0." 
whoMightWin game@(turn, board) depth = 
        if depth == 1 
        then tripleToTuple (comparison (comparing (\(x,y,z) -> x)) possibleMoves)
        else case hasWinState deepMoves of 
            Nothing -> comparison (comparing fst) deepMoves
            Just moveTuple -> moveTuple 

    where possibleMoves = [(rateGame newGame, move, newGame) | move <- validMoves game, let newGame = completeMoveUnsafe game move]
          deepMoves = [(fst (whoMightWin newState (depth-1)), possibleMove) | (rating, possibleMove, newState) <- possibleMoves]

          comparison = if turn == P1 then (maximumBy) else (minimumBy)

          hasWinState :: [(Rating, Move)] -> Maybe (Rating, Move)
          hasWinState [] = Nothing
          hasWinState (ratingTuple@(rating, move):xs) = 
            if (rating >= 400 && turn == P1) || (rating <= -400 && turn == P2) 
            then Just ratingTuple 
            else hasWinState xs  

          tripleToTuple :: (a, b, c) -> (a, b)
          tripleToTuple (f,s,t) = (f,s) 

----------------------------------------

------------- Story Ten ----------------

bestMove :: Game -> Maybe Move -- Doesn't need to consider no moves in validmoves because that is caught in checkwinner 
bestMove game@(turn,board) = case checkWinner game of
    (Just winner) -> Nothing
    Nothing       -> lookup (Win turn) moveTuples
                 <|> lookup Tie moveTuples
                 <|> (Just $ snd $ head moveTuples)

    where moveTuples = [(whoWillWin (completeMoveUnsafe game move), move) | move <- (validMoves game)]

----------------------------------------