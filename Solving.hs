module Solving where

import Game
import Control.Applicative ((<|>))

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

------------- Story Ten ----------------

bestMove :: Game -> Maybe Move -- Doesn't need to consider no moves in validmoves because that is caught in checkwinner 
bestMove game@(turn,board) = case checkWinner game of
    (Just winner) -> Nothing
    Nothing       -> lookup (Win turn) moveTuples
                 <|> lookup Tie moveTuples
                 <|> (Just $ snd $ head moveTuples)

    where moveTuples = [(whoWillWin (completeMoveUnsafe game move), move) | move <- (validMoves game)]

----------------------------------------