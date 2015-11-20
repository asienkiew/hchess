module AI where


import qualified Data.Vector as V

import Control.Monad
import Control.Parallel.Strategies
import Control.Parallel
import Data.Vector.Strategies

import Structures

whitePawnEval = V.fromList $ reverse [  0,  0,  0,  0,  0,  0,  0,  0,50, 50, 50, 50, 50, 50, 50, 50,10, 10, 20, 30, 30, 20, 10, 10, 5,  5, 10, 25, 25, 10,  5,  5, 0,  0,  0, 20, 20,  0,  0,  0, 5, -5,-10,  0,  0,-10, -5,  5, 5, 10, 10,-20,-20, 10, 10,  5, 0,  0,  0,  0,  0,  0,  0,  0]
whiteKnightEval =  V.fromList $ reverse [ -50,-40,-30,-30,-30,-30,-40,-50,-40,-20,  0,  0,  0,  0,-20,-40,-30,  0, 10, 15, 15, 10,  0,-30,-30,  5, 15, 20, 20, 15,  5,-30,-30,  0, 15, 20, 20, 15,  0,-30,-30,  5, 10, 15, 15, 10,  5,-30,-40,-20,  0,  5,  5,  0,-20,-40,  -50,-40,-30,-30,-30,-30,-40,-50]
whiteBishopEval =  V.fromList $ reverse [-20,-10,-10,-10,-10,-10,-10,-20,-10,  0,  0,  0,  0,  0,  0,-10,-10,  0,  5, 10, 10,  5,  0,-10,-10,  5,  5, 10, 10,  5,  5,-10,-10,  0, 10, 10, 10, 10,  0,-10,-10, 10, 10, 10, 10, 10, 10,-10,-10,  5,  0,  0,  0,  0,  5,-10,-20,-10,-10,-10,-10,-10,-10,-20]
whiteRookEval =  V.fromList $ reverse [0,  0,  0,  0,  0,  0,  0,  0,  5, 10, 10, 10, 10, 10, 10,  5, -5,  0,  0,  0,  0,  0,  0, -5, -5,  0,  0,  0,  0,  0,  0, -5, -5,  0,  0,  0,  0,  0,  0, -5, -5,  0,  0,  0,  0,  0,  0, -5, -5,  0,  0,  0,  0,  0,  0, -5,  0,  0,  0,  5,  5,  0,  0,  0]
whiteQueenEval =  V.fromList $ reverse [-20,-10,-10, -5, -5,-10,-10,-20,-10,  0,  0,  0,  0,  0,  0,-10,-10,  0,  5,  5,  5,  5,  0,-10, -5,  0,  5,  5,  5,  5,  0, -5,  0,  0,  5,  5,  5,  5,  0, -5,-10,  5,  5,  5,  5,  5,  0,-10,-10,  0,  5,  0,  0,  0,  0,-10,-20,-10,-10, -5, -5,-10,-10,-20]
whiteKingMiddleEval =  V.fromList $ reverse [-30,-40,-40,-50,-50,-40,-40,-30,-30,-40,-40,-50,-50,-40,-40,-30,-30,-40,-40,-50,-50,-40,-40,-30,-30,-40,-40,-50,-50,-40,-40,-30,-20,-30,-30,-40,-40,-30,-30,-20,-10,-20,-20,-20,-20,-20,-20,-10, 20, 20,  0,  0,  0,  0, 20, 20, 20, 30, 10,  0,  0, 10, 30, 20]
whiteKingEndEval =  V.fromList $ reverse [-50,-40,-30,-20,-20,-30,-40,-50,-30,-20,-10,  0,  0,-10,-20,-30,-30,-10, 20, 30, 30, 20,-10,-30,-30,-10, 30, 40, 40, 30,-10,-30,-30,-10, 30, 40, 40, 30,-10,-30,-30,-10, 20, 30, 30, 20,-10,-30,-30,-30,  0,  0,  0,  0,-30,-30,-50,-30,-30,-30,-30,-30,-30,-50]






getScore::Checkboard ->  Color -> Int
getScore checkboard who = score
  where vector = board checkboard
	score = V.sum $ V.imap (cpieceToValue who) vector


cpieceToValue::  Color -> Int -> CPiece -> Int
cpieceToValue who pos cpiece
  |   cpiece == Empty = 0
  |p  cpiece == Pawn =   sign * (100 + ( whitePawnEval V.! corr_pos))
  |p  cpiece == Knight = sign * (320 + ( whiteKnightEval V.! corr_pos))
  |p  cpiece == Bishop = sign * (330 + ( whiteBishopEval V.! corr_pos))
  |p  cpiece == Rook =   sign * (500 + ( whiteRookEval V.! corr_pos))
  |p  cpiece == Queen =  sign * (900 + ( whiteQueenEval V.! corr_pos))
  |p  cpiece == King =   sign * (100000 + ( whiteKingMiddleEval V.! corr_pos))
  | otherwise = 0
  where sign = if who == (c cpiece) then (1) else (-1)
	corr_pos = if (c cpiece) == White then pos else (63 - pos)


getBestMove::  Int -> Checkboard -> (Int, (Int, Int))    --(score, move)
getBestMove level checkboard
  | status checkboard == Draw = (-1, (0,0))
  | status checkboard == BlackWon && whoAtBegin == Black = (100000, (0,0))
  | status checkboard == BlackWon && whoAtBegin == White = (-100000, (0,0))
  | status checkboard == WhiteWon && whoAtBegin == White = (100000, (0,0))
  | status checkboard == WhiteWon && whoAtBegin == Black = (-100000, (0,0))
  | level == 0 =  ((extremumFunc movesScores), moves V.! (extremumFuncIndex movesScores))
--  | level < 3  && V.null moves = (sign * 10000, (0,0))   --TODO: to tak jak by szachmmat
  | level < 3 = ( ( extremumFunc movesScores), (0,0))

  | level == 3 = (getScore checkboard whoAtBegin, (0,0))
  where
        checkboardsAfterMove = (V.map (moveWithoutAssert checkboard) moves) `using` (parVector 3)
        moves = V.fromList  [(from, to) | (from, to)  <- precompiledMoves, isMoveLegal checkboard (from, to)]
        whoAtBegin = if level `mod` 2 == 0 then whoNext checkboard else oppColor
        oppColor = if (whoNext checkboard == White) then Black else White
        extremumFuncIndex = if level `mod` 2 == 0 then V.maxIndex else V.minIndex
        extremumFunc = if level `mod` 2 == 0 then V.maximum else V.minimum
        sign = if whoAtBegin == whoNext checkboard then (1) else (-1)
        precompiledMoves = foldl (++) [] (map  (\from -> ((getPossibleMovesTable ((board checkboard) V.! from)) V.! from)) [0..63] `using` rdeepseq)
        movesScores =  V.map  (fst . ( getBestMove (level + 1)))  checkboardsAfterMove `using` (parVector 3)

