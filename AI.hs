module AI where


import qualified Data.Vector as V

import Control.Monad
import Control.Parallel.Strategies
import Control.Parallel
import Data.Vector.Strategies

import Structures
import Const
import Checkboard



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
  | level < game_tree_depth = ( ( extremumFunc movesScores), (0,0))

  | level == game_tree_depth = (getScore checkboard whoAtBegin, (0,0))
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

