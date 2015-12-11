module AI where


import qualified Data.Vector as V

import Control.Monad
import Control.Parallel.Strategies
import Control.Parallel
import Data.Vector.Strategies

import Structures
import Const
import Checkboard

type GameTreeLevel = Int
type GameTreeDepth = Int
type IsEndGame = Bool

getScore::Checkboard ->  Color -> IsEndGame -> Int
getScore checkboard who isEndGame = V.sum $ V.imap (cpieceToValue who isEndGame) vector
  where vector = board checkboard

getScoreSimple::Checkboard ->  Color -> Int
getScoreSimple checkboard who = V.sum $ V.map (cpieceToValueSimple who) vector
  where vector = board checkboard

cpieceToValueSimple::  Color -> CPiece -> Int
cpieceToValueSimple who cpiece
  |   cpiece == Empty =     0
  |c  cpiece /= who =       0
  |p  cpiece == Pawn =    100
  |p  cpiece == Knight =  320
  |p  cpiece == Bishop =  330
  |p  cpiece == Rook =    500
  |p  cpiece == Queen =   900
  | otherwise = 0

	
cpieceToValue::  Color -> IsEndGame -> Int -> CPiece  -> Int
cpieceToValue who isEndGame pos cpiece
  |   cpiece == Empty = 0
  |p  cpiece == Pawn =   sign * (100 + ( whitePawnEval V.! corr_pos))
  |p  cpiece == Knight = sign * (320 + ( whiteKnightEval V.! corr_pos))
  |p  cpiece == Bishop = sign * (330 + ( whiteBishopEval V.! corr_pos))
  |p  cpiece == Rook =   sign * (500 + ( whiteRookEval V.! corr_pos))
  |p  cpiece == Queen =  sign * (900 + ( whiteQueenEval V.! corr_pos))
  |p  cpiece == King =   sign * (100000 + ( whiteKingEval V.! corr_pos))
  | otherwise = 0
  where sign = if who == (c cpiece) then (1) else (-1)
	corr_pos = if (c cpiece) == White then pos else (63 - pos)
	whiteKingEval = if isEndGame then whiteKingEndEval else whiteKingMiddleEval


getDepthLevelAdd:: (Int, Int) -> Int
getDepthLevelAdd (whiteScore, blackScore)
  | (whiteScore + blackScore) < 1000 = 3
  | (whiteScore + blackScore) < 1800 = 2
  |( whiteScore + blackScore) < 2800 = 1
  | otherwise = 0

getBestMove::  Checkboard -> (Int, Int)    --(score, move)
getBestMove checkboard = snd $ getBestMoveRecur 0 depth isEndGame checkboard
  where
    simpleEvalWhite = getScoreSimple checkboard White
    simpleEvalBlack = getScoreSimple checkboard Black
    depth = game_tree_depth + getDepthLevelAdd (simpleEvalBlack, simpleEvalWhite)
    isEndGame = simpleEvalBlack < 1200 && simpleEvalWhite < 1200

getBestMoveRecur::  GameTreeLevel -> GameTreeDepth -> IsEndGame -> Checkboard -> (Int, (Int, Int))    --(score, move)
getBestMoveRecur level depth isEndGame checkboard
  | status checkboard == Draw = (-1, (0,0))
  | status checkboard == BlackWon && whoAtBegin == Black = (100000, (0,0))
  | status checkboard == BlackWon && whoAtBegin == White = (-100000, (0,0))
  | status checkboard == WhiteWon && whoAtBegin == White = (100000, (0,0))
  | status checkboard == WhiteWon && whoAtBegin == Black = (-100000, (0,0))
  | level == 0 =  ((extremumFunc movesScores), moves V.! (extremumFuncIndex movesScores))
  | (level < depth) || (level == depth && deepenOne) = ( ( extremumFunc movesScores), (0,0)) --1 level deepening

  | level >= depth = (getScore checkboard whoAtBegin isEndGame, (0,0))
  where
        deepenOne = deepenOnAttack && whoWasAttackedLast checkboard /= Empty
        checkboardsAfterMove = (V.map (moveWithoutAssert checkboard) moves) `using` (parVector 3)
        moves = V.fromList  [(from, to) | (from, to)  <- precompiledMoves, isMoveLegal checkboard (from, to)]
        whoAtBegin = if level `mod` 2 == 0 then whoN else oppColor
        oppColor =  getOppColor $ whoN
        extremumFuncIndex = if level `mod` 2 == 0 then V.maxIndex else V.minIndex
        extremumFunc = if level `mod` 2 == 0 then V.maximum else V.minimum
        sign = if whoAtBegin == whoN then (1) else (-1)
        precompiledMoves = foldl (++) [] (map  (\from -> ((gPMTable ((board checkboard) V.! from) (whoN) ) V.! from)) numList `using` rdeepseq)
        movesScores =  V.map  (fst . ( getBestMoveRecur (level + 1) depth isEndGame))  checkboardsAfterMove `using` (parVector 3)
        whoN = whoNext checkboard
        gPMTable = getPossibleMovesTable
