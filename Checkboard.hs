module Checkboard where

import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.List (intersect)
import Data.Maybe (fromJust)
import Control.Parallel.Strategies
import Control.Parallel

import Structures
import Const
import Hashing

moveWithoutAssertL:: Checkboard -> (Int, Int) -> Checkboard
moveWithoutAssertL x m
  | to > 55 && from < 56 && from > 40 && movedPiece == (CPiece Pawn White)  = x { board = (vector V.// [(to, (CPiece Queen White)),(from, Empty)]), whoNext = oppColor,  history =  newHistory}
  | to < 8 && from < 16 && from > 7 && movedPiece == (CPiece Pawn Black)  = x { board =(vector V.// [(to, (CPiece Queen Black)),(from, Empty)]), whoNext = oppColor,   history =  newHistory}
  | otherwise =  x { board =(vector V.// [(to, movedPiece),(from, Empty)]), whoNext = oppColor, history =  newHistory}
  where from = fst m
	to = snd m
	movedPiece = vector V.! (from)
	attackedPiece = vector V.! (to)
	vector = board x
        status = computeStatus x
	oppColor = getOppColor $ whoNext x
	newHistory = x:(history x)        


moveWithoutAssert:: Checkboard -> (Int, Int) -> Checkboard
moveWithoutAssert x m = middleBoard { status = (computeStatus (middleBoard {historyMap =  newHistoryMap }))  , movesNoAttackNoPawn = newMovesNoAttackNoPawn, historyMap =  newHistoryMap}
   where middleBoard = moveWithoutAssertL x m
         from = fst m
         to = snd m
         vector = board x
         movedPiece = vector V.! (from)
         attackedPiece = vector V.! (to)
         newHistoryMap = if isAttackOrPawnMove 
			    then Map.singleton (hashCheckboard middleBoard) 1 
                            else  Map.alter updateMap (hashCheckboard middleBoard) (historyMap middleBoard) --TODO update with xor
         isAttackOrPawnMove = movedPiece == (CPiece Pawn White) || movedPiece == (CPiece Pawn Black) || attackedPiece /= Empty
         newMovesNoAttackNoPawn = if isAttackOrPawnMove 
                                     then 0 
				     else (movesNoAttackNoPawn x) + 1
         updateMap Nothing = Just 1
         updateMap (Just x) = Just (x + 1)         


computeStatus :: Checkboard -> Status
computeStatus x
   | movesNoAttackNoPawn x == no_attack_no_pawn_move_limit = Draw -- move limit
   |  (historyMap x) Map.! (hashCheckboard x) == 3 = Draw --3 the same rule
   | isAnyPossible =  InProgress
   | not isAnyPossible && isInCheck x whoN &&  whoN == White = BlackWon
   | not isAnyPossible &&  isInCheck x whoN &&  whoN == Black = WhiteWon
   | not isAnyPossible &&  not (isInCheck x whoN) = Draw
  where
      isAnyPossible = isAnyMovePossible x
      whoN = whoNext x


isMovePossible::  (Int, Int) -> CPiece -> Bool
isMovePossible move piece
 | ((fromY == toY) && (fromX == toX)) = False
 | p piece == Rook && rookMove = True
 | p piece == Bishop && bishopMove = True
 | p piece == Queen && (bishopMove || rookMove) = True
 | p piece == Knight && knightMove = True
 | p piece == King && kingMove = True
 | p piece == Pawn && c piece == White && pawnWhiteMove = True
 | p piece == Pawn && c piece == Black && pawnBlackMove = True
 | otherwise = False
  where
    rookMove = (fromX == toX || fromY == toY)
    bishopMove = (abs(fromX - toX) == abs(fromY - toY))
    knightMove = ((abs(fromX - toX) + abs(fromY - toY) == 3) && (fromX /= toX) && (fromY /= toY))
    pawnWhiteMove = ((fromX == toX && toY == fromY + 1 ) || --normal
                        (fromY == 1 && toY == 3 && fromX == toX)) || -- 2 squares at first move
                        (abs(fromX - toX) == 1 && toY == fromY + 1) -- attacking
    pawnBlackMove = ((fromX == toX && fromY == toY + 1 ) || --normal
                        (fromY == 6 && toY == 4 && fromX == toX)) || -- 2 squares at first move
                        (abs(fromX - toX) == 1 && fromY == toY + 1) -- attacking
    kingMove = abs(fromX - toX) <= 1 && abs(fromY - toY) <= 1
    (fromY, fromX) = (fst move `divMod` 8)
    (toY, toX) = (snd move `divMod` 8)

getPossibleMovesTable:: CPiece -> V.Vector [(Int, Int)]
getPossibleMovesTable cpiece
 | cpiece == Empty = V.map (\x -> [  ])  (V.enumFromTo 0 63)
 | otherwise = V.map (\x -> [(x,y) | y<-[0..63], isMovePossible (x, y) cpiece  ])  (V.enumFromTo 0 63)

isPieceBetween:: V.Vector CPiece -> (Int, Int) -> Bool
isPieceBetween vector move
  | ((abs(fromX - toX) < 2) && (abs(fromY - toY) < 2)) = False
  | fromX == toX = (not $ all  (==Empty) (map (vector V.!) [8 * y + toX  | y <- [(minY + 1)..(maxY -1)]]))
  | fromY == toY = (not $ all  (==Empty) (map (vector V.!) [8 * toY + x  | x <- [(minX + 1)..(maxX -1)]]))
  | abs(fromX - toX) == abs(fromY - toY) = (not $ all  (==Empty) (map (vector V.!)
                           [8 * y + x  | x <- [(minX + 1)..(maxX -1)],   y <- [(minY + 1)..(maxY -1)],  abs(x - toX) == abs(y - toY)  ]))
  | otherwise = False
  where (fromY, fromX) = (fst move `divMod` 8)
	(toY, toX) = (snd move `divMod` 8)
	minY = min toY  fromY
	maxY = max toY  fromY
	minX = min toX  fromX
	maxX = max toX  fromX
--[(from, to) | from <- [0..63], to <- [0..63], (initialBoard V.! from) /= Empty,  isMovePossible (from, to) (initialBoard V.! from), not $ isPieceBetween initialBoard (from,to)]

isMoveLegal:: Checkboard -> (Int, Int) -> Bool
isMoveLegal checkboard move
 | (vector V.! fst move) == Empty = False       --moving empty
 | c (vector V.! fst move) /= whoN = False   --moving not yours
 | ( cpieceToAttack) /= Empty && c (vector V.! snd move) == whoN = False   --attacking yours
 | isPieceBetween vector move = False
 | (( pieceToMove) == Pawn) && ( cpieceToAttack) == Empty && fromX /= toX = False --attack with Pawn on empty
 | ( pieceToMove == Pawn) &&  cpieceToAttack /= Empty && fromX == toX = False -- move with Pawn on nonempty
 | ( cpieceToAttack) /= Empty && p cpieceToAttack == King = False
 | not $ isMovePossible move (vector V.! fst move) = False
 | willBeInCheck checkboard move whoN = False
 | otherwise = True
  where vector = board checkboard
        whoN = whoNext checkboard
        pieceToMove = p (vector V.! fst move)
        cpieceToAttack =  (vector V.! snd move)
        (fromY, fromX) = (fst move `divMod` 8)
        (toY, toX) = (snd move `divMod` 8)

isInCheck:: Checkboard -> Color -> Bool
isInCheck checkboard color =  possibleAttacks /= []
  where kingPos = fromJust $ V.findIndex (CPiece King color ==) vector
        vector = board checkboard
        oppPositions = [x | x <- [0..63], vector V.! x /= Empty,  c (vector V.! x) == oppColor]
        oppColor = getOppColor color
        pawnPositionsColKing = [(kingPos - 8), (kingPos + 8)] `intersect` [x | x <- oppPositions,  p (vector V.! x) == Pawn]
        possibleAttacks = [(from, kingPos) | from <- oppPositions,
                                             isMovePossible (from,  kingPos) (vector V.! from),
                                             not $ isPieceBetween vector (from,kingPos),
                                             not $ from `elem` pawnPositionsColKing] --be sure to exlude Pawn nonattacking move

willBeInCheck::Checkboard -> (Int, Int) -> Color -> Bool
willBeInCheck a b c = isInCheck (moveWithoutAssert a b) c

isAnyMovePossible::Checkboard  -> Bool
isAnyMovePossible checkboard = [(from, to) | (from, to)  <- precompiledMoves, isMoveLegal checkboard (from, to)] /= []
 where
   precompiledMoves =  foldl (++) [] (map  (\from -> ((getPossibleMovesTable ((board checkboard) V.! from)) V.! from)) [0..63] `using` rpar)
