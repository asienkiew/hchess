module Structures where

import Data.Char (ord)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.List.Split (splitEvery)
import Data.List (intersect)
import Control.Monad
import qualified Data.Vector as V
import Control.Parallel.Strategies
import Control.Parallel


data Piece = Queen | King | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
data CPiece = CPiece {p :: Piece, c ::  Color }  | Empty deriving (Eq)

data Checkboard = Checkboard {board :: V.Vector CPiece , whoNext :: Color, status :: Status} deriving( Eq)
instance NFData Checkboard
data Status = Draw | WhiteWon | BlackWon | InProgress deriving (Eq, Show)

instance Show Checkboard where
    show a = checkboardToStr a

instance Show CPiece where
    show a =  [convertToChar a]

stringToMove ::  [Char] -> (Int,Int)
stringToMove x
  | length x /= 4 = err
  |  not (a `elem` rows) = err
  |  not (b `elem` columns) = err
  |  not (c `elem` rows) = err
  |  not (d `elem` columns) = err
  |  otherwise =   ((8 * (ord b - 49) + ord a - 97), (8 * (ord d - 49) + ord c - 97))
  where
     rows = ['a'..'h']
     columns = ['1'..'8']
     err = (0,0) --error "bad format of Command"
     a = toLower(x !! 0)
     b = toLower(x !! 1)
     c = toLower(x !! 2)
     d = toLower(x !! 3)

moveToString ::  (Int,Int) -> [Char]
moveToString move  =   (rows !! fromX):(columns !! fromY):(rows !! toX):(columns !! toY) : []
  where
     rows = ['a'..'h']
     columns = ['1'..'8']
     (fromY, fromX) = (fst move `divMod` 8)
     (toY, toX) = (snd move `divMod` 8)

printBoard :: Checkboard  -> IO ()
printBoard checkboard = putStr $ checkboardToStr checkboard

checkboardToStr :: Checkboard  -> [Char]
checkboardToStr checkboard
  | V.length a /= 64 = error "Bad format"
  | otherwise  = concat . reverse $

                border  ++
                zipWith3  (\x y z -> x ++ y ++ z)
                       (map (\x -> [x] ++ "  ") columns)
                       (splitEvery 16 (concat (map (:' ':[]) (map convertToChar (V.toList a) ))))
                       (map (\x -> [' '] ++ [x] ++ ['\n']) columns)
                ++ border


  where rows = ['a'..'h']
        columns = ['1'..'8']
        border = ["\n   " ++ concat (map (:' ':[])  rows) ++ " \n\n"]
        a = board checkboard



convertToChar :: CPiece -> Char
convertToChar x
    | x == (CPiece Queen White) = 'Q'
    | x == (CPiece King White) = 'K'
    | x == (CPiece Rook White) = 'R'
    | x == (CPiece Bishop White) = 'B'
    | x == (CPiece Knight White) = 'N'
    | x == (CPiece Pawn White) = 'P'
    | x == (CPiece Queen Black) = 'q'
    | x == (CPiece King Black) = 'k'
    | x == (CPiece Rook Black) = 'r'
    | x == (CPiece Bishop Black) = 'b'
    | x == (CPiece Knight Black) = 'n'
    | x == (CPiece Pawn Black) = 'p'
    | x == (Empty) = '.'
    | otherwise = error "bad letter"

convert :: Char -> CPiece
convert x
    | x =='Q' = (CPiece Queen White)
    | x == 'K' = (CPiece King White)
    | x == 'R' = (CPiece Rook White)
    | x == 'B' = (CPiece Bishop White)
    | x == 'N' = (CPiece Knight White)
    | x == 'P' = (CPiece Pawn White)
    | x == 'q' = (CPiece Queen Black)
    | x == 'k' = (CPiece King Black)
    | x == 'r' = (CPiece Rook Black)
    | x == 'b' = (CPiece Bishop Black)
    | x == 'n' = (CPiece Knight Black)
    | x == 'p' = (CPiece Pawn Black)
    | x == '.' = (Empty)
    | otherwise = error "bad letter"

getOppColor:: Color -> Color
getOppColor color = if (color == White) then Black else White

--initialBoard = V.fromList (map convert ("RNBQKBNR" ++ replicate 8 'P' ++ replicate 32 '.' ++ replicate 8 'p' ++ "rnbqkbnr" ))
--initialBoard = V.fromList (map convert ("RNB.KBN." ++ replicate 47 '.' ++ "R..k....." ))
initialBoard = V.fromList (map convert ("....KB.qb..P...P.P....P...Pb..............n.....ppp..Pppr...Qk.r"))




moveWithoutAssertL:: Checkboard -> (Int, Int) -> Checkboard
moveWithoutAssertL x m
  | to > 55 && from < 56 && from > 40 && movedPiece == (CPiece Pawn White)  = Checkboard (vector V.// [(to, (CPiece Queen White)),(from, Empty)]) oppColor status
  | to < 8 && from < 16 && from > 7 && movedPiece == (CPiece Pawn Black)  = Checkboard (vector V.// [(to, (CPiece Queen Black)),(from, Empty)]) oppColor status
  | otherwise =  Checkboard (vector V.// [(to, movedPiece),(from, Empty)]) oppColor status
  where from = fst m
	to = snd m
	movedPiece = vector V.! (from)
	vector = board x
	oppColor = if (whoNext x == White) then Black else White
	status = computeStatus x


moveWithoutAssert:: Checkboard -> (Int, Int) -> Checkboard
moveWithoutAssert x m = Checkboard (board middleBoard) (whoNext middleBoard) (computeStatus middleBoard)
   where middleBoard = moveWithoutAssertL x m

computeStatus :: Checkboard -> Status
computeStatus x
   | isAnyPossible =  InProgress
   | not isAnyPossible && isInCheck x whoN &&  whoN == White = BlackWon
   | not isAnyPossible &&  isInCheck x whoN &&  whoN == Black = WhiteWon
   | not isAnyPossible &&  isInCheck x whoN = Draw
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
        oppColor = if (color == White) then Black else White
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
