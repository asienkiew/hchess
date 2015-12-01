module Structures where

import Data.Char (ord)
import Data.Char (toLower)
import Data.List.Split (splitEvery)
import Control.Parallel.Strategies
import qualified Data.Vector as V
import qualified Data.Map as Map
import  Data.Int(Int32)

import Const



type ChHash = Int32

data Piece = Queen | King | Rook | Bishop | Knight | Pawn deriving (Eq, Show, Enum, Ord)
data Color = White | Black deriving (Eq, Show, Enum, Ord)
data CPiece = CPiece {p :: Piece, c ::  Color }  | Empty deriving (Eq)

data Checkboard = Checkboard {
           board :: V.Vector CPiece , 
           whoNext :: Color, 
           status :: Status,
           history :: [Checkboard],
	   movesNoAttackNoPawn :: Int, --number of moves with attack and pawn
           historyMap :: Map.Map ChHash Int } deriving( Eq)
instance NFData Checkboard
data Status = Draw | WhiteWon | BlackWon | InProgress deriving (Eq)


instance Show Checkboard where
    show a = checkboardToStr a

instance Show CPiece where
    show a =  [convertToChar a]

instance Show Status where
    show Draw =  "Draw"
    show WhiteWon =  "The End - White won"
    show BlackWon =  "The End - Black won"
    show InProgress =  "Game is on progress"
    

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
    | otherwise = error "bad piece"

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
