module Hashing where

import System.Random
import Data.Bits (xor)
import qualified Data.Vector as V


import Structures 

randomValues :: [ChHash] 
randomValues = map fst $ scanl (\(r, gen) _ -> random gen) (random (mkStdGen 1)) $ repeat ()


hashTable :: V.Vector ChHash
hashTable = V.fromList $ take (6 * 2 * 64 + 1) randomValues  -- [White, Black]* [pieces]* [files] + whonext


cpieceOnPosToHash :: Int -> CPiece -> ChHash
cpieceOnPosToHash pos cpiece 
  | cpiece == Empty = 0
  | otherwise = hashTable V.! ((pieceInt * colorInt * (pos + 1)) - 1)
  where 
    pieceInt = pieceToInt (p cpiece)
    colorInt = colorToInt (c cpiece)
    pieceToInt Queen = 1
    pieceToInt King = 2
    pieceToInt Rook = 3
    pieceToInt Bishop = 4
    pieceToInt Knight = 5
    pieceToInt Pawn = 6
    colorToInt White = 1
    colorToInt Black = 2
    
    
hashCheckboard ::  Checkboard -> ChHash
hashCheckboard a = hashedBoard `xor` whoNextHash
  where hashedBoard = V.foldl (xor) 0 (V.imap cpieceOnPosToHash (board a))
        whoNextHash = if whoNext a == White then V.last hashTable else 0


hashCheckboardWithMove ::  ChHash -> (Int, Int) -> (CPiece, CPiece, CPiece) ->ChHash
hashCheckboardWithMove hash move (movedPiece, attackedPiece, reallyMovedPiece) =
          foldl (xor) 0 [hash, h from movedPiece, h to attackedPiece, h to reallyMovedPiece, V.last hashTable]
         where
           (from, to) =  move
           h = cpieceOnPosToHash