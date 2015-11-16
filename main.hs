import Data.Char (ord)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.List.Split (splitEvery)
import Data.List (intersect)
import qualified Data.Vector as V



podziel (a,b) = 
  case (a,b) of 
             (0,0)-> 0
             (_,1)-> a
             (_,0)-> error "dzielenie przez zero"
             (a,b)-> a/b

             
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!" 


factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  


tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 


--length' :: (Num b) => [a] -> b  -
--length' (_:xs) = 1 + length xs 

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  
  
  
  
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
  
  
data Piece = Queen | King | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
data CPiece = CPiece {p :: Piece, c ::  Color }  | Empty deriving (Eq)

data Checkboard = Checkboard {board :: V.Vector CPiece , whoNext :: Color}



instance Show CPiece where  
    show (CPiece Queen White) = "Q"
    show (CPiece King White) = "K"    
    show (CPiece Rook White) = "R"    
    show (CPiece Bishop White) = "B"    
    show (CPiece Knight White) = "N"      
    show (CPiece Pawn White) = "P"
    show (CPiece Queen Black) = "q"
    show (CPiece King Black) = "k"    
    show (CPiece Rook Black) = "r"    
    show (CPiece Bishop Black) = "b"    
    show (CPiece Knight Black) = "n"      
    show (CPiece Pawn Black) = "p" 
    show (Empty) = "."        
  
  
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

initialBoard = V.fromList (map convert ("RNBQKBNR" ++ replicate 8 'P' ++ replicate 32 '.' ++ replicate 8 'p' ++ "rnbqkbnr" ))

moveWithoutAssert:: Checkboard -> (Int, Int) -> Checkboard
moveWithoutAssert x m
  | to > 55 && from < 56 && from > 40 && movedPiece == (CPiece Pawn White)  = Checkboard (vector V.// [(to, (CPiece Queen White)),(from, Empty)]) oppColor
  | to < 8 && from < 16 && from > 7 && movedPiece == (CPiece Pawn Black)  = Checkboard (vector V.// [(to, (CPiece Queen Black)),(from, Empty)]) oppColor
  | otherwise =  Checkboard (vector V.// [(to, movedPiece),(from, Empty)]) oppColor
  where from = fst m
	to = snd m
	movedPiece = vector V.! (from)
	vector = board x
	oppColor = if (whoNext x == White) then Black else White
  
--initialCBoard =  (CPiece Rook White) (CPiece Knight White) ++ (CPiece Bishop White) ++ (CPiece Queen White) + (CPiece King White) 
--                ++ (CPiece Bishop White) ++ (CPiece Knight White) ++ (CPiece Rook White) 
--                ++  replicate 8 (CPiece Pawn White) ++ replicate 32 (Empty) ++ replicate 8 (CPiece Pawn Black)  
--                ++ (CPiece Rook Black) ++ (CPiece Knight Black) ++ (CPiece Bishop Black) ++ (CPiece Queen Black) + (CPiece King Black) 
--                ++ (CPiece Bishop Black) ++ (CPiece Knight Black) ++ (CPiece Rook Black) 
  
  

isMovePossible::  (Int, Int) -> CPiece -> Bool
isMovePossible move piece = isMovePossibleL  (fst move `div` 8) (fst move `mod` 8) (snd move `div` 8) (snd move `mod` 8) piece
  

isMovePossibleL::  Int -> Int -> Int -> Int -> CPiece -> Bool
isMovePossibleL fromY fromX  toY toX piece
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
isInCheck checkboard color
 | possibleAttacks == [] = False
 | otherwise = True
  where kingPos = fromJust $ V.findIndex (CPiece King color ==) vector 
        vector = board checkboard
        oppPositions = [x | x <- [0..63], vector V.! x /= Empty,  c (vector V.! x) == oppColor] 
        oppColor = if (color == White) then Black else White
        pawnPositions = [x | x <- oppPositions,  p (vector V.! x) == Pawn] 
        possibleAttacks = [(from, kingPos) | from <- oppPositions,  isMovePossible (from,  kingPos) (vector V.! from), not $ isPieceBetween vector (from,kingPos), 
                      [(kingPos - 8), (kingPos + 8)] `intersect` pawnPositions == [] ] --be sure to exlude Pawn nonattacking move 
        
willBeInCheck::Checkboard -> (Int, Int) -> Color -> Bool
willBeInCheck a b c = isInCheck (moveWithoutAssert a b) c

toMove ::  [Char] -> (Int,Int)
toMove x
  | length x /= 4 = err
  |  not (a `elem` rows) = err
  |  not (b `elem` columns) = err
  |  not (c `elem` rows) = err
  |  not (d `elem` columns) = err
  |  otherwise =   ((8 * (ord b - 49) + ord a - 97), (8 * (ord d - 49) + ord c - 97))
  where 
     rows = ['a'..'h']
     columns = ['1'..'8']
     err = error "bad format of Command"     
     a = toLower(x !! 0)
     b = toLower(x !! 1)
     c = toLower(x !! 2)
     d = toLower(x !! 3)
     
         
printBoard :: Checkboard  -> IO ()
printBoard checkboard 
  | V.length a /= 64 = error "Bad format"
  | otherwise  = putStr . concat . reverse $

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
      
      
moveInfinite ::  Checkboard  -> IO ()
moveInfinite a = do 
  printBoard a
  print $ whoNext a
  print $ length moves
  if moves == [] then print "end" else moveInfinite b
  where b = moveWithoutAssert a (moves !! 20)        
        moves = [(from, to) | from <- [0..63], to <- [0..63],  isMoveLegal a (from, to)]

        