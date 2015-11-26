

import Structures
import AI
import Control.Monad
import qualified Data.Vector as V
import Const

{-
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

 -}
--ghc --make  -O2  -threaded -o chess main.hs
-- ./main +RTS -N4

moveInfinite ::  Checkboard  -> IO ()
moveInfinite a = do
  if (status a == InProgress)
     then do
         print a
         print $ whoNext a
         command <- getLine
         case (command, (length $ history a) > 1, stringToMove command /= (0,0), (isMoveLegal a (stringToMove command))) of
              ("r", True, _,_) -> do moveInfinite $ history a !! 1
              (_,_,True,True) -> do  print $  moveWithoutAssert a $ stringToMove command
              (_,_,_,_) -> do putStr  "illegal move"
                              moveInfinite a

         if (status (moveWithoutAssert a $ stringToMove command) == InProgress)
             then do
                 let bestMove = (snd $ getBestMove 0 (moveWithoutAssert a $ stringToMove command)  )
                 print $ "Chosen Move: " ++ (moveToString bestMove )
                 --print $ foldl (++) [] (map (\from -> ((getPossibleMovesTable ((board (moveWithoutAssert a $ stringToMove command)) V.! from)) V.! from)) [0..63])
                 moveInfinite $ moveWithoutAssert (moveWithoutAssert a $ stringToMove command) bestMove
             else do
                 print $ status (moveWithoutAssert a $ stringToMove command)
                 return()
         return()
    else do
         print a
	 print $ status a

    --    moves = [(from, to) | from <- [0..63], to <- [0..63],  isMoveLegal a (from, to)]

    
initialBoard = V.fromList $ map convert $ standard_board_string    
main = moveInfinite  (Checkboard initialBoard White InProgress [] 0)