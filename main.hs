import Structures
import AI
import Control.Monad
import qualified Data.Vector as V
import Const
import qualified Data.Map as Map

--ghc --make  -O2  -threaded -o chess main.hs
-- ./main +RTS -N4

playGame ::  Checkboard  -> IO ()
playGame chBoard = do
    print chBoard
    --print $ historyMap chBoard
    case (status chBoard, whoNext chBoard) of 
        (InProgress, White) -> do 
            print $ whoNext chBoard
            command <- getLine            
            case (command, (length $ history chBoard) > 1, stringToMove command /= (0,0), (isMoveLegal chBoard (stringToMove command))) of
                ("r", True, _,_) -> do playGame $ history chBoard !! 1
                (_,_,True,True) -> do playGame $ moveWithoutAssert chBoard $ stringToMove command
                (_,_,_,_) -> do putStr  "illegal move"
                                playGame chBoard
        (InProgress, Black) -> do
            print $ whoNext chBoard	  
	    let bestMove = (snd $ getBestMove 0 chBoard)
	    putStr $ "Chosen Move: " ++ (moveToString bestMove)	    
	    playGame $ moveWithoutAssert chBoard bestMove
        (_, _) -> do
	    print $ status chBoard


--debug
--print $ foldl (++) [] (map (\from -> ((getPossibleMovesTable ((board chBoard) V.! from)) V.! from)) [0..63])
--moves = [(from, to) | from <- [0..63], to <- [0..63],  isMoveLegal chBoard (from, to)]
  
initialBoard = V.fromList $ map convert $ standard_board_string
preInitialCheckboard =  Checkboard initialBoard White InProgress [] 0 Map.empty
initialMap = Map.singleton (hashCheckboard preInitialCheckboard) 1
initialCheckboard = preInitialCheckboard {historyMap = initialMap}


main = playGame initialCheckboard