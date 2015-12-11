module Const where

import qualified Data.Vector as V

game_tree_depth = 3 :: Int
deepenOnAttack = True
no_attack_no_pawn_move_limit = 50 :: Int

standard_board_string  = "RNBQKBNRPPPPPPPP................................pppppppprnbqkbnr"
white_won_board_string = "RNB.KBN................................................R..k....."
black_won_board_string = "....KB.qb..P...P.P....P...Pb..............n.....ppp..Pppr...Qk.r"
promotion_board_string = "RNB.KBN...............................................PR..k....."

start_board_string = standard_board_string


rows = ['a'..'h']
columns = ['1'..'8']
numList =  [0 :: Int .. 63 :: Int]
numVec = V.fromList numList

whitePawnEval :: V.Vector Int
whiteKnightEval :: V.Vector Int
whiteBishopEval :: V.Vector Int
whiteRookEval :: V.Vector Int
whiteQueenEval :: V.Vector Int
whiteKingMiddleEval :: V.Vector Int
whiteKingEndEval :: V.Vector Int
whitePawnEval = V.fromList $ reverse [  0,  0,  0,  0,  0,  0,  0,  0,50, 50, 50, 50, 50, 50, 50, 50,10, 10, 20, 30, 30, 20, 10, 10, 5,  5, 10, 25, 25, 10,  5,  5, 0,  0,  0, 20, 20,  0,  0,  0, 5, -5,-10,  0,  0,-10, -5,  5, 5, 10, 10,-20,-20, 10, 10,  5, 0,  0,  0,  0,  0,  0,  0,  0]
whiteKnightEval =  V.fromList $ reverse [ -50,-40,-30,-30,-30,-30,-40,-50,-40,-20,  0,  0,  0,  0,-20,-40,-30,  0, 10, 15, 15, 10,  0,-30,-30,  5, 15, 20, 20, 15,  5,-30,-30,  0, 15, 20, 20, 15,  0,-30,-30,  5, 10, 15, 15, 10,  5,-30,-40,-20,  0,  5,  5,  0,-20,-40,  -50,-40,-30,-30,-30,-30,-40,-50]
whiteBishopEval =  V.fromList $ reverse [-20,-10,-10,-10,-10,-10,-10,-20,-10,  0,  0,  0,  0,  0,  0,-10,-10,  0,  5, 10, 10,  5,  0,-10,-10,  5,  5, 10, 10,  5,  5,-10,-10,  0, 10, 10, 10, 10,  0,-10,-10, 10, 10, 10, 10, 10, 10,-10,-10,  5,  0,  0,  0,  0,  5,-10,-20,-10,-10,-10,-10,-10,-10,-20]
whiteRookEval =  V.fromList $ reverse [0,  0,  0,  0,  0,  0,  0,  0,  5, 10, 10, 10, 10, 10, 10,  5, -5,  0,  0,  0,  0,  0,  0, -5, -5,  0,  0,  0,  0,  0,  0, -5, -5,  0,  0,  0,  0,  0,  0, -5, -5,  0,  0,  0,  0,  0,  0, -5, -5,  0,  0,  0,  0,  0,  0, -5,  0,  0,  0,  5,  5,  0,  0,  0]
whiteQueenEval =  V.fromList $ reverse [-20,-10,-10, -5, -5,-10,-10,-20,-10,  0,  0,  0,  0,  0,  0,-10,-10,  0,  5,  5,  5,  5,  0,-10, -5,  0,  5,  5,  5,  5,  0, -5,  0,  0,  5,  5,  5,  5,  0, -5,-10,  5,  5,  5,  5,  5,  0,-10,-10,  0,  5,  0,  0,  0,  0,-10,-20,-10,-10, -5, -5,-10,-10,-20]
whiteKingMiddleEval =  V.fromList $ reverse [-30,-40,-40,-50,-50,-40,-40,-30,-30,-40,-40,-50,-50,-40,-40,-30,-30,-40,-40,-50,-50,-40,-40,-30,-30,-40,-40,-50,-50,-40,-40,-30,-20,-30,-30,-40,-40,-30,-30,-20,-10,-20,-20,-20,-20,-20,-20,-10, 20, 20,  0,  0,  0,  0, 20, 20, 20, 30, 10,  0,  0, 10, 30, 20]
whiteKingEndEval =  V.fromList $ reverse [-50,-40,-30,-20,-20,-30,-40,-50,-30,-20,-10,  0,  0,-10,-20,-30,-30,-10, 20, 30, 30, 20,-10,-30,-30,-10, 30, 40, 40, 30,-10,-30,-30,-10, 30, 40, 40, 30,-10,-30,-30,-10, 20, 30, 30, 20,-10,-30,-30,-30,  0,  0,  0,  0,-30,-30,-50,-30,-30,-30,-30,-30,-30,-50]

