module Const where

game_tree_depth = 3 :: Int
no_attack_no_pawn_move_limit = 50 :: Int

standard_board_string = "RNBQKBNR" ++ replicate 8 'P' ++ replicate 32 '.' ++ replicate 8 'p' ++ "rnbqkbnr"
white_won_board_string = "RNB.KBN." ++ replicate 47 '.' ++ "R..k....."
black_won_board_string = "....KB.qb..P...P.P....P...Pb..............n.....ppp..Pppr...Qk.r"
