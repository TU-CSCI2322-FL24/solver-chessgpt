module ChessGPT where
import Chess
import Data.Maybe

whoWillWin :: Game -> Winner--maybe use point system?https://www.chess.com/terms/chess-piece-value
whoWillWin game = 
    case winner game of
        Just winner -> winner
        Nothing -> undefined

bestMove :: Game -> Move--will need helper functions, as well as a way to determine which team it's making a move for. whoWillWin may be helpful.
bestMove game = undefined

readGame :: String -> Game--just turn printGame inside out
readGame str = undefined
