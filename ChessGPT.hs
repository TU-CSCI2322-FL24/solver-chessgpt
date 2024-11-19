module ChessGPT where
import Chess
import Data.Maybe
--Fogarty said these functions weren't necessary in chess.hs, but that they might be part of whoWillWin
{-danger :: Game -> Position -> Team -> Bool
danger game pos team = not $ null [op | op <- getTeamPieces game (oppositeTeam team), canMake game op pos]
safeMoves :: Game -> Piece -> [Move]
safeMoves game piece = [move | move@(Move old new) <- (possibleMoves game piece), not $ danger game new (getPieceTeam piece)]
check :: Game -> Piece -> Bool
check game piece@(pos, (King, team)) = danger game pos team
check game _ = False
checkmate :: Game -> Team -> Bool
checkmate game team = (check game king) && (null $ possibleMoves game king)
  where king = getKing game team-}
whoWillWin :: Game -> Winner--maybe use point system?https://www.chess.com/terms/chess-piece-value
whoWillWin game = 
    case winner game of
        Just winner -> winner
        Nothing -> undefined

bestMove :: Game -> Move--will need helper functions, as well as a way to determine which team it's making a move for. whoWillWin may be helpful.
bestMove game = undefined

readGame :: String -> Game--just turn printGame inside out
readGame str = undefined
