module ChessGPT where
import Chess
import Data.Maybe
import Text.Read
import Data.List.Split
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

-- Format: currentTeam(b|w) [space] turnCounter [space] list of pieces((b|w)(p|q|k|r|b|n)(a..h)(1..8)) separated by commas
-- Example for Initial Game: w\n100\nwra1 wnb1 wbc1 wqd1 wke1 wbf1 wng1 wrh1 
-- wpa2 wpb2 wpc2 wpd2 wpe2 wpf2 wpg2 wph2 bra8 bnb8 bbc8 bqd8 bke8 bbf8 bng8 brh8 bpa7 bpb7 bpc7 bpd7 bpe7 bpf7 bpg7 bph7
readGame :: String -> Maybe Game
readGame str = do
    (teamStr, counterStr, piecesStr) <- case lines str of 
        (teamStr:counterStr:piecesStr:_) -> Just (teamStr, counterStr, piecesStr)
        _ -> Nothing
    team <- parseTeam teamStr
    pieces <- sequence [parsePiece pieceStr | pieceStr <- words piecesStr]
    counter <- readMaybe counterStr
    return (team, pieces, counter)

parsePiece :: String -> Maybe Piece
parsePiece str = do
    (teamStr, typeStr, xStr, yStr) <- case str of
        (teamStr:typeStr:xStr:yStr:_) -> Just ([teamStr], [typeStr], xStr, [yStr])
        _ -> Nothing
    team <- parseTeam teamStr
    pieceType <- parsePieceType typeStr
    x <- letterToNum xStr
    y <- readMaybe yStr
    return ((x, y), (pieceType, team))

parsePieceType :: String -> Maybe PieceType
parsePieceType str = do
    pieceType <- case str of
        "p" -> Just Pawn
        "r" -> Just Rook
        "b" -> Just Bishop
        "n" -> Just Knight
        "k" -> Just King
        "q" -> Just Queen
        _ -> Nothing
    return pieceType

showPiece :: (PieceType,Team) -> String
showPiece (Pawn,White)  = "wp"
showPiece (Pawn,Black)  = "bp"
showPiece (Rook,White)  = "wr"
showPiece (Rook,Black)  = "br"
showPiece (Knight,White) = "wn"
showPiece (Knight,Black) = "bn"
showPiece (Bishop,White) = "wb"
showPiece (Bishop,Black) = "bb"
showPiece (King,White)  = "wk"
showPiece (King,Black)  = "bk"
showPiece (Queen,White) = "wq"
showPiece (Queen,Black) = "bq"

showPos :: Position -> String
showPos (1,y) = "a" ++ show y
showPos (2,y) = "b" ++ show y
showPos (3,y) = "c" ++ show y
showPos (4,y) = "d" ++ show y
showPos (5,y) = "e" ++ show y
showPos (6,y) = "f" ++ show y
showPos (7,y) = "g" ++ show y
showPos (8,y) = "h" ++ show y

showGame :: Game -> String
showGame game@(turn, pieces, turns) = init $ unlines [showTurn turn, show turns, unwords [showPiece (pType, pTeam) ++ showPos(x,y) | piece@((x,y), (pType, pTeam)) <- pieces]]
  where showTurn :: Team -> String
        showTurn White = "w"
        showTurn Black = "b"

parseTeam :: String -> Maybe Team
parseTeam str = do
    team <- if str == "w" then Just White else if str == "b" then Just Black else Nothing
    return team

writeGame :: Game -> FilePath -> IO ()
writeGame game path = undefined

loadGame :: FilePath -> IO Game 
loadGame path = do
  str <- readFile path
  return $ fromJust $ readGame str

putBestMove :: Game -> IO ()
putBestMove game = putStrLn $ show $ bestMove game
