module ChessGPT where
import Chess

whoWillWin :: Game -> Winner--maybe use point system?https://www.chess.com/terms/chess-piece-value
whoWillWin game = undefined

bestMove :: Game -> Move--will need helper functions, as well as a way to determine which team it's making a move for. whoWillWin may be helpful.
bestMove game = undefined

-- Format: currentTeam(b|w) [space] list of pieces((b|w)(p|q|k|r|b|n)(a..h)(1..8)) separated by commas
-- Example for Initial Game: w wra1,wnb1,wnc1,wbf1,wqc1,wrh1,wkc1,wpa2,wpb2,wpc2,wpd2,wpe2,wpf2,wpg2,wph2,
-- bra8,bnb8,bnc8,bnf8,bqc8,brh8,bkc8,bpa7,bpb7,bpc7,bpd7,bpe7,bpf7,bpg7,bph7
readGame :: String -> Maybe Game
readGame str = do
    (teamStr, piecesStr) <- case str of 
        (teamStr:piecesStr:_) -> Just (teamStr, piecesStr)
        _ -> Nothing
    team <- if teamStr == "w" then Just White else if teamStr == "b" then Just Black else Nothing
    

parsePiece :: String -> Maybe Piece
parsePiece str = do
    (teamStr, typeStr, xStr, yStr) <- case str of
        (teamStr:typeStr:xStr:yStr:_) -> Just (teamStr, typeStr, xStr, yStr)
        _ -> Nothing
    team <- parseTeam teamStr

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

parseTeam :: String -> Maybe Team
parseTeam str = do
    team <- if str == "w" then Just White else if str == "b" then Just Black else Nothing
    return team


initialGameStr :: String
initialGameStr = "w wra1,wnb1,wnc1,wbf1,wqc1,wrh1,wkc1,wpa2,wpb2,wpc2,wpd2,wpe2,wpf2,wpg2,wph2,bra8,bnb8,bnc8,bnf8,bqc8,brh8,bkc8,bpa7,bpb7,bpc7,bpd7,bpe7,bpf7,bpg7,bph7"
