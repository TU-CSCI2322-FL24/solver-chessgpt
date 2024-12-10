module ChessGPT where
import Chess
import Text.Read
import Data.Maybe
import Data.List
import Data.Ord

type Rating = Int 

whoWillWin :: Game -> Winner
whoWillWin game@(team, pieces, count) = 
    case winner game of
        Just w -> w
        Nothing ->  if any (== (Victor team)) outcomes then (Victor team)
                    else if any (== Stalemate) outcomes then Stalemate
                    else (Victor (oppositeTeam team))
            where validMoves = possibleGameMoves game
                  outcomes = [whoWillWin nextGame | nextMove <- validMoves, let Just nextGame = move game nextMove]

bestMove :: Game -> Move
bestMove game@(team, pieces, count) = if not $ null winnings then w else if not $ null ties then t else a
    where outputs = [(whoWillWin newGame, newMove) | newMove <- possibleGameMoves game, let Just newGame = move game newMove]        
          winnings@(w:_) = [theMove | (winner, theMove) <- outputs, winner == (Victor team)]
          ties@(t:_) = [theMove | (winner, theMove) <- outputs, winner == Stalemate]
          (a:_) = [theMove | (_, theMove) <- outputs]

whoMightWin :: Game -> Int -> Rating
whoMightWin game 0 = rateGame game 
{- couldn't figure out this merge conflict - mp
whoMightWin game@(team, _, _) depth 
    | team == White && null scores = 1000
    | team == Black && null scores = -1000
    | team == White = maximum scores
    | otherwise = minimum scores
    where scores = [whoMightWin newGame (depth - 1) | newMove <- possibleGameMoves game, let Just newGame = move game newMove]

goodMove :: Game -> Int -> Move
goodMove game@(team, pieces, count) depth = --if team == White then snd (maximum outputs) else snd (minimum outputs)
      if team == White then snd (maximumBy (comparing fst) outputs) else snd (minimumBy (comparing fst) outputs)
-}
whoMightWin game@(team, _, _) depth =
    if victor == Just (Victor White) then 1000 else if victor == Just (Victor Black) then -1000 else if team == White then maximum scores else minimum scores
    where scores = [whoMightWin newGame (depth-1) | newMove <- possibleGameMoves game, let Just newGame = move game newMove]
          victor = winner game

goodMove :: Game -> Int -> Move
goodMove game@(team, pieces, count) depth = if team == Black then snd (minimumBy (comparing fst) outputs) else snd (maximumBy (comparing fst) outputs)
    where outputs = [(whoMightWin newGame depth, newMove) | newMove <- possibleGameMoves game, let Just newGame = move game newMove]

rateGame :: Game -> Rating
rateGame game@(_,pieces,_) = 
    case winner game of
      Just Stalemate -> 0
      Just (Victor Black) -> -1000
      Just (Victor White) -> 1000
      Nothing -> wMaterial - bMaterial
    where wMaterial = sum [pieceValue pType | (_, (pType, team)) <- pieces, team == White]
          bMaterial = sum [pieceValue pType | (_, (pType, team)) <- pieces, team == Black]

pieceValue :: PieceType -> Int
pieceValue pType = if pType == Pawn then 1 
                   else if pType == Rook then 5
                   else if pType == Knight then 3
                   else if pType == Bishop then 3
                   else if pType == Queen then 9
                   else 1000

{-spaceControlled :: Game -> Int
spaceControlled (_,pieces,int) = (length (possibleGameMoves (White,pieces,int))) - (length (possibleGameMoves (Black,pieces,int)))--int is useless here?-}

{-pawnStructure :: Game -> Int
pawnStructure game = wStructure - bStructure
    where (_,pieces,_) = game
          wPawns = [(pos, (pType, team)) | (pos, (pType, team)) <- pieces, pType == Pawn, team == White]
          bPawns = [(pos, (pType, team)) | (pos, (pType, team)) <- pieces, pType == Pawn, team == Black]

          leftWPawn (x,y) = (getPiece game (x-1,y+1)) == Maybe ((x-1,y+1), (Pawn, White))
          rightWPawn (x,y) = (getPiece game (x+1,y+1)) == Maybe ((x+1,y+1), (Pawn, White))
          wHelper (x,y) = if leftWPawn (x,y) && rightWPawn (x,y) then 2 else if leftWPawn (x,y) || rightWPawn (x,y) then 1 else 0
          wStructure = sum [bHelper(x,y) | ((x,y), (_,_)) <- wPawns]

          leftBPawn (x,y) = (getPiece game (x-1,y-1)) == Maybe ((x-1,y-1), (Pawn, Black))
          rightBPawn (x,y) = (getPiece game (x+1,y-1)) == Maybe ((x+1,y-1), (Pawn, Black))
          bHelper (x,y) = if leftBPawn (x,y) && rightBPawn (x,y) then 2 else if leftBPawn (x,y) || rightBPawn (x,y) then 1 else 0
          bStructure = sum [bHelper(x,y) | ((x,y), (_,_)) <- bPawns]-}
          

{-adjacentTeamPieces :: Game -> Piece -> [Pieces]
adjacentTeamPieces game piece = if getPieceTeam piece == white then length [pos | pos <- adjacentPositions, (getPiece game pos) /= null]--need to only check for one team somehow
                                else 
                where (pieceX,pieceY) = getPosition piece
                      adjacentPositions = [(pieceX-1, pieceY+1),(pieceX, pieceY+1),(pieceX+1, pieceY+1),(pieceX-1, pieceY)
                                       ,(pieceX+1, pieceY),(pieceX-1, pieceY-1),(pieceX, pieceY-1),(pieceX+1, pieceY-1)]-}

{-kingSafety :: Game -> Int--at least 2 pawns in front of king
kingSafety game = (if wKingDefense == 3 then 2 else wKingDefense) - (if bKingDefense == 3 then 2 else bKingDefense)
    where (_,pieces,_) = game
          wKing = [(pos, (pType, team)) | (pos, (pType, team)) <- pieces, pType == King, team == White]
          bKing = [(pos, (pType, team)) | (pos, (pType, team)) <- pieces, pType == King, team == Black]
          leftWPawn (x,y) = if (getPiece game (x-1,y+1)) == Maybe ((x-1,y+1), (Pawn, White)) then 1 else 0
          middleWPawn (x,y) = if (getPiece game (x,y+1)) == Maybe ((x,y+1), (Pawn, White)) then 1 else 0
          rightWPawn (x,y) = if (getPiece game (x+1,y+1)) == Maybe ((x+1,y+1), (Pawn, White)) then 1 else 0
          wKingDefense = leftWPawn (getPosition wKing) + middleWPawn (getPosition wKing) + rightWPawn (getPosition wKing)
          leftBPawn (x,y) = if (getPiece game (x-1,y-1)) == Maybe ((x-1,y-1), (Pawn, Black)) then 1 else 0
          middleBPawn (x,y) = if (getPiece game (x,y-1)) == Maybe ((x,y-1), (Pawn, Black)) then 1 else 0
          rightBPawn (x,y) = if (getPiece game (x+1,y-1)) == Maybe ((x+1,y-1), (Pawn, Black)) then 1 else 0
          bKingDefense = leftBPawn (getPosition bKing) + middleBPawn (getPosition bKing) + rightBPawn (getPosition bKing)-}


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

parseTeam :: String -> Maybe Team
parseTeam str = do
    team <- if str == "w" then Just White else if str == "b" then Just Black else Nothing
    return team

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
showGame game@(turn, pieces, turns) = 
    intercalate "\n" [showTurn turn, show turns, unwords piecesStrs]
  where showTurn :: Team -> String
        showTurn White = "w"
        showTurn Black = "b"
        piecesStrs = [showPiece (pType, pTeam) ++ showPos(x,y) | piece@((x,y), (pType, pTeam)) <- pieces]

writeGame :: Game -> FilePath -> IO ()
writeGame game path = do
    writeFile path (showGame game)
    return ()

loadGame :: FilePath -> IO (Maybe Game)
loadGame path = do
  str <- readFile path
  return $ readGame str

putBestMove :: Game -> IO ()
putBestMove game = putStrLn $ show $ bestMove game
