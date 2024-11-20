module ChessGPT where
import Chess

whoWillWin :: Game -> Winner--maybe use point system?https://www.chess.com/terms/chess-piece-value
whoWillWin game = ((material game) * 10) + (spaceControlled game) + (pawnStructure game) + ((kingSafety game) * 6)

material :: Game -> Integer
material (_,whites,blacks) = wMaterial - bMaterial
    where wMaterial = sum [val | (_, (type, _)) <- whites
                      , if type == Pawn then 1 
                      else if type == Rook then 5
                      else if type == Knight then 3
                      else if type == Bishop then 3
                      else if type ==  Queen then 9
                      else 0]
          bMaterial = sum [val | (_, (type, _)) <- blacks
                      , if type == Pawn then 1 
                      else if type == Rook then 5
                      else if type == Knight then 3
                      else if type == Bishop then 3
                      else if type ==  Queen then 9
                      else 0]

spaceControlled :: Game -> Integer
spaceControlled (_,whites, blacks) = (length (possibleGameMoves (White,whites,blacks))) - (length (possibleGameMoves (Black,whites,blacks)))

pawnStructure :: Game -> Integer
pawnStructure game = wStructure - bStructure
    where (_, whites, blacks) = game
          wPawns = [(pos, (type, team)) | (pos, (type, team)) <- whites, type == Pawn]
          bPawns = [(pos, (type, team)) | (pos, (type, team)) <- blacks, type == Pawn]
          leftWPawn (x,y) = (getPiece game (x-1,y+1)) == maybe ((x-1,y+1), (Pawn, White))
          rightWPawn (x,y) = (getPiece game (x+1,y+1)) == maybe ((x+1,y+1), (Pawn, White))
          wStructure = sum [n | ((x,y) (_,_)) <- wPawns, if leftWPawn (x,y) && rightWPawn (x,y) then n = 2
                                                         else if leftWPawn (x,y) || rightWPawn (x,y) then n = 1
                                                         else n = 0]
          leftBPawn (x,y) = (getPiece game (x-1,y-1)) == maybe ((x-1,y-1), (Pawn, Black))
          rightBPawn (x,y) = (getPiece game (x+1,y-1)) == maybe ((x+1,y-1), (Pawn, Black))
          bStructure = sum [n | ((x,y) (_,_)) <- bPawns, if leftBPawn (x,y) && rightBPawn (x,y) then n = 2
                                                         else if leftBPawn (x,y) || rightBPawn (x,y) then n = 1
                                                         else n = 0]
          

{-adjacentTeamPieces :: Game -> Piece -> [Pieces]
adjacentTeamPieces game piece = if getPieceTeam piece == white then length [pos | pos <- adjacentPositions, (getPiece game pos) /= null]--need to only check for one team somehow
                                else 
                where (pieceX,pieceY) = getPosition piece
                      adjacentPositions = [(pieceX-1, pieceY+1),(pieceX, pieceY+1),(pieceX+1, pieceY+1),(pieceX-1, pieceY)
                                       ,(pieceX+1, pieceY),(pieceX-1, pieceY-1),(pieceX, pieceY-1),(pieceX+1, pieceY-1)]-}

kingSafety :: Game -> Integer--at least 2 pawns in front of king
kingSafety game = (if wKingDefense == 3 then 2 else wKingDefense) - (if bKingDefense == 3 then 2 else bKingDefense)
    where (_, whites, blacks) = game
          wKing = [(pos, (type, team)) | (pos, (type, team)) <- whites, type == King]
          bKing = [(pos, (type, team)) | (pos, (type, team)) <- blacks, type == King]
          leftWPawn (x,y) = if (getPiece game (x-1,y+1)) == maybe ((x-1,y+1), (Pawn, White)) then 1 else 0
          middleWPawn (x,y) = if (getPiece game (x,y+1)) == maybe ((x,y+1), (Pawn, White)) then 1 else 0
          rightWPawn (x,y) = if (getPiece game (x+1,y+1)) == maybe ((x+1,y+1), (Pawn, White)) then 1 else 0
          wKingDefense = leftWPawn (getPosition wKing) + middleWPawn (getPosition wKing) + rightWPawn (getPosition wKing)
          leftBPawn (x,y) = if (getPiece game (x-1,y-1)) == maybe ((x-1,y-1), (Pawn, Black)) then 1 else 0
          middleBPawn (x,y) = if (getPiece game (x,y-1)) == maybe ((x,y-1), (Pawn, Black)) then 1 else 0
          rightBPawn (x,y) = if (getPiece game (x+1,y-1)) == maybe ((x+1,y-1), (Pawn, Black)) then 1 else 0
          bKingDefense = leftBPawn (getPosition bKing) + middleBPawn (getPosition bKing) + rightBPawn (getPosition bKing)




bestMove :: Game -> Move--will need helper functions, as well as a way to determine which team it's making a move for. whoWillWin may be helpful.
bestMove game = undefined

readGame :: String -> Game--just turn printGame inside out
readGame str = undefined
