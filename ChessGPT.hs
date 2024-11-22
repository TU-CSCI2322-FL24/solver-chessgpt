module ChessGPT where
import Chess

whoWillWin :: Game -> Winner
whoWillWin game = if (((material game) * 10) + (spaceControlled game) + (pawnStructure game) + ((kingSafety game) * 6)) < 0 then Victor Black else Victor White

material :: Game -> Int
material (_,pieces,_) = wMaterial - bMaterial
    where wMaterial = sum [pieceValue pType | (_, (pType, team)) <- pieces, team == White]
          bMaterial = sum [pieceValue pType | (_, (pType, team)) <- pieces, team == Black]

pieceValue :: PieceType -> Int
pieceValue pType = if pType == Pawn then 1 
                   else if pType == Rook then 5
                   else if pType == Knight then 3
                   else if pType == Bishop then 3
                   else if pType == Queen then 9
                   else 100

spaceControlled :: Game -> Int
spaceControlled (_,pieces,int) = (length (possibleGameMoves (White,pieces,int))) - (length (possibleGameMoves (Black,pieces,int)))--int is useless here?

pawnStructure :: Game -> Int
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
          bStructure = sum [bHelper(x,y) | ((x,y), (_,_)) <- bPawns]
          

{-adjacentTeamPieces :: Game -> Piece -> [Pieces]
adjacentTeamPieces game piece = if getPieceTeam piece == white then length [pos | pos <- adjacentPositions, (getPiece game pos) /= null]--need to only check for one team somehow
                                else 
                where (pieceX,pieceY) = getPosition piece
                      adjacentPositions = [(pieceX-1, pieceY+1),(pieceX, pieceY+1),(pieceX+1, pieceY+1),(pieceX-1, pieceY)
                                       ,(pieceX+1, pieceY),(pieceX-1, pieceY-1),(pieceX, pieceY-1),(pieceX+1, pieceY-1)]-}

kingSafety :: Game -> Int--at least 2 pawns in front of king
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
          bKingDefense = leftBPawn (getPosition bKing) + middleBPawn (getPosition bKing) + rightBPawn (getPosition bKing)




bestMove :: Game -> Move--will need helper functions, as well as a way to determine which team it's making a move for. whoWillWin may be helpful.
bestMove game = undefined

readGame :: String -> Game--just turn printGame inside out
readGame str = undefined
