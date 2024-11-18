module ChessGPT where
import Chess

whoWillWin :: Game -> Winner--maybe use point system?https://www.chess.com/terms/chess-piece-value
whoWillWin game = undefined

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
pawnStructure game = 
    where wPawns = [(pos, (type, team)) | (pos, (type, team)) <- whites, type == Pawn]
          bPawns = [(pos, (type, team)) | (pos, (type, team)) <- whites, type == Pawn]
          wStructure = length [x | x <- wPawns, ]--use adjacentTeamPieces
          bStructure = 

adjacentTeamPieces :: Game -> Piece -> [Pieces]
adjacentTeamPieces game piece = if getPieceTeam piece == white then length [pos | pos <- adjacentPositions, (getPiece game pos) /= null]--need to only check for one team somehow
                                else 
                where (pieceX,pieceY) = getPosition piece
                      adjacentPositions = [(pieceX-1, pieceY+1),(pieceX, pieceY+1),(pieceX+1, pieceY+1),(pieceX-1, pieceY)
                                       ,(pieceX+1, pieceY),(pieceX-1, pieceY-1),(pieceX, pieceY-1),(pieceX+1, pieceY-1)]

kingSafety :: Game -> Integer--just check how many friendly pieces are around the king for now using adjecentTeamPieces
kingSafety game = undefined




bestMove :: Game -> Move--will need helper functions, as well as a way to determine which team it's making a move for. whoWillWin may be helpful.
bestMove game = undefined

readGame :: String -> Game--just turn printGame inside out
readGame str = undefined
