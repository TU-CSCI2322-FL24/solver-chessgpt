import Data.Ord

data Move = Move Team Team
data Team = White Piece | Black Piece--this may have been a mistake... if there's a way to make a call regardless of constructor we can cut a lot of the spaghetti below
--see if we can get the ascii characters in: https://www.alt-codes.net/chess-symbols.php
data Piece = Pawn Position | Rook Position | Knight Position | Bishop Position | Queen Position | King Position

type Position = (Int,Int)
type Game     = ([Team],[Team])
type Winner   = Team

opposite :: ([Team],[Team]) -> Team -> [Team]
opposite (whites,blacks) (White _) = blacks
opposite (whites,blacks) (Black _) = whites

replacePiece :: Eq Team => [Team] -> Team -> Team -> [Team]
replacePiece pieces old new = new:[piece | piece <- pieces, piece /= old]

move :: Eq Team => Eq Piece => Game -> Move -> Game--For every single one of these godforsaken functions that uses Eq, they compile but throw errors in ghci. No clue how to bypass this.
move game@(whites,blacks) (Move old new) 
  | old `elem` whites = if (canMake game (getPiece old) (getPosition new)) then (replacePiece whites old new,blacks) else error "can't move there"
  | old `elem` blacks = if (canMake game (getPiece old) (getPosition new)) then (whites,replacePiece blacks old new) else error "can't move there"
  | otherwise         = error "piece be upon you"

--will need refining for special cases; will need to write a function to block all pieces except knight when obstructed
canMake :: Eq Piece => Game -> Piece -> Position -> Bool
canMake _ (Pawn (x1,y1)) (x2,y2)      = (x2==x1)&&(y2==(y1+1)) --add diagonals to take pieces and the second space for the first move; also need a transformation function for when they reach the end of the board
canMake _ (Rook (x1,y1)) (x2,y2)      = (x2==x1)||(y2==y1) --what the hell is castling
canMake _ (Knight (x1,y1)) (x2,y2)    = ((x2==(x1+3))&&(y2==(y1+1)))||((x2==(x1+3))&&(y2==(y1-1)))||((x2==(x1-3))&&(y2==(y1+1)))||((x2==(x1-3))&&(y2==(y1-1)))||((x2==(x1+1))&&(y2==(y1+3)))||((x2==(x1-1))&&(y2==(y1+3)))||((x2==(x1+1))&&(y2==(y1-3)))||((x2==(x1-1))&&(y2==(y1-3)))
canMake _ (Bishop (x1,y1)) (x2,y2)    = (abs (y2-y1))==(abs (x2-x1))
canMake _ (Queen (x1,y1)) (x2,y2)     = ((y2-y1)==(x2-x1))||(x2==x1)||(y2==y1)
canMake game (King (x1,y1)) (x2,y2)   = (True{-this should be a case to ensure the king doesn't move into check, but I'm having trouble implementing it-})&&(((x2==(x1+1))&&(y2==(y1+1)))||((x2==x1)&&(y2==(y1+1)))||((x2==(x1+1))&&(y2==y1))||((x2==(x1-1))&&(y2==(y1-1)))||((x2==(x1-1))&&(y2==y1))||((x2==x1)&&(y2==(y1-1))))

getPiece :: Team -> Piece
getPiece (White piece) = piece
getPiece (Black piece) = piece

getPiecePosition :: Piece -> Position--I hate this please cull it
getPiecePosition (Pawn (x,y))   = (x,y)
getPiecePosition (Knight (x,y)) = (x,y)
getPiecePosition (Rook (x,y))   = (x,y)
getPiecePosition (Bishop (x,y)) = (x,y)
getPiecePosition (Queen (x,y))  = (x,y)
getPiecePosition (King (x,y))   = (x,y)

getPosition :: Team -> Position
getPosition piece = getPiecePosition (getPiece piece)

danger :: Eq Piece =>Game -> Team -> Bool
danger game piece = not $ null [op | op <- (opposite game piece), canMake game (getPiece op) (getPosition piece)]

possibleMoves :: Game -> Piece -> [Move]--needs to account for blockage, likely means we'll have to change both this and canMake's inputs to Team
possibleMoves game (Pawn (x,y))   = undefined--[Move (x,y) (x,y+1),(x,y+2),canMake]
possibleMoves game (Rook (x,y))   = undefined--[Move (x,y) (x,ys) | ys <- [1..8],canMake]++[Move (x,y) (xs,y) | xs <- [1..8],canMake]
possibleMoves game (Knight (x,y)) = undefined--no
possibleMoves game (Bishop (x,y)) = undefined--Not sure how to get this one
possibleMoves game (Queen (x,y))  = undefined--(possibleMoves (Bishop (x,y)))++(possibleMoves (Rook (x,y)))
possibleMoves game (King (x,y))   = undefined--like Queen but only 1 space radius
  
safeMoves :: Eq Piece => Game -> Team -> [Move]
safeMoves game piece = [move | move <- (possibleMoves game (getPiece piece)), not $ danger game piece]

check :: Eq Piece => Game -> Team -> Bool
check game piece@(White (King _)) = danger game piece
check game piece@(Black (King _)) = danger game piece --can we combine these functions, possibly using opposite?
check _ _ = False --non kings can't be in check

--could probably combine check and checkmate with another data type, not sure if worthwhile
checkMate :: Eq Piece => Game -> Team -> Bool
checkMate game piece = (check game piece)&&(null $ possibleMoves game (getPiece piece))