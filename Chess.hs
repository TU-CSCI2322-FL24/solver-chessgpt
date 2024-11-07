import Data.Ord

data Move = Move Piece Piece
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show,Eq)
data Team = White | Black deriving (Show,Eq)

type Position = (Int,Int)
type Game     = ([Piece],[Piece])
type Winner   = Team
type Piece    = (PieceType,Team,Position)

opposite :: ([Piece],[Piece]) -> Piece -> [Piece]
opposite (whites,blacks) (_,White,_) = blacks
opposite (whites,blacks) (_,Black,_) = whites

move :: Game -> Move -> Game
move game@(whites,blacks) (Move old new) 
  | old `elem` whites = if (canMake game old (getPosition new)) then (replacePiece whites old new,blacks) else error "can't move there"
  | old `elem` blacks = if (canMake game old (getPosition new)) then (whites,replacePiece blacks old new) else error "can't move there"
  | otherwise         = error "piece be upon you"
    where replacePiece :: [Piece] -> Piece -> Piece -> [Piece]
          replacePiece pieces old new = new:[piece | piece <- pieces, piece /= old]

block :: Game -> Move -> Bool
block game (Move (Knight,_,(x1,y1)) (Knight,_,(x2,y2))) = False--will need to account for cases where the position is occupied by a member of the same team
block game (Move _ _) = undefined

inBounds :: Position -> Bool
inBounds (x,y) = x>0&&x<9&&y>0&&y<9--this will need to change if we change board size or indexing, ie starting at 0

--will need refining for special cases; will need to write a function to block all pieces except knight when obstructed (above)
canMake :: Game -> Piece -> Position -> Bool
canMake _ (Pawn,White,(x1,y1)) (x2,y2) = 
  (inBounds (x2,y2))&&((x2==x1)&&(y2==(y1+1))) --add diagonals to take pieces and the second space for the first move; also need a transformation function for when they reach the end of the board
canMake _ (Pawn,Black,(x1,y1)) (x2,y2) = 
  (inBounds (x2,y2))&&((x2==x1)&&(y2==(y1-1))) --white and black pawns move in opposite directions;see notes above for needed additions. This setup assumes black is at the top of the board
canMake _ (Rook,_,(x1,y1)) (x2,y2)     = 
  (inBounds (x2,y2))&&((x2==x1)||(y2==y1)) --what the hell is castling; inBounds might be kinda redundant here
canMake _ (Knight,_,(x1,y1)) (x2,y2)   = --Shaan wanted me to format it this way, if you guys think it's ugly you can revert it
  (inBounds (x2,y2))&&
  (((x2==(x1+3))&&(y2==(y1+1)))||
   ((x2==(x1+3))&&(y2==(y1-1)))||
   ((x2==(x1-3))&&(y2==(y1+1)))||
   ((x2==(x1-3))&&(y2==(y1-1)))||
   ((x2==(x1+1))&&(y2==(y1+3)))||
   ((x2==(x1-1))&&(y2==(y1+3)))||
   ((x2==(x1+1))&&(y2==(y1-3)))||
   ((x2==(x1-1))&&(y2==(y1-3))))
canMake _ (Bishop,_,(x1,y1)) (x2,y2)   = 
  (inBounds (x2,y2))&&((abs (y2-y1))==(abs (x2-x1)))--inBounds might be redundant here
canMake _ (Queen,_,(x1,y1)) (x2,y2)    = 
  (inBounds (x2,y2))&&(((y2-y1)==(x2-x1))||(x2==x1)||(y2==y1))
canMake game (King,team,(x1,y1)) (x2,y2)  = 
  (inBounds (x2,y2))&&
  (not $ check game (King,team,(x2,y2)))&&
  (((x2==(x1+1))&&(y2==(y1+1)))||
  ((x2==x1)&&(y2==(y1+1)))||
  ((x2==(x1+1))&&(y2==y1))||
  ((x2==(x1-1))&&(y2==(y1-1)))||
  ((x2==(x1-1))&&(y2==y1))||
  ((x2==x1)&&(y2==(y1-1))))

getPosition :: Piece -> Position
getPosition (_,_,(x,y)) = (x,y)

danger :: Game -> Piece -> Bool
danger game piece@(a,b,c) = not $ null [op | op <- (opposite game piece), canMake game piece c]

possibleMoves :: Game -> Piece -> [Move]
possibleMoves game piece@(Pawn,team,(x,y))   = 
  [Move piece (Pawn,team,move) | move <- moves,canMake game piece move]
    where moves = [(x,y+1),(x,y+2),(x,y-1),(x,y-2)]
possibleMoves game piece@(Rook,team,(x,y))   = 
  [Move (Rook,team,(x,y)) (Rook,team,(x,ys)) | ys <- [1..8],canMake game piece (x,ys)]++
  [Move (Rook,team,(x,y)) (Rook,team,(xs,y)) | xs <- [1..8],canMake game piece (xs,y)]
possibleMoves game piece@(Knight,team,(x,y)) = 
  [Move piece (Knight,team,move) | move <- moves,canMake game piece move]
    where moves = [(x+3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y+3),(x-3,y+1),(x+1,y-3),(x-1,y-3),(x-3,y-1)]--Jumpin Jesus Huckleberry Christ on a crutch, I have never hated a line of code this much
possibleMoves game piece@(Bishop,_,(x,y))    = undefined--Not sure how to get this one
possibleMoves game (Queen,team,(x,y))        = 
  (possibleMoves game (Rook,team,(x,y)))++(possibleMoves game (Bishop,team,(x,y)))
possibleMoves game piece@(King,team,(x,y))   = 
  [Move piece (King,team,move) | move <- moves,canMake game piece move]
    where moves = [(x,y+1),(x+1,y),(x,y-1),(x-1,y),(x+1,y+1),(x+1,y-1),(x-1,y-1),(x-1,y+1)]
  
safeMoves :: Game -> Piece -> [Move]
safeMoves game piece = [move | move@(Move old new) <- (possibleMoves game piece), not $ danger game new]

check :: Game -> Piece -> Bool
check game piece@(King,_,_) = danger game piece
check game (_,_,_) = False

--could probably combine check and checkmate with another data type, not sure if worthwhile
checkMate :: Game -> Piece -> Bool
checkMate game piece = (check game piece)&&(null $ possibleMoves game piece)

take :: Game -> Move -> Game
take game move = undefined

winner :: Game -> Maybe Winner
winner game@(whites,blacks) = --should somehow account for stalemates - likely best to consider which combinations of pieces cannot force checkmate and make those cases
  | any checkmate game [piece | piece <- whites,piece==(King,team,_)] = Just Black
  | any checkmate game [piece | piece <- blacks,piece==(King,team,_)] = Just White
  | otherwise = Nothing

toString :: Game -> String
toString game = undefined--use these: https://www.alt-codes.net/chess-symbols.php