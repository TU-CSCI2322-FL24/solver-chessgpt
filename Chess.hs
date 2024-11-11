import Data.Ord
import Data.List.Split
import Data.Maybe 

data Move = Move Piece Piece deriving (Show, Eq)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show,Eq)
data Team = White | Black deriving (Show,Eq)

type Position = (Int, Int)
-- type Game     = ([Piece],[Piece])
type Game = (Team, [Piece], [Piece])
data Winner   = Victor Team | Draw | None deriving (Show, Eq)
-- type Piece    = (PieceType,Team,Position)
type Piece = (Position, (PieceType, Team))

getPieceType :: Piece -> PieceType
getPieceType (_,(b,_)) = b

getPiece :: Game -> Position -> Maybe Piece -- takes a position and checks whether there is a piece there, if yes then returns Just Piece, if no then returns Nothing
getPiece (_, white, black) pos =
    case [piece | piece <- white ++ black, getPosition piece == pos] of
        (p:_)   -> Just p
        []      -> Nothing 

opposite :: Game -> Piece -> [Piece]
opposite (_, whites,blacks) (_, (_, White)) = blacks
opposite (_, whites,blacks) (_, (_, Black)) = whites

oppositeTeam :: Team -> Team
oppositeTeam White = Black
oppositeTeam Black = White

move :: Game -> Move -> Maybe Game
move game@(team, whites,blacks) (Move old new) 
  | old `elem` whites = if (canMake game old (getPosition new)) then Just (oppositeTeam team, replacePiece whites old new,blacks) else Nothing
  | old `elem` blacks = if (canMake game old (getPosition new)) then Just (oppositeTeam team, whites,replacePiece blacks old new) else Nothing
  | otherwise         = Nothing
    where replacePiece :: [Piece] -> Piece -> Piece -> [Piece]
          replacePiece pieces old new = new:[piece | piece <- pieces, piece /= old]

block :: Game -> Move -> Bool
block game (Move ((x1,y1), (Knight,_)) ((x2,y2), (Knight,_))) = False--will need to account for cases where the position is occupied by a member of the same team
block game (Move _ _) = undefined

inBounds :: Position -> Bool
inBounds (x,y) = x>0 && x<9 && y>0 && y<9--this will need to change if we change board size or indexing, ie starting at 0

--will need refining for special cases; will need to write a function to block all pieces except knight when obstructed (above)
canMake :: Game -> Piece -> Position -> Bool
canMake _ _ _ = True
-- canMake _ (Pawn,White,(x1,y1)) (x2,y2) = 
--   (inBounds (x2,y2))&&((x2==x1)&&(y2==(y1+1))) --add diagonals to take pieces and the second space for the first move; also need a transformation function for when they reach the end of the board
-- canMake _ (Pawn,Black,(x1,y1)) (x2,y2) = 
--   (inBounds (x2,y2))&&((x2==x1)&&(y2==(y1-1))) --white and black pawns move in opposite directions;see notes above for needed additions. This setup assumes black is at the top of the board
-- canMake _ (Rook,_,(x1,y1)) (x2,y2)     = 
--   (inBounds (x2,y2))&&((x2==x1)||(y2==y1)) --what the hell is castling; inBounds might be kinda redundant here
-- canMake _ (Knight,_,(x1,y1)) (x2,y2)   = 
--   (inBounds (x2,y2))&&
--   (((x2==(x1+3))&&(y2==(y1+1)))||
--    ((x2==(x1+a (x2,y2))&&((abs (y2-y1))==(abs (x2-x1)))
-- canMake _ (Queen,_,(x1,y1)) (x2,y2)    = 
--   (inBounds (x2,y2))&&(((y2-y1)==(x2-x1))||(x2==x1)||(y2==y1))
-- canMake game (King,team,(x1,y1)) (x2,y2)  = 
--   (inBounds (x2,y2))&&
--   (not $ check game (King,team,(x2,y2)))&&
--   (((x2==(x1+1))&&(y2==(y1+1)))||
--   ((x2==x1)&&(y2==(y1+1)))||
--   ((x2==(x1+1))&&(y2==y1))||
--   ((x2==(x1-1))&&(y2==(y1-1)))||
--   ((x2==(x1-1))&&(y2==y1))||
--   ((x2==x1)&&(y2==(y1-1))))

getPosition :: Piece -> Position
getPosition ((x, y), (_, _)) = (x,y)

danger :: Game -> Piece -> Bool
danger game piece@(pos, (_, _)) = not $ null [op | op <- (opposite game piece), canMake game op pos]

promote :: Piece -> PieceType -> Piece--unsure how to implement - will likely need user input for new type, unless we just make it automatically a queen?
promote ((x, 8), (Pawn, White)) = ((x, 8), (Queen, White))
promote ((x, 1), (Pawn, Black)) = ((x, 1), (Queen, Black))
promote piece _ = piece

possibleMoves :: Game -> Piece -> [Move]
possibleMoves game piece@((x, y), (Pawn, team))   = 
  [Move piece (move, (Pawn, team)) | move <- moves, canMake game piece move]
    where moves = [(x,y+1),(x,y+2),(x,y-1),(x,y-2)]
possibleMoves game piece@((x, y), (Rook, team))   = 
  [Move ((x, y), (Rook,team) ) ((x, ys), (Rook,team)) | ys <- [1..8], canMake game piece (x,ys)] ++
  [Move ((x, y), (Rook,team) ) ((xs, y), (Rook,team)) | xs <- [1..8], canMake game piece (xs,y)]
possibleMoves game piece@((x,y), (Knight, team)) = 
  [Move piece (move, (Knight,team)) | move <- moves,canMake game piece move]
    where moves = [(x+3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y+3),(x-3,y+1),(x+1,y-3),(x-1,y-3),(x-3,y-1)]
possibleMoves game piece@((x, y), (Bishop, team))    = 
  [Move piece (move, (Bishop, team)) | move <- moves,canMake game piece move]--ensures moves are in bounds
    where moves = [(x+1,y+1),(x+2,y+2),(x+3,y+3),(x+4,y+4),(x+5,y+5),(x+6,y+6),(x+7,y+7),(x+8,y+8),(x-1,y-1),(x-2,y-2),(x-3,y-3),(x-4,y-4),(x-5,y-5),(x-6,y-6),(x-7,y-7),(x-8,y-8)]
possibleMoves game ((x,y), (Queen, team))        = 
  (possibleMoves game ((x,y), (Rook, team))) ++ (possibleMoves game ((x,y), (Bishop,team)))
possibleMoves game piece@((x,y), (King, team))   = 
  [Move piece (move, (King, team)) | move <- moves,canMake game piece move]
    where moves = [(x,y+1),(x+1,y),(x,y-1),(x-1,y),(x+1,y+1),(x+1,y-1),(x-1,y-1),(x-1,y+1)]
  
safeMoves :: Game -> Piece -> [Move]
safeMoves game piece = [move | move@(Move old new) <- (possibleMoves game piece), not $ danger game new]

possibleGameMoves :: Game -> [Move]
possibleGameMoves game@(team, whites, blacks) = 
  let pieces = if team == White then whites else blacks
  in concat [possibleMoves game p | p <- pieces]

check :: Game -> Piece -> Bool
check game piece@(_, (King, _)) = danger game piece
check game _ = False

--could probably combine check and checkmate with another data type, not sure if worthwhile
checkmate :: Game -> Piece -> Bool
checkmate game piece = (check game piece) && (null $ possibleMoves game piece)

take :: Game -> Move -> Game
take game move = undefined

--the winner function is being difficult, pls help
winner :: Game -> Winner
winner game@(_, whites, blacks)--should somehow account for stalemates - likely best to consider which combinations of pieces cannot force checkmate and make those cases
  | checkmate game (head [piece | piece <- whites,(getPieceType piece)==King]) = Victor Black
  | checkmate game (head [piece | piece <- blacks,(getPieceType piece)==King]) = Victor White
  | otherwise = None

pieceToString :: Piece -> String
pieceToString (_, (Pawn, Black))   = "♙"
pieceToString (_, (Pawn, White))   = "♟"
pieceToString (_, (Rook, Black))   = "♖"
pieceToString (_, (Rook, White))   = "♜"
pieceToString (_, (Knight, Black)) = "♘"
pieceToString (_, (Knight, White)) = "♞"
pieceToString (_, (Bishop, Black)) = "♗"
pieceToString (_, (Bishop, White)) = "♝"
pieceToString (_, (Queen, Black))  = "♕"
pieceToString (_, (Queen, White))  = "♛"
pieceToString (_, (King, Black))   = "♔"
pieceToString (_, (King, White))   = "♚"


toString :: Game -> String
toString game = unlines [rowString y game | y <- [8,7..1]]
    where rowString y game = unwords [cellString (x, y) game | x <- [1..8]]
          cellString pos game = maybe "." pieceToString (getPiece game pos)

-- Written with the help of ChatGPT
initialGame :: Game
initialGame = (White, whitePieces, blackPieces)
  where
    whitePawns   = [((x, 2), (Pawn, White)) | x <- [1..8]]
    whitePieces = [
        ((1, 1), (Rook, White)), ((2, 1), (Knight, White)), ((3, 1), (Bishop, White)),
        ((4, 1), (Queen, White)), ((5, 1), (King, White)),
        ((6, 1), (Bishop, White)), ((7, 1), (Knight, White)), ((8, 1), (Rook, White))
      ] ++ whitePawns

    blackPawns   = [((x, 7), (Pawn, Black)) | x <- [1..8]]
    blackPieces = [
        ((1, 8), (Rook, Black)), ((2, 8), (Knight, Black)), ((3, 8), (Bishop, Black)),
        ((4, 8), (Queen, Black)), ((5, 8), (King, Black)),
        ((6, 8), (Bishop, Black)), ((7, 8), (Knight, Black)), ((8, 8), (Rook, Black))
      ] ++ blackPawns


printGame :: Game -> IO ()--written with the help of ChatGPT
printGame game = putStrLn $ toString game
