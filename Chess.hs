module Chess where
import Data.Ord
import Data.List
import Data.List.Split
import Data.Maybe 

data Move = Move Piece Position deriving (Show, Eq)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show,Eq)
data Team = White | Black deriving (Show,Eq)

type Position = (Int, Int)
-- type Game     = ([Piece],[Piece])
type Game = (Team, [Piece], [Piece])
data Winner   = Victor Team | Stalemate | None deriving (Show, Eq)
-- type Piece    = (PieceType,Team,Position)
type Piece = (Position, (PieceType, Team))

getPieceType :: Piece -> PieceType
getPieceType (_,(b,_)) = b

getPieceTeam :: Piece -> Team
getPieceTeam (_,(_,c)) = c

getPosition :: Piece -> Position
getPosition ((x,y), (_,_)) = (x,y)

getTeamPieces :: Game -> Team -> [Piece]
getTeamPieces game@(_, whites, blacks) team
  | team == White = whites
  | otherwise = blacks

-- takes a position and checks whether there is a piece there, if yes then returns Just Piece, if no then returns Nothing
getPiece :: Game -> Position -> Maybe Piece 
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

replacePiece :: [Piece] -> Piece -> Piece -> [Piece]
replacePiece pieces old new = new:[piece | piece <- pieces, piece /= old]

move :: Game -> Move -> Maybe Game
move game@(team, whites, blacks) (Move old newPos)
  | not $ canMake game old newPos = Nothing -- Can't make move
  | getPieceTeam old /= team      = Nothing -- Attempted move is by the wrong team
  | otherwise = 
    case (getPiece game newPos) of 
      Just target -> Just $ if team == White then (newTeam, newWhites, (delete target newBlacks)) else (newTeam, (delete target newWhites), newBlacks)
      Nothing -> Just newGame -- Just a move, no pieces taken
    where newPiece = (newPos, (getPieceType old, getPieceTeam old))
          replacePiece pieces old new = new:(delete old pieces)
          newGame@(newTeam, newWhites, newBlacks) = if old `elem` whites then (Black, replacePiece whites old newPiece, blacks) else (White, whites, replacePiece blacks old newPiece)

inBounds :: Position -> Bool
inBounds (x,y) = x>0 && x<9 && y>0 && y<9 --this will need to change if we change board size or indexing, ie starting at 0

-- returns true if the diagonal is clear, false if it blocked
diagClear :: Game -> Position -> Position -> Bool
diagClear game (x1, y1) (x2, y2) = all (isNothing . getPiece game) positions
    where dx = signum (x2 - x1)
          dy = signum (y2 - y1)
          positions = takeWhile (/= (x2, y2)) $ tail [(x1 + i * dx, y1 + i * dy) | i <- [1..]]

-- returns true if the row/column is clear, false if is blocked
rowClear :: Game -> Position -> Position -> Bool
rowClear game (x1, y1) (x2, y2)
    | x1 == x2 = all (isNothing . getPiece game) [(x1, y) | y <- [min y1 y2 + 1 .. max y1 y2 - 1]]
    | y1 == y2 = all (isNothing . getPiece game) [(x, y1) | x <- [min x1 x2 + 1 .. max x1 x2 - 1]]
    | otherwise = False

-- returns true if the move can be made, and returns false if it cannot be made
-- checks whether a move is in the bounds of the board
-- checks that a move is valid for the given piece
-- checks that no piece of either color blocks the move
-- checks that the destination position is not currently occupied by a piece of the same color 
    -- if the destination is occupied by an opposite colored piece, returns true
canMake :: Game -> Piece -> Position -> Bool
canMake game ((x1, y1), (Pawn, White)) (x2, y2) =
    inBounds (x2, y2) &&
    (x2 == x1 && y2 == y1 + 1 && isNothing (getPiece game (x2, y2))) ||  -- regular one-step forward
    (x2 == x1 && y1 == 2 && y2 == 4 && isNothing (getPiece game (x2, 3)) && isNothing (getPiece game (x2, y2))) ||  -- initial two-step
    (abs (x2 - x1) == 1 && y2 == y1 + 1 &&  
    case getPiece game (x2, y2) of -- diagonal capture
        Just target -> getPieceTeam target == Black
        Nothing     -> False)
canMake game ((x1, y1), (Pawn, Black)) (x2, y2) =
    inBounds (x2, y2) &&
    (x2 == x1 && y2 == y1 - 1 && isNothing (getPiece game (x2, y2))) ||  -- regular one-step forward
    (x2 == x1 && y1 == 7 && y2 == 5 && isNothing (getPiece game (x2, 6)) && isNothing (getPiece game (x2, y2))) ||  -- initial two-step
    (abs (x2 - x1) == 1 && y2 == y1 - 1 &&
    case getPiece game (x2, y2) of -- diagonal capture
        Just target -> getPieceTeam target == White
        Nothing     -> False)
canMake game piece@((x1, y1), (Rook, _)) (x2, y2) =
    inBounds (x2, y2) &&
    ((x2 == x1 || y2 == y1) && rowClear game (x1, y1) (x2, y2)) &&  -- clear row/column
    canCapture game (x2, y2) piece
canMake game piece@((x1, y1), (Knight, _)) (x2, y2) =
    inBounds (x2, y2) &&
    ((abs (x2 - x1), abs (y2 - y1)) `elem` [(2, 1), (1, 2)]) &&
    canCapture game (x2, y2) piece
canMake game piece@((x1, y1), (Bishop, _)) (x2, y2) =
    inBounds (x2, y2) &&
    abs (x2 - x1) == abs (y2 - y1) &&  -- diagonal check
    diagClear game (x1, y1) (x2, y2) &&
    canCapture game (x2, y2) piece
canMake game piece@((x1, y1), (Queen, _)) (x2, y2) =
    inBounds (x2, y2) &&
    ((x2 == x1 || y2 == y1) && rowClear game (x1, y1) (x2, y2) ||  -- rook-like move
     abs (x2 - x1) == abs (y2 - y1) && diagClear game (x1, y1) (x2, y2)) &&  -- bishop-like move
    canCapture game (x2, y2) piece
canMake game piece@((x1, y1), (King, team)) (x2, y2) =
    inBounds (x2, y2) &&
    abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1 &&  -- one-square in any direction
    canCapture game (x2, y2) piece &&
    not (danger game (x1, y1) team)  -- ensure king is not moving into check

-- takes a game, position, and piece and checks whether the position is currently occupied 
    -- if the piece is the same color -> return false  
    -- if the piece is the opposite color -> return true
    -- if there is no piece -> return true
canCapture :: Game -> Position -> Piece -> Bool
canCapture game (x, y) og = 
    case getPiece game (x, y) of 
        Just target -> getPieceTeam target /= getPieceTeam og
        Nothing     -> True

danger :: Game -> Position -> Team -> Bool
danger game pos team = not $ null [op | op <- getTeamPieces game (oppositeTeam team), canMake game op pos]

promote :: Piece -> Piece--unsure how to implement - will likely need user input for new type, unless we just make it automatically a queen?
promote ((x, 8), (Pawn, White)) = ((x, 8), (Queen, White))
promote ((x, 1), (Pawn, Black)) = ((x, 1), (Queen, Black))
promote piece = piece

possibleMoves :: Game -> Piece -> [Move]
possibleMoves game piece@((x, y), (Pawn, team))   = 
  [Move piece move | move <- moves, canMake game piece move]
    where moves = [(x,y+1),(x,y+2),(x,y-1),(x,y-2)]
possibleMoves game piece@((x,y),(Rook,team))   = 
  [Move ((x,y), (Rook,team)) (x,ys) | ys <- [1..8], canMake game piece (x,ys)] ++
  [Move ((x,y), (Rook,team)) (xs,y) | xs <- [1..8], canMake game piece (xs,y)]
possibleMoves game piece@((x,y),(Knight, team)) = 
  [Move piece move | move <- moves,canMake game piece move]
    where moves = [(x+3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y+3),(x-3,y+1),(x+1,y-3),(x-1,y-3),(x-3,y-1)]
possibleMoves game piece@(Bishop,_,(x,y))    = 
  [Move piece (Bishop,team,move) | move <- moves,canMake game piece move]
    where moves = [ (x + i, y + i) | i <- [1..8], inBounds (x + i, y + i) ] ++ [ (x - i, y - i) | i <- [1..8], inBounds (x - i, y - i) ] ++ [ (x - i, y + i) | i <- [1..8], inBounds (x - i, y + i) ] ++ [ (x + i, y - i) | i <- [1..8], inBounds (x + i, y - i) ] --efficiency helped with chatgpt
possibleMoves game ((x,y), (Queen,team))        = 
  (possibleMoves game (((x,y), (Rook,team)))) ++ (possibleMoves game ((x,y), (Bishop,team)))
possibleMoves game piece@((x,y), (King,team))   = 
  [Move piece move | move <- moves, canMake game piece move]
  
safeMoves :: Game -> Piece -> [Move]
safeMoves game piece = [move | move@(Move old new) <- (possibleMoves game piece), not $ danger game new (getPieceTeam piece)]

possibleGameMoves :: Game -> [Move]
possibleGameMoves game@(team, whites, blacks) = 
  let pieces = if team == White then whites else blacks
  in concat [possibleMoves game p | p <- pieces]

check :: Game -> Piece -> Bool
check game piece@(pos, (King, team)) = danger game pos team
check game _ = False

checkmate :: Game -> Piece -> Bool
checkmate game piece = (check game piece) && (null $ possibleMoves game piece)

winner :: Game -> Winner
winner game@(_, whites, blacks)--should somehow account for stalemates - king&knight vs king, king&bishop vs king,king&bishop vs king&bishop where bishops are on the same color,king&knight&knight vs king can checkmate but cannot force checkmate - accept user input for forfeit to solve this?
  | checkmate game (head [piece | piece <- whites,(getPieceType piece)==King]) = Victor Black
  | checkmate game (head [piece | piece <- blacks,(getPieceType piece)==King]) = Victor White
  | null (safeMoves game (head [piece | piece <- whites,(getPieceType piece)==King]))||
    null (safeMoves game (head [piece | piece <- whites,(getPieceType piece)==King])) = Stalemate
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
toString game = unlines $ boardRows ++ [footer]
    where rowString y game = show y ++ " " ++ unwords [cellString (x, y) game | x <- [1..8]]
          cellString pos game = maybe "." pieceToString (getPiece game pos)
          footer = "  a b c d e f g h" 
          boardRows = [rowString y game | y <- [8,7..1]] 

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
