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
type Game = (Team,[Piece],Int)
data Winner   = Victor Team | Stalemate deriving (Show, Eq)
-- type Piece    = (PieceType,Team,Position)
type Piece = (Position, (PieceType, Team))

getPieceType :: Piece -> PieceType
getPieceType (_,(b,_)) = b

getPieceTeam :: Piece -> Team
getPieceTeam (_,(_,c)) = c

getPosition :: Piece -> Position
getPosition ((x,y), (_,_)) = (x,y)

getTeamPieces :: Game -> Team -> [Piece]
getTeamPieces game@(_,pieces,_) White = [piece | piece <- pieces, getPieceTeam piece == White]
getTeamPieces game@(_,pieces,_) Black = [piece | piece <- pieces, getPieceTeam piece == Black]

opposite :: Game -> Piece -> [Piece]
opposite game (_, (_, White)) = getTeamPieces game Black
opposite game (_, (_, Black)) = getTeamPieces game White

-- takes a position and checks whether there is a piece there, if yes then returns Just Piece, if no then returns Nothing
getPiece :: Game -> Position -> Maybe (PieceType, Team)
getPiece (_,pieces,_) pos = lookup pos pieces

oppositeTeam :: Team -> Team
oppositeTeam White = Black
oppositeTeam Black = White

replacePiece :: [Piece] -> Piece -> Piece -> [Piece]
replacePiece pieces old new = new:[piece | piece <- pieces, piece /= old]

move :: Game -> Move -> Maybe Game
move game@(team,pieces,count) (Move old newPos)
  | not $ canMake game old newPos = Nothing -- Can't make move
  | getPieceTeam old /= team      = Nothing -- Attempted move is by the wrong team
  | otherwise = 
    case (getPiece game newPos) of 
      Just target -> Just $ if team == White then (newTeam, pieces,count-1) else (newTeam, pieces,count-1)
      Nothing -> Just newGame -- Just a move, no pieces taken
    where whites = getTeamPieces game White
          blacks = getTeamPieces game Black
          newPiece = (newPos, (getPieceType old, getPieceTeam old))
          replacePiece pieces old new = new:(delete old pieces)
          newGame@(newTeam, pieces,newCount) = 
              if old `elem` whites 
                then (Black, replacePiece whites old newPiece++blacks,count-1) 
                else (White, whites++replacePiece blacks old newPiece,count-1)

inBounds :: Position -> Bool
inBounds (x,y) = x>0 && x<9 && y>0 && y<9 --this will need to change if we change board size or indexing, ie starting at 0

-- returns true if the diagonal is clear, false if it blocked
pathClear :: Game -> Position -> Position -> Bool
pathClear game (x1, y1) (x2, y2) = all (isNothing . getPiece game) positions
    where dx = signum (x2 - x1)
          dy = signum (y2 - y1)
          positions = takeWhile (/= (x2, y2)) $ tail [(x1 + i * dx, y1 + i * dy) | i <- [1..]]

isEmpty :: Game -> Position -> Bool
isEmpty game loc = isNothing $ getPiece game loc

-- returns true if the move can be made, and returns false if it cannot be made
-- checks whether a move is in the bounds of the board
-- checks that a move is valid for the given piece
-- checks that no piece of either color blocks the move
-- checks that the destination position is not currently occupied by a piece of the same color 
    -- if the destination is occupied by an opposite colored piece, returns true
canMake :: Game -> Piece -> Position -> Bool
canMake game@(turn,_,_) ((x1, y1), (Pawn, White)) (x2, y2) =
    turn==White && inBounds (x2, y2) &&
    (x2 == x1 && y2 == y1 + 1 && isNothing (getPiece game (x2, y2))) ||  -- regular one-step forward
    (x2 == x1 && y1 == 2 && y2 == 4 && isNothing (getPiece game (x2, 3)) && isNothing (getPiece game (x2, y2))) ||  -- initial two-step
    (abs (x2 - x1) == 1 && y2 == y1 + 1 &&  
    case getPiece game (x2, y2) of -- diagonal capture
        Just target -> snd target == Black
        Nothing     -> False)
canMake game@(turn,_,_) ((x1, y1), (Pawn, Black)) (x2, y2) =
    turn == Black && inBounds (x2, y2) &&
    (x2 == x1 && y2 == y1 - 1 && isNothing (getPiece game (x2, y2))) ||  -- regular one-step forward
    (x2 == x1 && y1 == 7 && y2 == 5 && isNothing (getPiece game (x2, 6)) && isNothing (getPiece game (x2, y2))) ||  -- initial two-step
    (abs (x2 - x1) == 1 && y2 == y1 - 1 &&
    case getPiece game (x2, y2) of -- diagonal capture
        Just target -> snd target == White
        Nothing     -> False)
canMake game@(turn,_,_) piece@((x1, y1), (Rook, team)) (x2, y2) =
    turn == team && inBounds (x2, y2) &&
    ((x2 == x1 || y2 == y1) && pathClear game (x1, y1) (x2, y2)) &&  -- clear row/column
    canCapture game (x2, y2) piece
canMake game@(turn,_,_) piece@((x1, y1), (Knight, team)) (x2, y2) =
    turn == team && inBounds (x2, y2) &&
    ((abs (x2 - x1), abs (y2 - y1)) `elem` [(2, 1), (1, 2)]) &&
    canCapture game (x2, y2) piece
canMake game@(turn,_,_) piece@((x1, y1), (Bishop, team)) (x2, y2) =
    turn == team && inBounds (x2, y2) &&
    abs (x2 - x1) == abs (y2 - y1) &&  -- diagonal check
    pathClear game (x1, y1) (x2, y2) &&
    canCapture game (x2, y2) piece
canMake game@(turn,_,_) piece@((x1, y1), (Queen, team)) (x2, y2) =
    turn == team && inBounds (x2, y2) &&
    ((x2 == x1 || y2 == y1) && pathClear game (x1, y1) (x2, y2) ||  -- rook-like move
     abs (x2 - x1) == abs (y2 - y1) && pathClear game (x1, y1) (x2, y2)) &&  -- bishop-like move
    canCapture game (x2, y2) piece
canMake game@(turn,_,_) piece@((x1, y1), (King, team)) (x2, y2) =
    turn == team && inBounds (x2, y2) &&
    abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1 &&  -- one-square in any direction
    canCapture game (x2, y2) piece &&
    null [op | op <- getTeamPieces game (oppositeTeam team), canMake game op (x2,y2)]--prevents king from moving into check

-- takes a game, position, and piece and checks whether the position is currently occupied 
    -- if the piece is the same color -> return false  
    -- if the piece is the opposite color -> return true
    -- if there is no piece -> return true
canCapture :: Game -> Position -> Piece -> Bool
canCapture game (x, y) og = 
    case getPiece game (x, y) of 
        Just target -> snd target /= getPieceTeam og
        Nothing     -> True

promote :: Piece -> Piece
promote ((x, 8), (Pawn, White)) = ((x, 8), (Queen, White))--we will need to change these cases for proper promotion eventually
promote ((x, 1), (Pawn, Black)) = ((x, 1), (Queen, Black))
promote piece = piece

possibleMoves :: Game -> Piece -> [Move]
possibleMoves game piece@((x, y), (Pawn, team))   = 
  [Move piece move | move <- moves, canMake game piece move]
    where moves = [(x,y+1),(x,y+2),(x,y-1),(x,y-2)]
possibleMoves game piece@((x, y), (Rook, team))   = 
  [Move ((x, y), (Rook,team) ) (x, ys) | ys <- [1..8], canMake game piece (x,ys)] ++
  [Move ((x, y), (Rook,team) ) (xs, y) | xs <- [1..8], canMake game piece (xs,y)]
possibleMoves game piece@((x,y), (Knight, team)) = 
  [Move piece move | move <- moves,canMake game piece move]
    where moves = [(x+3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y+3),(x-3,y+1),(x+1,y-3),(x-1,y-3),(x-3,y-1)]
possibleMoves game piece@((x, y), (Bishop, team))    = 
  [Move piece move | move <- moves,canMake game piece move]--ensures moves are in bounds
    where moves = [(x+1,y+1),(x+2,y+2),(x+3,y+3),(x+4,y+4),(x+5,y+5),(x+6,y+6),(x+7,y+7),(x+8,y+8),(x-1,y-1),(x-2,y-2),(x-3,y-3),(x-4,y-4),(x-5,y-5),(x-6,y-6),(x-7,y-7),(x-8,y-8)]
possibleMoves game ((x,y), (Queen, team))        = 
  (possibleMoves game (((x,y), (Rook, team)))) ++ (possibleMoves game ((x,y), (Bishop,team)))
possibleMoves game piece@((x,y), (King, team))   = 
  [Move piece move | move <- moves, canMake game piece move]
    where moves = [(x,y+1),(x+1,y),(x,y-1),(x-1,y),(x+1,y+1),(x+1,y-1),(x-1,y-1),(x-1,y+1)]

possibleGameMoves :: Game -> [Move]
possibleGameMoves game@(team,pieces,_) = 
  let teamPieces = if team == White then whites else blacks
  in concat [possibleMoves game p | p <- teamPieces]
    where whites = getTeamPieces game White
          blacks = getTeamPieces game Black
          

winner :: Game -> Maybe Winner
winner game@(turn,pieces,count) = --should somehow account for stalemates - king&knight vs king, king&bishop vs king,king&bishop vs king&bishop where bishops are on the same color,king&knight&knight vs king can checkmate but cannot force checkmate - accept user input for forfeit to solve this?
  if count<1 
    then Just Stalemate 
    else case teamKings of
      ([],_) -> Just (Victor Black)
      (_,[]) -> Just (Victor White)
      ((w:_),(b:_)) -> if (null (possibleMoves game w)&&(turn==White))||
                          (null (possibleMoves game b)&&(turn==Black)) then Just Stalemate else Nothing
   where kings = [piece | piece <- pieces,(getPieceType piece)==King]
         teamKings = ([king | king <- kings,getPieceTeam king == White],[king | king <- kings,getPieceTeam king == Black])
pieceToString :: (PieceType, Team) -> String
pieceToString (Pawn, Black)   = "♙"
pieceToString (Pawn, White)   = "♟"
pieceToString (Rook, Black)   = "♖"
pieceToString (Rook, White)   = "♜"
pieceToString (Knight, Black) = "♘"
pieceToString (Knight, White) = "♞"
pieceToString (Bishop, Black) = "♗"
pieceToString (Bishop, White) = "♝"
pieceToString (Queen, Black)  = "♕"
pieceToString (Queen, White)  = "♛"
pieceToString (King, Black)   = "♔"
pieceToString (King, White)   = "♚"

toString :: Game -> String
toString game = unlines $ boardRows ++ [footer]
    where rowString y game = show y ++ " " ++ unwords [cellString (x, y) game | x <- [1..8]]
          cellString pos game = maybe "." pieceToString (getPiece game pos)
          footer = "  a b c d e f g h" 
          boardRows = [rowString y game | y <- [8,7..1]] 

printGame :: Game -> IO ()--written with the help of ChatGPT
printGame game = putStrLn $ toString game
