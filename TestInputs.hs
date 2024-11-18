module TestInputs where
import Chess
import ChessGPT
--we can use this file to make inputs
initialGame :: Game-- Written with the help of ChatGPT
initialGame = (White, whitePieces, blackPieces,100)--may need to change turn count
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

timeOut :: Game
timeOut = (Black,[((1,1),(King,White))],[((8,8),(King,Black))],0)

checkmate1 :: Game
checkmate1 = (Black,[((1,1),(King,White))],[],50)