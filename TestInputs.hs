module TestInputs where
import Chess
import ChessGPT
--we can use this file to make inputs
--should ONLY be used when we intend for a game to be generated
getGame :: Maybe Game -> Game
getGame (Just game) = game
getGame Nothing = error "no game no life"

initialGame :: Game-- Written with the help of ChatGPT
initialGame = (White, whitePieces++blackPieces,100)--may need to change turn count
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

initialGameStr :: String
initialGameStr = 
    "w\n100\nwra1 wnb1 wbc1 wqd1 wke1 wbf1 wng1 wrh1 wpa2 wpb2 wpc2 wpd2 wpe2 wpf2 wpg2 wph2 bra8 bnb8 bbc8 bqd8 bke8 bbf8 bng8 brh8 bpa7 bpb7 bpc7 bpd7 bpe7 bpf7 bpg7 bph7"

game1 :: Game
game1 = getGame $ move initialGame (Move ((1, 2), (Pawn, White)) (1,3))

game2 :: Game
game2 = getGame $ move game1 (Move ((1, 7), (Pawn, Black)) (1,6))

bishopGame :: Game
bishopGame = (White,[((1,1),(King,White)),((1,8),(Bishop,White)),((8,8),(King,Black))],50)

timeOut :: Game
timeOut = (Black,[((1,1),(King,White)),((8,8),(King,Black))],0)

win1 :: Game
win1 = (Black,[((1,1),(King,White))],50)