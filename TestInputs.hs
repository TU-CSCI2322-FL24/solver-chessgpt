module TestInputs where
import Chess
import ChessGPT
import Data.Maybe

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

blackMateTwo :: String
blackMateTwo = "b\n10\nbkb7 brb3 bqf4 wkg2"

whiteMateTwo :: String
whiteMateTwo = "w\n20\nwka1 wqh5 wrb8 bkh7 bph6 bng6 bpg7"

pawnGame1 :: Game
pawnGame1 = fromJust $ move initialGame (Move ((1, 2), (Pawn, White)) (1,3))

pawnGame2 :: Game
pawnGame2 = fromJust $ move pawnGame1 (Move ((1, 7), (Pawn, Black)) (1,6))

pawnGame3 :: Game
pawnGame3 = (White,[((1,1),(King,White)),((2,7),(Pawn,White)),((8,8),(King,Black)),((1,8),(Pawn,Black))],50)

pawnGame4 :: Game
pawnGame4 = (Black,[((1,1),(King,White)),((2,7),(Pawn,White)),((8,8),(King,Black)),((1,8),(Pawn,Black))],50)

rookGame1 :: Game
rookGame1 = (White,[((1,1),(King,White)),((2,7),(Rook,White)),((8,8),(King,Black)),((1,7),(Rook,Black))],50)

knightGame1 :: Game
knightGame1 = (White,[((1,1),(King,White)),((3,7),(Knight,White)),((8,8),(King,Black)),((1,8),(Knight,Black))],50)

bishopGame1 :: Game
bishopGame1 = (White,[((1,1),(King,White)),((1,8),(Bishop,White)),((8,8),(King,Black))],50)

bishopGame2 :: Game
bishopGame2 = (White,[((1,1),(King,White)),((1,8),(Bishop,White)),((2,7),(Bishop,White)),((8,8),(King,Black))],50)

bishopGame3 :: Game
bishopGame3 = (White,[((1,1),(King,White)),((1,8),(Bishop,White)),((8,8),(King,Black)),((2,7),(Bishop,Black))],50)

queenGame1 :: Game
queenGame1 = (White,[((1,1),(King,White)),((2,7),(Queen,White)),((8,8),(King,Black)),((1,7),(Rook,Black))],50)

kingGame1 :: Game
kingGame1 = (White,[((2,1),(King,White)),((2,7),(Rook,White)),((8,8),(King,Black)),((1,2),(Rook,Black))],50)

timeOut :: Game
timeOut = (Black,[((1,1),(King,White)),((8,8),(King,Black))],0)

win1 :: Game
win1 = (Black,[((1,1),(King,White))],50)