module Testing where
import Chess
import ChessGPT
import TestInputs
import Data.List
import Data.Semigroup
import Test.Grader.Tests
import Test.Grader.Core
import Test.Grader.Eval
import Test.Grader.Rubric
import Control.Monad.Extra
import Control.Monad.Trans.RWS
import System.IO.Unsafe
import Data.Maybe

--add new Grader String functions for each assess statement
testGetPiece :: Grader String
testGetPiece = assess "getPiece" 0 $ do--The number denotes the point value, which isn't particularly meaningful for our purposes
--use checks for actual tests; `shouldBe` is an equality assertion. Other assertions can be found in past Testing files if you want them.
        check "that getPiece returns a piece where one exists" $ getPiece win1 (1,1) `shouldBe` Just (King,White)
        check "that getPiece does not return a piece where one does not exist" $ getPiece win1 (8,8) `shouldBe` Nothing

testReplacePiece :: Grader String
testReplacePiece = assess "replacePiece" 0 $ do 
        check "that replacePiece changes position" $ ((5, 2), (King, White)) `shouldBeIn` newW
        check "that replacePiece changes type" $ replacePiece [((5, 1), (King, White))] ((5, 1), (King, White)) ((5, 1), (Pawn, White)) `shouldBe` [((5, 1), (Pawn, White))]
        check "that replacePiece replaces a piece" $ newW `shouldNotContain` ((5, 1), (King, White)) 
          where (team,pieces,count) = initialGame
                whites = getTeamPieces initialGame White
                newW = replacePiece whites ((5, 1), (King, White)) ((5, 2), (King, White))

testMove :: Grader String
testMove = assess "move" 0 $ do
        check "that move can move a pawn one space" $ getPiece (fromJust $ move initialGame (Move ((1, 2), (Pawn, White)) (1,3))) (1,3) `shouldBe` Just (Pawn,White)
        check "that move can move a pawn two spaces initially" $ getPiece (fromJust $ move initialGame (Move ((1, 2), (Pawn, White)) (1,4))) (1,4) `shouldBe` Just (Pawn,White)
        check "that move can move a pawn one space again" $ getPiece (fromJust $ move pawnGame2 (Move ((1, 3), (Pawn, White)) (1,4))) (1,4) `shouldBe` Just (Pawn,White)
        check "that move can't move a pawn two spaces again" $ move pawnGame2 (Move ((1, 3), (Pawn, White)) (1,5)) `shouldBe` Nothing
        check "that a pawn can't move through another piece" $ move (White,[((1, 3), (Pawn, White)),((1, 4), (Pawn, Black))],50) (Move ((1, 3), (Pawn, White)) (1,4)) `shouldBe` Nothing
        check "that a pawn can take a piece" $ pawnPieces `shouldNotContain` ((1,8),(Pawn,Black))
        check "that a black pawn can take a piece" $ bpawnPieces `shouldNotContain` ((2,7),(Pawn,White))
        check "that a rook can move vertically" $ getPiece (fromJust $ move rookGame1 (Move ((2,7),(Rook,White)) (2,1))) (2,1) `shouldBe` Just (Rook,White)
        check "that a rook can move horizontally" $ getPiece (fromJust $ move rookGame1 (Move ((2,7),(Rook,White)) (8,7))) (8,7) `shouldBe` Just (Rook,White)
        check "that a rook can't move diagonally" $ move rookGame1 (Move ((2,7),(Rook,White)) (1,8)) `shouldBe` Nothing
        check "that a rook can take a piece" $ rookPieces `shouldNotContain` ((1,7),(Rook,Black))
        check "that a knight can move over pieces" $ getPiece (fromJust $ move initialGame (Move ((2,1),(Knight,White)) (3,3))) (3,3) `shouldBe` Just (Knight,White)
        check "that the knight's bloody stupid moveset doesn't let it do nonsense" $ move initialGame (Move ((2,1),(Knight,White)) (8,3)) `shouldBe` Nothing
        check "that a knight can take a piece" $ knightPieces `shouldNotContain` ((1,8),(Knight,Black))
        check "that a bishop can move diagonally" $ getPiece (fromJust $ move bishopGame1 (Move ((1,8),(Bishop,White)) (7,2))) (7,2) `shouldBe` Just (Bishop,White)
        check "that a bishop can't move horizontally" $ move bishopGame1 (Move ((1,8),(Bishop,White)) (7,8)) `shouldBe` Nothing
        check "that a bishop can't move vertically" $ move bishopGame1 (Move ((1,8),(Bishop,White)) (1,2)) `shouldBe` Nothing
        check "that a bishop can't move through a piece" $ move bishopGame2 (Move ((1,8),(Bishop,White)) (7,2)) `shouldBe` Nothing
        check "that a bishop can take a piece" $ bishopPieces `shouldNotContain` ((2,7),(Bishop,Black))
        check "that a queen can move vertically" $ getPiece (fromJust $ move queenGame1 (Move ((2,7),(Queen,White)) (2,1))) (2,1) `shouldBe` Just (Queen,White)
        check "that a queen can move horizontally" $ getPiece (fromJust $ move queenGame1 (Move ((2,7),(Queen,White)) (8,7))) (8,7) `shouldBe` Just (Queen,White)
        check "that a queen can move diagonally" $ getPiece (fromJust $ move queenGame1 (Move ((2,7),(Queen,White)) (7,2))) (7,2) `shouldBe` Just (Queen,White)
        check "that a queen can't move elsewhere" $ move queenGame1 (Move ((2,7),(Queen,White)) (7,5)) `shouldBe` Nothing
        check "that a queen can't move through a piece" $ move initialGame (Move ((4,1),(Queen,White)) (4,2)) `shouldBe` Nothing
        check "that a queen can take a piece" $ queenPieces `shouldNotContain` ((1,7),(Rook,Black))
        check "that a king can move" $ getPiece (fromJust $ move rookGame1 (Move ((1,1),(King,White)) (2,2))) (2,2) `shouldBe` Just (King,White)
        check "that a king can't move two spaces" $ move rookGame1 (Move ((1,1),(King,White)) (3,2)) `shouldBe` Nothing
        check "that a king can't move into check" $ move kingGame1 (Move ((2,1),(King,White)) (1,1)) `shouldBe` Nothing--do we want this?
        check "that a king can't move through a piece" $ move initialGame (Move ((5, 1), (King, White)) (5,2)) `shouldBe` Nothing
        check "that a king can take a piece" $ kingPieces `shouldNotContain` ((1,2),(Rook,Black))
        check "that a king can't move out of bounds" $ move initialGame (Move ((5, 1), (King, White)) (5,0)) `shouldBe` Nothing
        check "that Black can't move on the first turn" $ move initialGame (Move ((1, 7), (Pawn, Black)) (1,6)) `shouldBe` Nothing
        check "that White can't move on the second turn" $ move pawnGame1 (Move ((1,3),(Pawn,White)) (1,4)) `shouldBe` Nothing
    where (_,pawnPieces,_)   = fromJust $ move pawnGame3 (Move ((2,7),(Pawn,White)) (1,8))
          (_,bpawnPieces,_)  = fromJust $ move pawnGame4 (Move ((1,8),(Pawn,Black)) (2,7))
          (_,bishopPieces,_) = fromJust $ move bishopGame3 (Move ((1,8),(Bishop,White)) (2,7))
          (_,knightPieces,_) = fromJust $ move knightGame1 (Move ((3,7),(Knight,White)) (1,8))
          (_,rookPieces,_)   = fromJust $ move rookGame1 (Move ((2,7),(Rook,White)) (1,7))
          (_,queenPieces,_)  = fromJust $ move queenGame1 (Move ((2,7),(Queen,White)) (1,7))
          (_,kingPieces,_)   = fromJust $ move kingGame1 (Move ((2,1),(King,White)) (1,2))

testCanMake :: Grader String
testCanMake = assess "canMake" 0 $ do
        check "that a pawn can move one space" $ canMake initialGame ((1, 2), (Pawn, White)) (1,3) `shouldBe` True
        check "that a pawn can move two spaces initially" $ canMake initialGame ((1, 2), (Pawn, White)) (1,4) `shouldBe` True
        check "that a pawn can move one space again" $ canMake pawnGame2 ((1, 3), (Pawn, White)) (1,4) `shouldBe` True
        check "that a pawn can't move two spaces normally" $ canMake pawnGame1 ((1, 3), (Pawn, White)) (1,5) `shouldBe` False
        check "that a pawn can't move through another piece" $ canMake (White,[((1, 3), (Pawn, White)),((1, 4), (Pawn, Black))],50) ((1, 3), (Pawn, White)) (1,4) `shouldBe` False
        check "that a king can't move initially" $ canMake initialGame ((5, 1), (King, White)) (5,2) `shouldBe` False
        check "that a king can't move out of bounds" $ canMake initialGame ((5, 1), (King, White)) (5,2) `shouldBe` False
        check "that Black can't move on the first turn" $ canMake initialGame ((1, 7), (Pawn, Black)) (1,6) `shouldBe` False
        check "that White can't move on the second turn" $ canMake pawnGame1 ((1,3),(Pawn,White)) (1,4) `shouldBe` False 

testCanCapture :: Grader String
testCanCapture = assess "canCapture" 0 $ do
        check "that you can move to an unoccupied position" $ canCapture initialGame (3,3) ((1, 2), (Pawn, White)) `shouldBe` True
        check "that you can move to a position occupied by an enemy piece" $ canCapture initialGame (1,7) ((1, 2), (Pawn, White)) `shouldBe` True
        check "that you can't move to a position occupied by a friendly piece" $ canCapture initialGame (2,2) ((1, 2), (Pawn, White)) `shouldBe` False

testPromote :: Grader String
testPromote = assess "promote" 0 $ do
        check "that a pawn promotes to a queen" $ promote ((1, 8), (Pawn, White)) `shouldBe` ((1,8),(Queen, White))
        check "that a different piece doesn't promote" $ promote ((1, 8), (King, White)) `shouldBe` ((1,8),(King, White))

testWinner :: Grader String
testWinner = assess "winner" 0 $ do
        check "that then winner wins" $ winner win1 `shouldBe` Just (Victor White)
        check "that there is no winner when time runs out" $ winner timeOut `shouldBe` Just Stalemate

testWhoWillWin :: Grader String
testWhoWillWin = assess "whoWillWin" 0 $ do
        check "that the winner will win" $ whoWillWin win1 `shouldBe` Victor White
        check "that there will be no winner when time runs out" $ whoWillWin timeOut `shouldBe` Stalemate

testReadShowGame :: Grader String
testReadShowGame = assess "readGame & showGame" 0 $ do
        check "that readGame reads a game" $ readGame initialGameStr `shouldBe` Just initialGame
        check "that showGame shows a game" $ showGame initialGame `shouldBe` initialGameStr
        check "that readGame(showGame(game))==game" $ readGame (showGame initialGame) `shouldBe` Just initialGame

--testLoadGame :: Grader String
--testLoadGame = assess "loadGame" 0 $ do
--        check "that loadGame loads a game" $ unsafePerformIO $ loadGame "input1" `shouldBe` initialGame

--Only checks chessGPT.hs       
testSyntax :: Grader String
testSyntax = assess "syntax" 0 $ do
        check "that you don't use (!!)" $ shouldNotBeCalled "!!"
        check "that you don't use head" $ shouldNotBeCalled "head"
        check "that you don't use tail" $ shouldNotBeCalled "tail"
        check "that you don't use last" $ shouldNotBeCalled "last"
        check "that you don't use fst" $ shouldNotBeCalled "fst"
        check "that you don't use snd" $ shouldNotBeCalled "snd"
        check "that you don't use fromJust" $ shouldNotBeCalled "fromJust"

tree :: Grader String
tree = describe "Project 5" $ 
    do describe "Chess" $ do
--you must add new Grader String types to these do blocks for the tests you have written to run; try to add them in order for readability
        testGetPiece
        testReplacePiece
        testMove
        testCanMake
        testCanCapture
        testPromote
        testWinner
       describe "ChessGPT" $ do
         testWhoWillWin
         testReadShowGame
       describe "Makin' it pretty" $ do
        testSyntax
     
--run with ./chessGPT --test in command line   
runTests :: Int -> Bool -> IO ()--how do test io
runTests verb force = do
        let a = runGrader tree
        format <- makeFormat verb force "projectDesc.yaml"
        runRWST a () format
        return ()