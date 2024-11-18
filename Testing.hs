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

--add new Grader String functions for each assess statement
testGetPiece :: Grader String
testGetPiece = assess "getPiece" 0 $ do--The number denotes the point value, which isn't particularly meaningful for our purposes
--use checks for actual tests; `shouldBe` is an equality assertion. Other assertions can be found in past Testing files if you want them.
        check "that getPiece returns a piece where one exists" $ getPiece checkmate1 (1,1) `shouldBe` Just ((1,1),(King,White))
        check "that getPiece does not return a piece where one does not exist" $ getPiece checkmate1 (8,8) `shouldBe` Nothing

testReplacePiece :: Grader String
testReplacePiece = assess "replacePiece" 0 $ do 
        check "that replacePiece changes position" $ ((5, 2), (King, White)) `shouldBeIn` newW
        check "that replacePiece changes type" $ replacePiece [((5, 1), (King, White))] ((5, 1), (King, White)) ((5, 1), (Pawn, White)) `shouldBe` [((5, 1), (Pawn, White))]
        check "that replacePiece replaces a piece" $ newW `shouldNotContain` ((5, 1), (King, White)) 
          where (team,whites,blacks,count) = initialGame
                newW = replacePiece whites ((5, 1), (King, White)) ((5, 2), (King, White))

testMove :: Grader String
testMove = assess "move" 0 $ do
        check "that move can move a pawn one space" $ getPiece (getGame $ move initialGame (Move ((1, 2), (Pawn, White)) (1,3))) (1,3) `shouldBe` Just ((1,3),(Pawn,White))
        check "that move can move a pawn two spaces initially" $ getPiece (getGame $ move initialGame (Move ((1, 2), (Pawn, White)) (1,4))) (1,4) `shouldBe` Just ((1,4),(Pawn,White))
        check "that move can move a pawn one space again" $ getPiece (getGame $ move game2 (Move ((1, 3), (Pawn, White)) (1,4))) (1,4) `shouldBe` Just ((1,4),(Pawn,White))
        check "that move can't move a pawn two spaces again" $ move game1 (Move ((1, 3), (Pawn, White)) (1,5)) `shouldBe` Nothing
        check "that a pawn can't move through another piece" $ move (White,[((1, 3), (Pawn, White))],[((1, 4), (Pawn, Black))],50) (Move ((1, 3), (Pawn, White)) (1,4)) `shouldBe` Nothing
        check "that move can't move a king initially" $ move initialGame (Move ((5, 1), (King, White)) (5,2)) `shouldBe` Nothing
        check "that Black can't move on the first turn" $ move initialGame (Move ((1, 7), (Pawn, Black)) (1,6)) `shouldBe` Nothing
        check "that a piece can't move out of bounds" $ move initialGame (Move ((5, 1), (King, White)) (5,0)) `shouldBe` Nothing

testCanMake :: Grader String
testCanMake = assess "canMake" 0 $ do
        check "that a pawn can move one space" $ canMake initialGame ((1, 2), (Pawn, White)) (1,3) `shouldBe` True
        check "that a pawn can move two spaces initially" $ canMake initialGame ((1, 2), (Pawn, White)) (1,4) `shouldBe` True
        check "that a pawn can move one space again" $ canMake game1 ((1, 3), (Pawn, White)) (1,4) `shouldBe` True
        check "that a pawn can't move two spaces normally" $ canMake game1 ((1, 3), (Pawn, White)) (1,5) `shouldBe` False
        check "that a pawn can't move through another piece" $ canMake (White,[((1, 3), (Pawn, White))],[((1, 4), (Pawn, Black))],50) ((1, 3), (Pawn, White)) (1,4) `shouldBe` False
        check "that a king can't move initially" $ canMake initialGame ((5, 1), (King, White)) (5,2) `shouldBe` False
        check "that Black can't move on the first turn" $ canMake initialGame ((1, 7), (Pawn, Black)) (1,6) `shouldBe` False--do we even want to fix this?
        check "that a piece can't move out of bounds" $ canMake initialGame ((5, 1), (King, White)) (5,2) `shouldBe` False

testCanCapture :: Grader String
testCanCapture = assess "canCapture" 0 $ do
        check "that you can move to an unoccupied position" $ canCapture initialGame (3,3) ((1, 2), (Pawn, White)) `shouldBe` True
        check "that you can move to a position occupied by an enemy piece" $ canCapture initialGame (1,7) ((1, 2), (Pawn, White)) `shouldBe` True
        check "that you can't move to a position occupied by a friendly piece" $ canCapture initialGame (2,2) ((1, 2), (Pawn, White)) `shouldBe` False

testPromote :: Grader String
testPromote = assess "promote" 0 $ do
        check "that a pawn promotes to a queen" $ promote ((1, 8), (Pawn, White)) `shouldBe` ((1, 8), (Queen, White))
        check "that a different piece doesn't promote" $ promote ((1, 8), (King, White)) `shouldBe` ((1, 8), (King, White))

testWinner :: Grader String
testWinner = assess "winner" 0 $ do
        check "that then winner wins" $ winner checkmate1 `shouldBe` Just (Victor White)
        check "that there is no winner when time runs out" $ winner timeOut `shouldBe` Just Stalemate

testWhoWillWin :: Grader String
testWhoWillWin = assess "whoWillWin" 0 $ do
              check "that the winner will win" $ whoWillWin checkmate1 `shouldBe` Victor White
             
testSyntax :: Grader String
testSyntax = assess "syntax" 0 $ do
        check "that you don't use (!!)" $ shouldNotBeCalled "!!"
        check "that you don't use head" $ shouldNotBeCalled "head"
        check "that you don't use tail" $ shouldNotBeCalled "tail"
        check "that you don't use last" $ shouldNotBeCalled "last"
        check "that you don't use fst" $ shouldNotBeCalled "fst"
        check "that you don't use snd" $ shouldNotBeCalled "snd"

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
       describe "Makin' it pretty" $ do
        testSyntax

     
--run with runhaskell Main.hs in command line   
runTests :: Int -> Bool -> IO ()
runTests verb force = do
        let a = runGrader tree
        format <- makeFormat verb force "projectDesc.yaml"
        runRWST a () format
        return ()