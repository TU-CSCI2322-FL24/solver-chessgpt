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
--should ONLY be used when we intend for a game to be generated
getGame :: Maybe Game -> Game
getGame (Just game) = game
getGame Nothing = error "no game no life"
--add new Grader String functions for each assess statement
testGetPiece :: Grader String
testGetPiece = assess "getPiece" 1 $ do--The number denotes the point value, which isn't particularly meaningful for our purposes
--use checks for actual tests; `shouldBe` is an equality assertion. Other assertions can be found in past Testing files if you want them.
        check "that getPiece returns a piece where one exists" $ getPiece checkmate1 (1,1) `shouldBe` Just ((1,1),(King,White))
        check "that getPiece does not return a piece where one does not exist" $ getPiece checkmate1 (8,8) `shouldBe` Nothing

testReplacePiece :: Grader String
testReplacePiece = assess "replacePiece" 1 $ do 
        check "that replacePiece changes position" $ replacePiece [((5, 1), (King, White))] ((5, 1), (King, White)) ((5, 2), (King, White)) `shouldBe` [((5, 2), (King, White))]
        check "that replacePiece changes type" $ replacePiece [((5, 1), (King, White))] ((5, 1), (King, White)) ((5, 1), (Pawn, White)) `shouldBe` [((5, 1), (Pawn, White))]

testMove :: Grader String
testMove = assess "move" 1 $ do
        check "that move can move a pawn" $ getPiece (getGame $ move initialGame (Move ((1, 2), (Pawn, White)) (1,3))) (1,3) `shouldBe` Just ((1,3),(Pawn,White))
        check "that move can't move a king initially" $ move initialGame (Move ((5, 1), (King, White)) (5,2)) `shouldBe` Nothing
        check "that Black can't move on the first turn" $ move initialGame (Move ((1, 7), (Pawn, Black)) (1,6)) `shouldBe` Nothing

testWinner :: Grader String
testWinner = assess "winner" 1 $ do
        check "that then winner wins" $ winner checkmate1 `shouldBe` Just (Victor White)
        check "that there is no winner when time runs out" $ winner timeOut `shouldBe` Just Stalemate

testWhoWillWin :: Grader String
testWhoWillWin = assess "whoWillWin" 1 $ do
              check "that the winner will win" $ whoWillWin checkmate1 `shouldBe` Victor White

tree :: Grader String
tree = describe "Project 5" $ 
    do describe "Chess" $ do
--you must add new Grader String types to these do blocks for the tests you have written to run; try to add them in order for readability
        testGetPiece
        testReplacePiece
        testMove
        testWinner
       describe "ChessGPT" $ do
         testWhoWillWin
     
--run with runhaskell Main.hs in command line   
runTests :: Int -> Bool -> IO ()
runTests verb force = do
        let a = runGrader tree
        format <- makeFormat verb force "projectDesc.yaml"
        runRWST a () format
        return ()