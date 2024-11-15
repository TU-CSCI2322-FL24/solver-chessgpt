module Test where
import Chess
import ChessGPT
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Control.Exception (evaluate)
--created using Hspec framework:https://hspec.github.io/
main :: IO()
main = hspec $ do 
    describe "move" $ do
        it "can move a pawn" $ do
            getPiece (fromJust $ (move initialGame (Move ((1,2),(Pawn,White)) (1,3)))) (1,3) `shouldBe` (Just ((1,3),(Pawn,White)))
        it "can't move a king at the start" $ do 
            (move initialGame (Move ((5,1),(King,White)) (5,2))) `shouldBe` Nothing