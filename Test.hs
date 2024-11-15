module Test where
import Chess
import ChessGPT
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do 
    describe "move" $ do
        it "can move a pawn" $ do
            getPiece (fromJust $ (move initialGame (Move ((1,2),(Pawn,White)) (1,3)))) (1,3) `shouldBe` (Just ((1,3),(Pawn,White)))