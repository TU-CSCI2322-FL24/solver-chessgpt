module Main where
import Chess
import ChessGPT
import Testing
import System.Console.GetOpt

--run with runhaskell Main.hs in command line
main :: IO()
main = do
  runTests 1 True --runs tests, do not touch