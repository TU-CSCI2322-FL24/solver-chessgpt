module Main where
import Chess
import ChessGPT
import Testing
import System.IO.Unsafe

writeGame :: Game -> FilePath -> IO ()
writeGame game path = undefined

loadGame :: FilePath -> IO Game 
loadGame path = putStrLn $ readGame str
  where str = unsafePerformIO $ readFile path

putBestMove :: Game -> IO ()
putBestMove game = putStrLn $ show $ bestMove game

--run with runhaskell Main.hs in command line
main :: IO()
main = do
  runTests 1 True --runs tests, do not touch