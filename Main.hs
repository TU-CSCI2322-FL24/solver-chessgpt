module Main where
import Chess
import ChessGPT
import Testing

writeGame :: Game -> FilePath -> IO ()
writeGame game path = undefined

loadGame :: FilePath -> IO Game 
loadGame path = do
  str <- readFile path
  return $ readGame str

putBestMove :: Game -> IO ()
putBestMove game = putStrLn $ show $ bestMove game

--run with runhaskell Main.hs in command line
main :: IO()
main = do
  runTests 1 True --runs tests, do not touch