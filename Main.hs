module Main where
import Chess
import ChessGPT
import Testing

writeGame :: Game -> FilePath -> IO ()
writeGame game path = undefined

loadGame :: FilePath -> IO Game 
loadGame path = undefined

putBestMove :: Game -> IO ()
putBestMove game = undefined

--run with runhaskell Main.hs in command line
main :: IO()
main = do
  runTests 1 True --runs tests, do not touch