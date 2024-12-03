module Main where
import Chess
import ChessGPT
import Testing
import System.IO
import System.Environment
import System.Console.GetOpt

data Flag = Winner | Depth String | MoveFl String | Verbose | Interactive | Help | Test  deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg Winner) "Print best move and exit."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Specifies a cutoff depth."
          , Option ['m'] ["move"] (ReqArg MoveFl "<move>") "Print the result of the move and exit."
          , Option ['v'] ["verbose"] (NoArg Verbose) "Pretty-print the result of the move and exit."
          , Option ['i'] ["interactive"] (NoArg Interactive) "Play a game against the computer."
          , Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['t'] ["test"] (NoArg Test) "Run tests and exit."
          ]

--run with ./chessGPT in command line
main :: IO()
main = do
  args <- getArgs
  let (flags, inputs, errors) = getOpt Permute options args
  --putStrLn $ show (flags, inputs, errors)
  if (Help `elem` flags) || (not $ null errors)
  then putStrLn $ usageInfo "ChessGPT [options] [filename] chess game." options
  else 
    do let fName = if (null args)||(null inputs) then "initial.txt" else head inputs
       contents <- readFile fName
       case readGame contents of
        Nothing -> error "invalid game"
        Just game -> 
         if Winner `elem` flags then putBestMove game
         --if (Depth x) `elem` flags then 
         --if (MoveFl newMove) `elem` flags then printGame $ move game newMove
         else if Verbose `elem` flags then printGame game
         --if Interactive `elem` flags then helphelphelpAAAAAA
         else if Test `elem` flags then runTests 1 True
         else putStrLn contents