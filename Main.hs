module Main where
import Chess
import ChessGPT
import Testing
import Data.Maybe
import Text.Read
import System.IO
import System.Environment
import System.Console.GetOpt

data Flag = Winner | Depth String | MoveFl String | Verbose | Interactive | Interactive2p | Hard | Cheats | Help | Test  deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg Winner) "Print best move and exit."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Specifies a cutoff depth."
          , Option ['m'] ["move"] (ReqArg MoveFl "<move>") "Print the result of the move and exit."
          , Option ['v'] ["verbose"] (NoArg Verbose) "Pretty-print the result of the move and exit."
          , Option ['i'] ["interactive"] (NoArg Interactive) "Play a game against the computer."
          , Option ['2'] ["interactive2p"] (NoArg Interactive2p) "Play a two-player game of chess."
          , Option ['a'] ["hard"] (NoArg Hard) "Play a game against a more skilled computer."
          , Option ['c'] ["cheats"] (NoArg Cheats) "enable illegal moves."
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
  else if Test `elem` flags then runTests 1 True
  else
    do let fName = if (null args)||(null inputs) then "initial.txt" else head inputs
       contents <- readFile fName
       case (readGame contents, getDepth flags) of
        (Nothing, _) -> putStrLn "Error 404: Game not found"
        (game, Nothing) -> putStrLn "Error 404: Depth not found"
        (Just game,Just depth) -> dispatch flags game depth

dispatch :: [Flag] -> Game -> Int -> IO()
dispatch fs game@(team,_,_) depth 
  | any isMove fs = moveIO fs game
  | Winner `elem` fs = if Verbose `elem` fs then putStrLn ("The best move is: " ++ show wM ++ "; the outcome will be: " ++ (winEval (whoWillWin game depth) team)) 
  else putStrLn ("The best move is: " ++ show wM)
  | Interactive `elem` fs = undefined
  | Interactive2p `elem` fs = interactive2p (Verbose `elem` fs) (Cheats `elem` fs) game depth
  | otherwise = if Verbose `elem` fs then putStrLn ("Try: " ++ show dM ++ "; its rating is " ++ show dR)
  else putStrLn ("Try: " ++ show dM)
    where wM = bestMove game
          (dR,dM) = whoMightWin game depth

getMoveFl :: [Flag] -> Game -> Maybe Move
getMoveFl [] _ = Nothing
getMoveFl (MoveFl m:_) game = readMove game m
getMoveFl (_:fs) game = getMoveFl fs game

isMove :: Flag -> Bool
isMove (MoveFl _) = True
isMove _ = False

getDepth :: [Flag] -> Maybe Int
getDepth [] = Just 3
getDepth (Depth d:_) = readMaybe d
getDepth (_:fs) = getDepth fs

moveIO :: [Flag] -> Game -> IO()
moveIO flags game = 
  case getMoveFl flags game of
    Just movefl -> 
        case move game movefl of
          Just g -> if Verbose `elem` flags then printGame g else putStrLn $ showGame g
          Nothing -> putStrLn "Error: illegal move"
    Nothing -> putStrLn "Error: invalid move"

winEval :: (Maybe Winner) -> Team -> String
winEval w t = case w of
  Just Stalemate -> "a stalemate"
  Just (Victor team) -> if team==t then "a victory for you" else "a loss for you"
  _ -> "indeterminate"

interactive2p :: Bool -> Bool -> Game -> Int -> IO()
interactive2p isVerbose isCheat game@(team,_,turns) depth = do
    if isVerbose then printGame game else putStrLn $ showGame game
    putStrLn (show team ++ "'s turn; " ++ show turns ++ " turns remaining")
    m0 <- prompt "enter move"
    if m0 == "forfeit" then do putStrLn (show (oppositeTeam team) ++ " wins")
                               return ()
    else
      let m = readMove game m0
      in case m of
          Just movefl -> if isCheat then 
                         case cMove game movefl of
                           Just g -> case winner g of
                                       Nothing -> interactive2p isVerbose isCheat g depth
                                       Just Stalemate -> putStrLn "It's a draw!"
                                       Just (Victor w) -> putStrLn (show w ++ " wins!") 
                           Nothing -> do putStrLn "Error: illegal move"
                                         interactive2p isVerbose isCheat game depth
                         else case move game movefl of
                           Just g -> case winner g of
                                       Nothing -> interactive2p isVerbose isCheat g depth
                                       Just Stalemate -> putStrLn "It's a draw!"
                                       Just w -> putStrLn (show w ++ " wins!") 
                           Nothing -> do putStrLn "Error: illegal move"
                                         interactive2p isVerbose isCheat game depth
          Nothing -> do putStrLn "Error: invalid move"
                        interactive2p isVerbose isCheat game depth

prompt :: String -> IO String
prompt str = 
  do putStr $ str ++ ": "
     hFlush stdout
     answer <- getLine
     return answer