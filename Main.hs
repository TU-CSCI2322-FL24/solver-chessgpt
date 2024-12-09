module Main where
import Chess
import ChessGPT
import Testing
import Data.Maybe
import Text.Read
import System.IO
import System.Environment
import System.Console.GetOpt

data Flag = Winner | Depth String | MoveFl String | Verbose | Interactive | Interactive2p | Hard | Kaizo | Cheats | Help | Test  deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg Winner) "Print best move and exit."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Specifies a cutoff depth."
          , Option ['m'] ["move"] (ReqArg MoveFl "<move>") "Print the result of the move and exit."
          , Option ['v'] ["verbose"] (NoArg Verbose) "Pretty-print the result of the move and exit."
          , Option ['i'] ["interactive"] (NoArg Interactive) "Play a game against the computer."
          , Option ['2'] ["interactive2p"] (NoArg Interactive2p) "Play a two-player game of chess."
          , Option ['a'] ["hard"] (NoArg Hard) "Increase the skill of the computer."
          , Option ['k'] ["kaizo"] (NoArg Kaizo) "Do you fear death?"
          , Option ['c'] ["cheats"] (NoArg Cheats) "enable illegal moves."
          , Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['t'] ["test"] (NoArg Test) "Run tests and exit."
          ]
{-
Default: output good move, verbose gives details, depth gives depth. Should be done
Should help and test combine with others?
Move - done - should any additional info be provided?
is verbose adequate?
complete interactive
check errors
-}
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
dispatch fs game depth 
  | any isMove fs = moveIO fs game
  | Winner `elem` fs = if Verbose `elem` fs then putStrLn ("The best move is: " ++ show wM ++ "; the outcome will be: " ++ (winEval (whoWillWin game))) 
  else putStrLn ("The best move is: " ++ show wM)
  | Interactive `elem` fs = do
    t <- teamSelect
    interactive (Verbose `elem` fs) (Cheats `elem` fs) (Hard `elem` fs) (Kaizo `elem` fs) game t depth
  | Interactive2p `elem` fs = interactive2p (Verbose `elem` fs) (Cheats `elem` fs) game
  | otherwise = if Verbose `elem` fs then putStrLn ("Try: " ++ show dM ++ "; its rating is " ++ show dR)
  else putStrLn ("Try: " ++ show dM)
    where wM = bestMove game
          dM = goodMove game depth
          dR = case move game dM of
                Just g -> rateGame game
                Nothing -> -1000000

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

winEval :: (Maybe Winner) -> String
winEval w = case w of
  Just Stalemate -> "a stalemate"
  Just (Victor team) -> "a victory for " ++ show team
  _ -> "indeterminate"

interactive :: Bool -> Bool -> Bool -> Bool -> Game -> Team -> Int -> IO()
interactive isVerbose isCheat isHard isKaizo game@(turn,pieces,turns) team depth = do
  if isVerbose then printGame game else putStrLn $ showGame game
  putStrLn (show turn ++ "'s turn; " ++ show turns ++ " turns remaining")
  if turn == team then do
    m0 <- prompt "enter move"
    if m0 == "forfeit" then do putStrLn (show (oppositeTeam team) ++ " wins")
                               return ()
    else if m0 == "save" then do writeGame game "saved.txt"
                                 return ()
    else if getAddPiece m0 /= Nothing then case getAddPiece m0 of 
                                            Just p -> do interactive isVerbose isCheat isHard isKaizo (turn,(p:pieces),turns) team depth
                                            Nothing -> return ()
    else
      let m = readMove game m0
      in case m of 
        Just movefl -> if isCheat then 
                         case cMove game movefl of
                           Just g -> case winner g of
                                       Nothing -> interactive isVerbose isCheat isHard isKaizo g team depth
                                       Just Stalemate -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                            putStrLn "It's a draw!"
                                                            return ()
                                       Just (Victor w) -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                             putStrLn (show w ++ " wins!")
                                                             return () 
                           Nothing -> do putStrLn "Error: illegal move"
                                         interactive isVerbose isCheat isHard isKaizo game team depth
                         else case move game movefl of
                           Just g -> case winner g of
                                       Nothing -> interactive isVerbose isCheat isHard isKaizo g team depth
                                       Just Stalemate -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                            putStrLn "It's a draw!"
                                                            return ()
                                       Just (Victor w) -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                             putStrLn (show w ++ " wins!")
                                                             return () 
                           Nothing -> do putStrLn "Error: illegal move"
                                         interactive isVerbose isCheat isHard isKaizo game team depth
        Nothing -> do putStrLn "Error: invalid move"
                      interactive isVerbose isCheat isHard isKaizo game team depth
  else if isHard then let m = bestMove game
                      in case move game m of
                        Just g -> case winner g of
                                       Nothing -> interactive isVerbose isCheat isHard isKaizo g team depth
                                       Just Stalemate -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                            putStrLn "It's a draw!"
                                                            return ()
                                       Just (Victor w) -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                             putStrLn (show w ++ " wins!")
                                                             return () 
                        Nothing -> putStrLn "guys I think the AI is borked"
  else if isKaizo then let m = bestMove game
                       in case move game m of
                        Just g -> case winner g of
                                       Nothing -> interactive isVerbose isCheat isHard isKaizo g team depth
                                       Just Stalemate -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                            putStrLn "It's a draw!"
                                                            return ()
                                       Just (Victor w) -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                             putStrLn (show w ++ " wins!")
                                                             return () 
                        Nothing -> putStrLn "guys I think the AI is borked"
  else let m = goodMove game depth
       in case move game m of
        Just g -> case winner g of
                                       Nothing -> interactive isVerbose isCheat isHard isKaizo g team depth
                                       Just Stalemate -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                            putStrLn "It's a draw!"
                                                            return ()
                                       Just (Victor w) -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                             putStrLn (show w ++ " wins!")
                                                             return () 
        Nothing -> putStrLn "guys I think the AI is borked"
     
interactive2p :: Bool -> Bool -> Game -> IO()
interactive2p isVerbose isCheat game@(turn,pieces,turns) = do
    if isVerbose then printGame game else putStrLn $ showGame game
    putStrLn (show turn ++ "'s turn; " ++ show turns ++ " turns remaining")
    m0 <- prompt "enter move"
    if m0 == "forfeit" then do putStrLn (show (oppositeTeam turn) ++ " wins")
                               return ()
    else if m0 == "save" then do filename <- prompt "enter desired file name"
                                 writeGame game (filename++".txt")
                                 return ()
    else if getAddPiece m0 /= Nothing then case getAddPiece m0 of 
                                            Just p -> do interactive2p isVerbose isCheat (turn,(p:pieces),turns)
                                            Nothing -> return ()
    else
      let m = readMove game m0
      in case m of
          Just movefl -> if isCheat then 
                         case cMove game movefl of
                           Just g -> case winner g of
                                       Nothing -> interactive2p isVerbose isCheat g
                                       Just Stalemate -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                            putStrLn "It's a draw!"
                                                            return ()
                                       Just (Victor w) -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                             putStrLn (show w ++ " wins!")
                                                             return ()  
                           Nothing -> do putStrLn "Error: illegal move"
                                         interactive2p isVerbose isCheat game
                         else case move game movefl of
                           Just g -> case winner g of
                                       Nothing -> interactive2p isVerbose isCheat g
                                       Just Stalemate -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                            putStrLn "It's a draw!"
                                                            return ()
                                       Just (Victor w) -> do if isVerbose then printGame g else putStrLn $ showGame g
                                                             putStrLn (show w ++ " wins!")
                                                             return ()  
                           Nothing -> do putStrLn "Error: illegal move"
                                         interactive2p isVerbose isCheat game
          Nothing -> do putStrLn "Error: invalid move"
                        interactive2p isVerbose isCheat game

teamSelect :: IO Team
teamSelect = do
  team <- prompt "Select your team"
  if team `elem` ["White","white","W","w"] then return White else return Black

getAddPiece :: String -> Maybe Piece
getAddPiece ('a':'d':'d':x) = parsePiece x
getAddPiece _ = Nothing

prompt :: String -> IO String
prompt str = 
  do putStr $ str ++ ": "
     hFlush stdout
     answer <- getLine
     return answer