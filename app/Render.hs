module Render where

import Board
import Data.List
import System.IO
import qualified System.Console.ANSI as C

prettyStringMatrix :: Int -> Int -> [[String]] -> String
prettyStringMatrix ncols nrows strings = concat
        [ "┌ ", unwords (replicate ncols blank), " ┐\n"
        , "│ ", unwords (replicate ncols blank), " │\n"
        , unlines
        (concat [[ "│ " ++ unwords (fmap (\j -> fill (getAt strings (i,j))) [0 .. ncols - 1]) ++ " │",
                "│ " ++ unwords (replicate ncols blank) ++ " │"] | i <- [0 .. nrows - 1]])
        , "└ ", unwords (replicate ncols blank), " ┘"
        ]
    where
        widest = maximum (fmap length (concat strings))
        fill str = let n = (widest - length str) in replicate (n `div` 2) ' ' ++ str ++ replicate (n - (n `div` 2)) ' '
        blank = fill ""

buildStringMatrix :: Board -> [[String]]
buildStringMatrix board = transpose (("    " : fmap show [0..7]) : transpose (fmap show [0..7] : fmap (fmap showSingle) board))


prettyCoreState :: CoreState -> String
prettyCoreState (CoreState board turn (Just p)) =
    "You have selected " ++ show p
    ++ ", move it to one of:\n" ++ show (movables p board)
    ++ "\nor select another piece between:\n" ++ show (delete p (selectables turn board))
prettyCoreState (CoreState board turn Nothing)  =
    "Please select one piece:\n" ++ show (selectables turn board)

render :: CoreState -> IO()
render cs = do
    C.clearScreen
    C.setCursorPosition 0 18
    putStrLn "Line of Action"
    putStrLn (prettyStringMatrix 9 9 (buildStringMatrix (board cs)))
    putStrLn (replicate 20 ' ' ++ show (turn cs) ++ "'s turn.")
    putStrLn (prettyCoreState cs)


putStrClearLine :: String -> IO ()
putStrClearLine s = do
    putStr s
    C.clearFromCursorToLineEnd
    hFlush stdout



