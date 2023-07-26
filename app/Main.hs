module Main where

import Util
import Board
import Render
import Data.Foldable
import Data.Functor
import Data.Bifunctor
import Control.Monad
import Text.Read (readMaybe)
import System.IO
import Data.Char

execute :: String -> CoreState -> Either String CoreState
execute posStr = go (readMaybe posStr)
    where
        go :: Maybe (Int, Int) -> CoreState -> Either String CoreState
        go (Just t) (CoreState board turn Nothing)  =
            if t `elem` selectables turn board
                then Right (CoreState board turn (Just t))
                else Left "Error: Position is not selectable.\n"
        go (Just t) (CoreState board turn (Just s))
            | t `elem` selectables turn board = Right (CoreState board turn (Just t))
            | t `elem` movables s board       = Right (CoreState (makeMove s t board) (negPiece turn) Nothing)
            | otherwise = Left "Error: Position is not selectable nor movable.\n"
        go Nothing _ = Left "Error: Invaild Position.\n"

play :: CoreState -> IO ()
play cs = do
    render cs
    cs <- doUntilM $ do
        posStr <- getLine
        case execute posStr cs of
            Left e -> do
                putStrClearLine e
                return Nothing
            Right cs -> return $ Just cs
    unless (win cs) . play $ cs

main :: IO ()
main = play initCoreState