{-# LANGUAGE FlexibleContexts #-}
module Board where

import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe
import Data.List
import Data.List.Lens
import Control.Lens.Operators
import Control.Lens.At

data Piece = Cross | Circle deriving (Enum, Eq, Bounded, Read)

instance Show  Piece where
    show x = ["X", "O"] !! fromEnum x

showSingle :: Maybe Piece -> String
showSingle (Just x) = show x
showSingle Nothing  = " "

type Board = [[Maybe Piece]]

data CoreState = CoreState 
    { board :: Board
    , turn ::  Piece
    , selected ::  Maybe (Int, Int)
    }


crossRow :: [Maybe Piece]
crossRow = [Just Cross] ++ replicate 6 Nothing ++ [Just Cross]

circleRow :: [Maybe Piece]
circleRow = [Nothing] ++ replicate 6 (Just Circle) ++ [Nothing]

initBoard :: Board
initBoard = [circleRow] ++ replicate 6 crossRow ++ [circleRow]

initCoreState :: CoreState
initCoreState = CoreState initBoard Cross Nothing

evalPiece :: Maybe Piece -> Int
evalPiece Nothing = 0
evalPiece _       = 1

negPiece :: Piece -> Piece
negPiece x = case x of
    Cross  -> Circle
    Circle -> Cross

data Path = Horizontal | Vertical | Diagonal | SubDiag deriving (Enum, Eq)

isBounded :: (Int, Int) -> Bool
isBounded (a, b) = (a <= 7) && (a >= 0) && (b <= 7) && (b >= 0)

getAt :: [[a]] -> (Int, Int) -> a
getAt m (x,y)  = (m !! x) !! y

moveAlongPath :: Path -> Int -> (Int, Int) -> (Int, Int)
moveAlongPath p = case p of
    Horizontal -> \i (a, b) -> (a, b + i)
    Vertical   -> \i (a, b) -> (a + i, b)
    Diagonal   -> \i (a, b) -> (a + i, b + i)
    SubDiag    -> \i (a, b) -> (a + i, b - i)

pathToPosList :: Path -> (Int, Int) -> [(Int, Int)]
pathToPosList p (a, b) = [pt | i <- [-7..7], let pt = moveAlongPath p i (a, b), isBounded pt]

stepSize :: Path -> (Int, Int) -> Board -> Int
stepSize p (a, b) board = sum (map (evalPiece . getAt board) (pathToPosList p (a, b)))

boundedMovesOnPath :: Path -> (Int, Int) -> Board -> [(Int, Int)]
boundedMovesOnPath p (a, b) board = [pt | i <- [-s,s], let pt = moveAlongPath p i (a, b), isBounded pt]
    where
        s = stepSize p (a, b) board

isUnobstructed :: Path -> (Int, Int) -> Board -> (Int, Int) -> Bool
isUnobstructed p (a, b) board (c, d) = neg `notElem` pieces && (getAt board (a,b) /= getAt board (c,d))
    where
        coorlist = pathToPosList p (a, b)
        i = fromJust (elemIndex (a, b) coorlist)
        j = fromJust (elemIndex (c, d) coorlist)
        neg = Just (negPiece $ fromJust $ getAt board (a,b))
        indices = if i > j
            then [j + 1 .. i] else [i .. j - 1]
        pieces = map (getAt board . (coorlist !!)) indices

validMovesOnPath :: Path -> (Int, Int) -> Board -> [(Int, Int)]
validMovesOnPath p (a, b) board = filter (isUnobstructed p (a, b) board) bm
    where
        bm = boundedMovesOnPath p (a, b) board

movables :: (Int, Int) -> Board -> [(Int, Int)]
movables (a, b) board = concatMap (\p -> validMovesOnPath p (a, b) board) [Horizontal ..]


selectables :: Piece -> Board -> [(Int, Int)]
selectables c board = [(i,j) | i <- [0..7], j <- [0..7], getAt board (i,j) == Just c]

makeMove :: (Int, Int) -> (Int, Int) -> Board -> Board
makeMove s t board = (board & ixx t .~ (board ^?! ixx s)) & ixx s .~ Nothing
    where
        ixx (i, j) = ix i . ix j

win :: CoreState -> Bool
win cs = False