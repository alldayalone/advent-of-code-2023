{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where
import Data.Matrix as Matrix (Matrix(..), (!), setElem, fromLists)
import Data.Char (digitToInt)
import Data.Maybe as Maybe (isJust, isNothing, fromJust)
import Data.List (filter)
import Data.HashMap.Strict as HashMap (HashMap, (!), insert, empty, lookup, member)
import Data.Hashable (Hashable, hash)
import GHC.Generics (Generic)
import Data.PSQueue as PSQ (PSQ(..), empty, lookup, insert, minView, null, adjust)
import Data.PSQueue.Internal (Binding(..))

main :: IO ()
main = do
  contents <- readFile "src/17_1/input.txt"
  print . solve' . parse $ contents

parse :: String -> Matrix Int
parse = fmap digitToInt . Matrix.fromLists . lines

initialCells :: [Cell]
initialCells = [Cell 1 1 dir | dir <- [South, East]]

solve' :: Matrix Int -> Int
solve' grid = solve q g grid
  where
    n = Matrix.nrows grid
    m = Matrix.ncols grid
    targetValue = grid Matrix.! (n, m)
    g = foldr (`HashMap.insert` 0) HashMap.empty initialCells
    q = foldr (\cell -> PSQ.insert cell (estimate n m targetValue cell)) PSQ.empty initialCells

data Cell = Cell {
  x :: Int,
  y :: Int,
  dir :: Direction
} deriving (Show, Ord, Eq, Generic, Hashable)

data Direction = North | East | South | West deriving (Eq, Show, Ord, Generic, Hashable)

solve :: PSQ Cell Int -> HashMap Cell Int -> Matrix Int -> Int
solve queue g grid
  | isJust result = fromJust result
  | otherwise = solve queue'' g' grid
  where
    result = minimum [HashMap.lookup (Cell n m dir) g | dir <- [East, South]]
    (Just (current :-> _, queue')) = PSQ.minView queue
    n = Matrix.nrows grid
    m = Matrix.ncols grid
    targetValue = grid Matrix.! (n, m)
    nbrs = map (\x -> (x, getCost grid current x)) . filter (validBounds n m) . getNeighbors $ current
    (queue'', g') = foldl (folder current (estimate n m targetValue)) (queue', g) nbrs

folder :: Cell -> (Cell -> Int) -> (PSQ Cell Int, HashMap Cell Int) -> (Cell, Int) -> (PSQ Cell Int, HashMap Cell Int)
folder current estimate (q, g) (nbr, value)
  | HashMap.member nbr g && tentativeScore >= g HashMap.! nbr = (q, g)
  | isNothing (PSQ.lookup nbr q) = (q', g')
  | tentativeScore < g HashMap.! nbr = (q, g')
  where
    tentativeScore = value + g HashMap.! current
    g' = HashMap.insert nbr tentativeScore g
    q' = PSQ.insert nbr ((g' HashMap.! nbr) + estimate nbr) q

getNeighbors :: Cell -> [Cell]
getNeighbors cell = [move cell step | cell <- map (\ dir -> cell {dir}) newDirs, step <- [1..3]]
  where
    newDirs = if dir cell `elem` [North, South] then [East, West] else [North, South]

getCost :: Matrix Int -> Cell -> Cell -> Int
getCost grid (Cell x1 y1 _) (Cell x2 y2 _) = sum . map (grid Matrix.!) $ [(x, y) | x <- range x1 x2, y <- range y1 y2]

range :: Int -> Int -> [Int]
range current nbr
  | current == nbr = [current]
  | abs (current - nbr) == 1 = [nbr]
  | current < nbr = nbr : range current (nbr - 1)
  | current > nbr = nbr : range current (nbr + 1)

move :: Cell -> Int -> Cell
move (Cell x y dir) step = case dir of
  North -> Cell (x - step) y dir
  South -> Cell (x + step) y dir
  East  -> Cell x (y + step) dir
  West  -> Cell x (y - step) dir


validBounds :: Int -> Int -> Cell -> Bool
validBounds n m (Cell x y _) = x <= n && y <= m && x > 0 && y > 0

estimate :: Int -> Int -> Int -> Cell -> Int
estimate n m targetValue (Cell x y _)
  | x == n && y == m = 0
  | otherwise = n + m + targetValue - x - y - 1