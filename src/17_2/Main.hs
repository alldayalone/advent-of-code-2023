{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main (main) where
import Debug.Trace (trace)
import Data.Matrix as Matrix (Matrix(..), (!), setElem, fromLists)
import Data.Char (digitToInt)
import Data.Maybe as Maybe (isJust, isNothing, fromJust)

import Data.Ord (compare)
import Data.List (filter, sortBy)
import Data.List.Split (splitOn)
import Data.HashMap.Strict as HashMap (HashMap, (!), insert, empty, lookup, member)
import Data.Hashable (Hashable, hash)
import GHC.Generics (Generic)
import Data.PSQueue as PSQ (PSQ(..), empty, lookup, insert, minView, null, adjust)
import Data.PSQueue.Internal (Binding(..))

-- there has to be a bug cause it's giving 96 instead of 102, and 1024 instead of 1023
-- But I literally spend couple hours moving JS code into haskell. And it worked out first try. WTF??!?!?!!?!

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

-- median :: Matrix Cell -> Matrix Int -> [Seq (Direction, (Int, Int))] -> Int
-- median grid estimates paths = estimates Matrix.! pos
--   where
--     (_:|>(_, pos)) = head paths

-- fourInRow :: Seq (Direction, (Int, Int)) -> Bool
-- fourInRow Sequence.Empty = False
-- fourInRow ((North,_):<|(North,_):<|(North,_):<|(North,_):<|_) = True
-- fourInRow ((East,_):<|(East,_):<|(East,_):<|(East,_):<|_) = True
-- fourInRow ((South,_):<|(South,_):<|(South,_):<|(South,_):<|_) = True
-- fourInRow ((West,_):<|(West,_):<|(West,_):<|(West,_):<|_) = True
-- fourInRow (_:<|t) = fourInRow t

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
getNeighbors cell = [move cell step | cell <- map (\ dir -> cell {dir}) newDirs, step <- [4..10]]
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

  -- | HashMap.member greatPath hm = solve hm' paths best (State { grid, path })
  -- | fourInRow greatPath = solve hm' paths best (State { grid, path })
  -- | calc grid greatPath > best = solve hm' paths best (State { grid, path })
  -- | Maybe.isJust (Sequence.findIndexL (\(_, p) -> p == pos) initPath) = solve hm' paths best (State { grid, path })
  -- | (>1) . length . Sequence.filter (\(_, pos2) -> isNeighbour pos pos2) $ initPath = solve hm' paths best (State { grid, path })
  -- | pos == (n, n) = solve hm' paths (min best (calc grid greatPath)) (State { grid, path = greatPath })
  -- | otherwise = solve hm' newCandidatePaths best (State { grid, path })
  -- where
  --   (greatPath:paths) = candidatePaths
  --   (initPath:|>(dir, pos)) = greatPath
  --   (y,x) = pos
  --   cell = grid Matrix.! pos
  --   newCandidatePaths = (++) paths . Data.List.filter (\path -> eval grid path < best ) . map (greatPath :|>) . Data.List.filter validBounds . candidateDirections $ (dir, pos)
  --   hm' = HashMap.insert greatPath True hm

-- eval :: Matrix Cell -> Seq (Direction, (Int, Int)) -> Int
-- eval grid path = calc grid path
--   where
--     (_:|>(_,pos)) = path

isNeighbour :: (Int, Int) -> (Int, Int) -> Bool
isNeighbour (y1, x1) (y2, x2) = y1 == y2 && x1 == x2 - 1 || y1 == y2 && x1 == x2 + 1 || x1 == x2 && y1 == y2 - 1 || x1 == x2 && y1 == y2 + 1

-- mainFolder :: State -> Seq (Direction, (Int, Int)) -> State
-- mainFolder (State { grid , path }) path' = solve (State { grid = grid, path = path' })

validBounds :: Int -> Int -> Cell -> Bool
validBounds n m (Cell x y _) = x <= n && y <= m && x > 0 && y > 0

-- Prefer to go East or South
candidateDirections :: (Direction, (Int, Int)) -> [(Direction, (Int, Int))]
candidateDirections (North, (y, x)) = [(East,  (y, x + 1)), (West,  (y, x - 1)), (North, (y - 1, x))]
candidateDirections (East,  (y, x)) = [(East,  (y, x + 1)), (South, (y + 1, x)), (North, (y - 1, x))]
candidateDirections (South, (y, x)) = [(South, (y + 1, x)), (East,  (y, x + 1)), (West,  (y, x - 1))]
candidateDirections (West,  (y, x)) = [(South, (y + 1, x)), (North, (y - 1, x)), (West,  (y, x - 1))]

estimate :: Int -> Int -> Int -> Cell -> Int
estimate n m targetValue (Cell x y _)
  | x == n && y == m = 0
  | otherwise = n + m + targetValue - x - y - 1