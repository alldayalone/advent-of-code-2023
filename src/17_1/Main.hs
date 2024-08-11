{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main (main) where
import Debug.Trace (trace)

import Data.Matrix as Matrix (Matrix(..), (!), setElem, fromLists)
import Data.Char (digitToInt)
import Data.Maybe as Maybe (isJust)
import Data.Sequence as Sequence (Seq (..), reverse, filter, take, findIndexL, singleton, empty, viewr)
import Data.List (filter, sortBy)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- readFile "src/17_1/input.txt"
  estimatesFile <- readFile "src/17_1/estimates3.txt"
  print . solve (parseEstimates estimatesFile) maxBound . parse $ contents

parse = initialState . fmap initialCell . Matrix.fromLists . lines

parseEstimates :: String -> Matrix Int
parseEstimates = fmap read . Matrix.fromLists . map (splitOn " ") . lines

n = 141

data Cell = Cell {
  heatLoss :: Int,
  minResult :: Int
}

instance Show Cell where
  show Cell { heatLoss, minResult } = show minResult

initialCell :: Char -> Cell
initialCell char = Cell { heatLoss = digitToInt char, minResult = maxBound }

data Direction = North | East | South | West deriving (Eq, Show)
data State = State {
  grid :: Matrix Cell,
  path :: Seq (Direction, (Int, Int))
}

instance Show State where
  show State { grid, path } = show grid

initialState :: Matrix Cell -> State
initialState grid = State { grid = zeroFirstCell grid, path = Sequence.singleton (East, (1, 1)) }

zeroFirstCell :: Matrix Cell -> Matrix Cell
zeroFirstCell = Matrix.setElem (Cell { heatLoss = 0, minResult = 0 }) (1, 1)

solve :: Matrix Int ->  Int ->  State -> Int
solve _ best _ | trace (show best) False = undefined
solve estimates best  State { grid, path }
  | (\x -> length x == 4 && alleq Nothing x) . fmap fst . Sequence.take 4 . Sequence.reverse $ path = best
  | calc grid path + estimates Matrix.! pos > best = best
  | Maybe.isJust (Sequence.findIndexL (\(_, p) -> p == pos) initPath) = best
  | (>1) . length . Sequence.filter (\(_, pos2) -> isNeighbour pos pos2) $ initPath = best
  | pos == (n, n) = min (calc grid path) best
  | otherwise = foldl (solve estimates) best $ map (\path -> State { grid, path }) candidatePaths
  where
    (initPath:|>(dir, pos)) = path
    (y,x) = pos
    cell = grid Matrix.! pos
    candidatePaths = map (path :|>) . sortBy (sorter grid estimates) . Data.List.filter validBounds . candidateDirections $ (dir, pos)

sorter :: Matrix Cell -> Matrix Int -> (Direction, (Int, Int)) ->  (Direction, (Int, Int)) -> Ordering
sorter grid estimates (_, pos1) (_,pos2) = compare ( estimates Matrix.! pos1) (estimates Matrix.! pos2)

calc :: Matrix Cell -> Seq (Direction, (Int, Int)) -> Int
calc grid = foldl (\acc (_, pos) -> acc + heatLoss (grid Matrix.! pos)) 0

calcEstimate :: Matrix Cell -> Seq (Direction, (Int, Int)) -> Int
calcEstimate grid (_:|>(_,(y,x))) = (n - y + n - x) * 4 + 1

isNeighbour :: (Int, Int) -> (Int, Int) -> Bool
isNeighbour (y1, x1) (y2, x2) = y1 == y2 && x1 == x2 - 1 || y1 == y2 && x1 == x2 + 1 || x1 == x2 && y1 == y2 - 1 || x1 == x2 && y1 == y2 + 1

-- mainFolder :: State -> Seq (Direction, (Int, Int)) -> State
-- mainFolder (State { grid , path }) path' = solve (State { grid = grid, path = path' })

newHeatLoss :: Seq (Direction, (Int, Int)) -> Matrix Cell -> Int
newHeatLoss Sequence.Empty grid = 0
newHeatLoss (_:|>(_, prevPos):|>(dir, pos)) grid = minResult (grid Matrix.! prevPos) + heatLoss (grid Matrix.! pos)
newHeatLoss (_:|>(dir, pos)) grid = heatLoss $ grid Matrix.! pos

validBounds :: (Direction, (Int, Int)) -> Bool
validBounds (_, (x, y)) = x <= n && y <= n && x > 0 && y > 0

-- Prefer to go East or South
candidateDirections :: (Direction, (Int, Int)) -> [(Direction, (Int, Int))]
candidateDirections (North, (y, x)) = [(East,  (y, x + 1)), (West,  (y, x - 1)), (North, (y - 1, x))]
candidateDirections (East,  (y, x)) = [(East,  (y, x + 1)), (South, (y + 1, x)), (North, (y - 1, x))]
candidateDirections (South, (y, x)) = [(South, (y + 1, x)), (East,  (y, x + 1)), (West,  (y, x - 1))]
candidateDirections (West,  (y, x)) = [(South, (y + 1, x)), (North, (y - 1, x)), (West,  (y, x - 1))]

alleq :: Eq a => Maybe a -> Seq a -> Bool
alleq _ Sequence.Empty = True
alleq Nothing (h:<|t ) = alleq (Just h) t
alleq (Just e) (h:<|t)
  | h == e = alleq (Just e) t
  | otherwise = False