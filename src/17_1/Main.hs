{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main (main) where
import Debug.Trace (trace)

import Data.Matrix as Matrix (Matrix(..), (!), setElem, fromLists)
import Data.Char (digitToInt)
import Data.Maybe as Maybe (isJust)
import Data.Sequence as Sequence (Seq (..), reverse, filter, take, findIndexL, singleton, empty, viewr)

main :: IO ()
main = do
  contents <- readFile "src/17_1/input.txt"
  print . bestResult' . solve . parse $ contents

parse = initialState . fmap initialCell . Matrix.fromLists . lines

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

solve :: State -> State
solve s | trace (show (bestResult' s)) False = undefined
solve State { grid, path }
  | invalidBounds pos grid = State { grid, path = initPath }
  | (\x -> length x == 4 && alleq Nothing x) . fmap fst . Sequence.take 4 . Sequence.reverse $ path = State { grid, path = initPath }
  | newHeatLoss' > bestResult grid = State { grid, path = initPath }
  -- | Maybe.isJust (Sequence.findIndexL (\(_, p) -> p == pos) initPath) = State { grid, path = initPath }
  | (>1) . length . Sequence.filter (\(_, pos2) -> isNeighbour pos pos2) $ initPath = State { grid, path = initPath }
  | pos == (n, n) = State { grid = grid', path }
  | otherwise = foldl mainFolder (State { grid = grid', path }) candidatePaths
  where
    (initPath:|>(dir, pos)) = path
    (y,x) = pos
    cell = grid Matrix.! pos
    newHeatLoss' = newHeatLoss path grid
    cell' = Cell { heatLoss = heatLoss cell, minResult = newHeatLoss' }
    grid' = Matrix.setElem cell' pos grid
    candidatePaths = map (path :|>) . candidateDirections $ (dir, pos)

isNeighbour :: (Int, Int) -> (Int, Int) -> Bool
isNeighbour (y1, x1) (y2, x2) = y1 == y2 && x1 == x2 - 1 || y1 == y2 && x1 == x2 + 1 || x1 == x2 && y1 == y2 - 1 || x1 == x2 && y1 == y2 + 1

bestResult :: Matrix Cell -> Int
bestResult grid = minResult $ grid Matrix.! (Matrix.nrows grid, Matrix.ncols grid)

bestResult' :: State -> Int
bestResult' State { grid } = bestResult grid

mainFolder :: State -> Seq (Direction, (Int, Int)) -> State
mainFolder (State { grid , path }) path' = solve (State { grid = grid, path = path' })

newHeatLoss :: Seq (Direction, (Int, Int)) -> Matrix Cell -> Int
newHeatLoss Sequence.Empty grid = 0
newHeatLoss (_:|>(_, prevPos):|>(dir, pos)) grid = minResult (grid Matrix.! prevPos) + heatLoss (grid Matrix.! pos)
newHeatLoss (_:|>(dir, pos)) grid = heatLoss $ grid Matrix.! pos

invalidBounds :: (Int, Int) -> Matrix Cell -> Bool
invalidBounds (x, y) matrix = x > n || y > n

-- Prefer to go East or South
candidateDirections :: (Direction, (Int, Int)) -> [(Direction, (Int, Int))]
candidateDirections (North, (1, 1)) = [(East,  (1, 2))]
candidateDirections (North, (1, x)) = [(East,  (1, x + 1)), (West,  (1, x - 1))]
candidateDirections (North, (y, 1)) = [(East,  (y, 2)), (North, (y - 1, 1))]
candidateDirections (North, (y, x)) = [(East,  (y, x + 1)), (West,  (y, x - 1)), (North, (y - 1, x))]

candidateDirections (East,  (1, x)) = [(East,  (1, x + 1)), (South, (2, x))]
candidateDirections (East,  (y, 1)) = [(East,  (y, 2)), (South, (y + 1, 1)), (North, (y - 1, 1))]
candidateDirections (East,  (y, x)) = [(East,  (y, x + 1)), (South, (y + 1, x)), (North, (y - 1, x))]

candidateDirections (South, (1, 1)) = [(South, (2, 1)), (East,  (1, 2))]
candidateDirections (South, (1, x)) = [(South, (2, x)), (East,  (1, x + 1)), (West,  (1, x - 1))]
candidateDirections (South, (y, 1)) = [(South, (y + 1, 1)), (East,  (y, 2))]
candidateDirections (South, (y, x)) = [(South, (y + 1, x)), (East,  (y, x + 1)), (West,  (y, x - 1))]

candidateDirections (West,  (1, 1)) = [(South, (2, 1))]
candidateDirections (West,  (1, x)) = [(South, (2, x)), (West,  (1, x - 1))]
candidateDirections (West,  (y, 1)) = [(South, (y + 1, 1)), (North, (y - 1, 1))]
candidateDirections (West,  (y, x)) = [(South, (y + 1, x)), (North, (y - 1, x)), (West,  (y, x - 1))]

alleq :: Eq a => Maybe a -> Seq a -> Bool
alleq _ Sequence.Empty = True
alleq Nothing (h:<|t )  = alleq (Just h) t 
alleq (Just e) (h:<|t) 
  | h == e = alleq (Just e) t
  | otherwise = False