{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main (main) where
import Debug.Trace (trace)

import Data.Matrix as Matrix (Matrix(..), (!), setElem, fromLists)
import Data.Char (digitToInt)
import Data.Maybe as Maybe (isJust)
import Data.Sequence as Sequence (Seq (..), findIndexL, singleton, ViewR, empty, viewr)

main :: IO ()
main = do
  contents <- readFile "src/17_1/input_test.txt"
  print . solve . parse $ contents

parse = initialState . fmap initialCell . Matrix.fromLists . lines

data Cell = Cell {
  heatLoss :: Int,
  minResult :: Int
}

instance Show Cell where
  show Cell { heatLoss, minResult } = show minResult

initialCell :: Char -> Cell
initialCell char = Cell { heatLoss = digitToInt char, minResult = maxBound }

data Direction = North | East | South | West deriving (Show)
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
solve s | trace (show s) False = undefined
solve State { grid, path }
  | invalidBounds pos grid = State { grid, path = initPath }
  | newHeatLoss' > minResult cell = State { grid, path = initPath }
  | Maybe.isJust (Sequence.findIndexL (\(_, p) -> p == pos) initPath) = State { grid, path = initPath }
  | pos == (Matrix.nrows grid, Matrix.ncols grid) = State { grid = grid', path }
  | otherwise = foldl mainFolder (State { grid = grid', path }) candidatePaths
  where
    (initPath:|>(dir, pos)) = path
    cell = grid Matrix.! pos
    newHeatLoss' = newHeatLoss path grid
    cell' = Cell { heatLoss = heatLoss cell, minResult = newHeatLoss' }
    grid' = Matrix.setElem cell' pos grid
    candidateDirections = map (\f -> f (dir, pos)) [goStraight, turnRight, turnLeft]
    candidatePaths = map (path :|>) candidateDirections

mainFolder :: State -> Seq (Direction, (Int, Int)) -> State
mainFolder (State { grid , path }) path' = solve (State { grid = grid, path = path' })

newHeatLoss :: Seq (Direction, (Int, Int)) -> Matrix Cell -> Int
newHeatLoss Sequence.Empty grid = 0
newHeatLoss (_:|>(_, prevPos):|>(dir, pos)) grid = minResult (grid Matrix.! prevPos) + heatLoss (grid Matrix.! pos)
newHeatLoss (_:|>(dir, pos)) grid = heatLoss $ grid Matrix.! pos

invalidBounds :: (Int, Int) -> Matrix Cell -> Bool
invalidBounds (x, y) matrix = x < 1 || y < 1 || x > Matrix.ncols matrix || y > Matrix.nrows matrix

goStraight :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
goStraight (North, (y, x)) = (North, (y - 1, x))
goStraight (East, (y, x)) = (East, (y, x + 1))
goStraight (South, (y, x)) = (South, (y + 1, x))
goStraight (West, (y, x)) = (West, (y, x - 1))

turnLeft :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
turnLeft (North, (y, x)) = (West, (y, x - 1))
turnLeft (East, (y, x)) = (North, (y - 1, x))
turnLeft (South, (y, x)) = (East, (y, x + 1))
turnLeft (West, (y, x)) = (South, (y + 1, x))

turnRight :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
turnRight (North, (y, x)) = (East, (y, x + 1))
turnRight (East, (y, x)) = (South, (y + 1, x))
turnRight (South, (y, x)) = (West, (y, x - 1))
turnRight (West, (y, x)) = (North, (y - 1, x))