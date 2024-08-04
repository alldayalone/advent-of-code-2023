{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where
import Debug.Trace (trace)

import Data.Matrix as Matrix (Matrix(..), (!), setElem, fromLists)
import Data.Char (digitToInt)
main :: IO ()
main = do
  contents <- readFile "src/17_1/input_test2.txt"
  print . solve . parse $ contents

parse = initialState . fmap initialCell . Matrix.fromLists . lines

data Cell = Cell { 
  h :: Int, -- heat loss
  v :: Bool -- visited
} deriving (Show)
initialCell :: Char -> Cell
initialCell char = Cell { h = digitToInt char, v = False }

data Direction = North | East | South | West deriving (Show)
data State = State { 
  g :: Matrix Cell, -- grid
  c :: (Direction, (Int, Int)), -- current (dir, pos) 
  th :: Int, -- total heat lost
  s :: Int -- steps count
} deriving (Show)
initialState :: Matrix Cell -> State
initialState g = State { g = g, c = (West, (1, 1)), th = 0, s = 0 }

solve :: State -> Int
-- turn s | trace ("turn " ++ show s) False = undefined
solve s | trace ("solve " ++ show s) False = undefined
solve State { g, c = (d, p), th, s }
  | invalidBounds p g || v cell = maxBound
  | fst p == Matrix.ncols g && snd p == Matrix.nrows g = th
  | otherwise = minimum $ map solve candidateStates
  where
      cell = g Matrix.! p
      th' = th + h cell
      s' = s + 1
      g' = Matrix.setElem (Cell {h = h cell, v = True}) p g
      candidateDirections = map (\f -> f (d, p)) [goStraight, turnLeft, turnRight]
      candidateStates = map (\c' -> State {g = g', c = c', th = th', s = s'}) candidateDirections
            
invalidBounds :: (Int, Int) -> Matrix Cell -> Bool
invalidBounds (x, y) matrix = x < 1 || y < 1 || x > Matrix.ncols matrix || y > Matrix.nrows matrix

goStraight :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
goStraight (North, (x, y)) = (North, (x, y - 1))
goStraight (East, (x, y)) = (East, (x + 1, y))
goStraight (South, (x, y)) = (South, (x, y + 1))
goStraight (West, (x, y)) = (West, (x - 1, y))

turnLeft :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
turnLeft (North, (x, y)) = (West, (x - 1, y))
turnLeft (East, (x, y)) = (North, (x, y - 1))
turnLeft (South, (x, y)) = (East, (x + 1, y))
turnLeft (West, (x, y)) = (South, (x, y + 1))

turnRight :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
turnRight (North, (x, y)) = (East, (x + 1, y))
turnRight (East, (x, y)) = (South, (x, y + 1))
turnRight (South, (x, y)) = (West, (x - 1, y))
turnRight (West, (x, y)) = (North, (x, y - 1))