{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Debug.Trace (trace)
import Data.List (transpose)

-- Plan:
-- Rewrite solve into 2 separate functions:
--  1. [x] Moving Os
--  2. [x] Calculating the load
-- [x] Write a function that takes current matrix and direction and returns the next matrix
-- Start iterating cycles, and find the pattern - when matrix + direction is repeated
-- Skip unnecessary cycles mathematically

main :: IO ()
main = do
  contents <- readFile "src/14_2/input.txt"
  print . solve . tilt . parse $ contents

parse :: String -> State
parse = turn . eastState . lines

solve :: State -> Int
solve (State m _) = sum . map calcLoad $ m 

data Direction = North | West | South | East deriving (Show, Bounded, Eq, Enum)
data State = State { matrix :: [String], direction :: Direction } deriving (Show, Eq)

next :: (Eq a, Bounded a, Enum a) => a -> a
next x = if x == maxBound then minBound else succ x

eastState :: [String] -> State
eastState m = State m East

turn :: State -> State
turn (State m dir) = State (transpose m) (next dir)

tilt :: State -> State
tilt (State m dir) = State (map tiltRow m) dir

tiltRow :: String -> String
tiltRow = tiltRow' 0 0

tiltRow' :: Int -> Int -> String -> String
tiltRow' cO cDot [] = generateSection cO cDot
tiltRow' cO cDot ('#':xs) = generateSection cO cDot ++ "#" ++ tiltRow xs
tiltRow' cO cDot ('O':xs) = tiltRow' (cO + 1) cDot xs
tiltRow' cO cDot ('.':xs) = tiltRow' cO (cDot + 1) xs

generateSection :: Int -> Int -> String
generateSection cO cDot = replicate cO 'O' ++ replicate cDot '.'


calcLoad :: String -> Int
-- calcLoad v | trace ("fn " ++ v) False = undefined
calcLoad [] = 0
calcLoad ('O':xs) = length xs + 1 + calcLoad xs
calcLoad (_:xs) = calcLoad xs
