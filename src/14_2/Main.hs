{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Debug.Trace (trace)
import Data.List (transpose)

-- Plan:
-- Rewrite solve into 2 separate functions:
--  1. Moving Os
--  2. [x] Calculating the load
-- [x] Write a function that takes current matrix and direction and returns the next matrix
-- Start iterating cycles, and find the pattern - when matrix + direction is repeated
-- Skip unnecessary cycles mathematically

main :: IO ()
main = do
  contents <- readFile "src/14_2/input_test.txt"
  print  . parse $ contents

parse :: String -> State
parse = turn . turn . eastState . lines

solve :: String -> Int
solve = calcLoad 

data Direction = North | West | South | East deriving (Show, Bounded, Eq, Enum)
data State = State { matrix :: [String], direction :: Direction } deriving (Show, Eq)

next :: (Eq a, Bounded a, Enum a) => a -> a
next x = if x == maxBound then minBound else succ x

eastState :: [String] -> State
eastState m = State m East

turn :: State -> State
turn (State m dir) = State (transpose m) (next dir)

calcLoad :: String -> Int
-- calcLoad v | trace ("fn " ++ v) False = undefined
calcLoad [] = 0
calcLoad ('O':xs) = length xs + 1 + calcLoad xs
calcLoad (_:xs) = calcLoad xs
