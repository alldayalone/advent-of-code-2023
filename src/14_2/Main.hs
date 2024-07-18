module Main (main) where
import Debug.Trace (trace)
import Data.List (transpose)

-- Plan:
-- Rewrite solve into 2 separate functions:
--  1. Moving Os
--  2. [x] Calculating the load
-- Write a function that takes current matrix and direction and returns the next matrix
-- Start iterating cycles, and find the pattern - when matrix + direction is repeated
-- Skip unnecessary cycles mathematically

main :: IO ()
main = do
  contents <- readFile "src/14_2/input_test.txt"
  print . map solve . parse $ contents

-- | Transpose to get the columns
parse :: String -> [String]
parse = transpose . lines

solve :: String -> Int
solve = calcLoad 

calcLoad :: String -> Int
-- calcLoad v | trace ("fn " ++ v) False = undefined
calcLoad [] = 0
calcLoad ('O':xs) = length xs + 1 + calcLoad xs
calcLoad (_:xs) = calcLoad xs
