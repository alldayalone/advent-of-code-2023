module Main (main) where
import Debug.Trace (trace)
import Data.List (transpose)

-- Plan:
-- Rewrite solve into 2 separate function - moving Os and calculating the load
-- Write a function that takes current matrix and direction and returns the next matrix
-- Start iterating cycles, and find the pattern - when matrix + direction is repeated
-- Skip unnecessary cycles mathematically

main :: IO ()
main = do
  contents <- readFile "src/14_2/input.txt"
  print . sum . map solve . parse $ contents

-- | Transpose to get the columns
parse :: String -> [String]
parse = transpose . lines

solve :: String -> Int
solve s = fn 0 0 n n s 
  where n = length s

-- | tv - total value
-- | lc - local count (number of elements in the current group)
-- | tcd - total count down
-- | lcd - local count down (index ot the last #)
-- | Ex. load 10 3 = 10 + 9 + 8 = 27
fn :: Int -> Int -> Int -> Int -> String -> Int
-- fn tv lc tcd lcd v | trace ("fn " ++ show tv ++ " " ++ show lc ++ " " ++ show tcd ++ " " ++ show lcd ++ " " ++ v) False = undefined
fn tv lc tcd lcd  []      = tv + value lcd lc
fn tv lc tcd lcd ('#':xs) = fn (tv + value lcd lc) 0        (tcd - 1) (tcd - 1) xs
fn tv lc tcd lcd ('O':xs) = fn                 tv  (lc + 1) (tcd - 1) lcd       xs
fn tv lc tcd lcd ('.':xs) = fn                 tv  lc       (tcd - 1) lcd       xs

-- | a - index
-- | n - total number of elements
-- | Ex. load 10 3 = 10 + 9 + 8 = 27
value :: Int -> Int -> Int
value a n = n * (2 * a - n + 1) `div` 2
