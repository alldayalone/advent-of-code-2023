module Main (main) where

-- Algo draft
-- 1. pop the queue
-- 2. get new candidates based on the cell
-- 3. update states
-- 4. continue while queue is not empty

main :: IO ()
main = do
  contents <- readFile "src/16_1/input_test.txt"
  print . lines $ contents
