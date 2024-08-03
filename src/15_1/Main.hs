module Main (main) where

main :: IO ()
main = do
  contents <- readFile "src/15_1/input_test.txt"
  print . lines $ contents