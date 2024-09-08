module Main (main) where

main :: IO ()
main = do
  contents <- readFile "src/18_1/input_test.txt"
  print . parse $ contents

parse = lines
