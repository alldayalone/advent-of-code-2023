module Main (main) where

main :: IO ()
main = do
  contents <- readFile "src/14_1/input_test.txt"
  print . parse $ contents

parse = lines