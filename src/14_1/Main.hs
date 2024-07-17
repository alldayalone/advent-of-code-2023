module Main (main) where
import Data.Matrix

main :: IO ()
main = do
  contents <- readFile "src/14_1/input_test.txt"
  print . parse $ contents

parse = fromLists . lines