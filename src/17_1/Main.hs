module Main (main) where
  
import Data.Matrix as Matrix (Matrix(..), fromLists)
import Data.Char (digitToInt)
main :: IO ()
main = do
  contents <- readFile "src/17_1/input_test.txt"
  print . parse $ contents

parse = fmap digitToInt . Matrix.fromLists . lines
