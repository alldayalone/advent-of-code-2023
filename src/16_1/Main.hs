module Main (main) where
import Cell (Cell(..), addLight, north, empty)
import Data.Matrix as Matrix (Matrix, fromLists, setElem)

-- Algo draft
-- 1. pop the queue
-- 2. get new candidates based on the cell
-- 3. update states
-- 4. continue while queue is not empty

main :: IO ()
main = do
  contents <- readFile "src/16_1/input_test.txt"
  print . parse $ contents

parse = fmap (Cell.addLight north) . fmap Cell.empty . Matrix.fromLists . lines
