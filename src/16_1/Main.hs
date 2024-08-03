module Main (main) where
import Cell (Cell(..), addLight, north, empty, lightExists, energized)
import Data.Matrix as Matrix (Matrix, fromLists, setElem, (!), ncols, nrows)
import Debug.Trace (trace)

-- Algo draft
-- 1. pop the queue
-- 2. get new candidates based on the cell
-- 3. update states
-- 4. continue while queue is not empty

main :: IO ()
main = do
  contents <- readFile "src/16_1/input.txt"
  print . sum . fmap Cell.energized . tick [((1,1),2)] . parse $ contents

parse = fmap Cell.empty . Matrix.fromLists . lines

tick :: [((Int, Int), Int)] -> Matrix Cell -> Matrix Cell
-- tick ((pos, dir):xs) _ | trace (show (pos, dir)) False = undefined
tick [] matrix = matrix
tick ((pos, dir):xs) matrix = if skip (pos, dir) matrix then tick xs matrix else tick (newCandidates ++ xs) $ Matrix.setElem cell pos matrix
  where
    cell = addLight dir (matrix Matrix.! pos)
    newCandidates = case (char cell, dir) of
      ('.', 1) -> [((fst pos - 1, snd pos), 1)]
      ('.', 2) -> [((fst pos, snd pos + 1), 2)]
      ('.', 4) -> [((fst pos + 1, snd pos), 4)]
      ('.', 8) -> [((fst pos, snd pos - 1), 8)]
      ('/', 1) -> [((fst pos, snd pos + 1), 2)]
      ('/', 2) -> [((fst pos - 1, snd pos), 1)]
      ('/', 4) -> [((fst pos, snd pos - 1), 8)]
      ('/', 8) -> [((fst pos + 1, snd pos), 4)]
      ('\\', 1) -> [((fst pos, snd pos - 1), 8)]
      ('\\', 2) -> [((fst pos + 1, snd pos), 4)]
      ('\\', 4) -> [((fst pos, snd pos + 1), 2)]
      ('\\', 8) -> [((fst pos - 1, snd pos), 1)]
      ('|', 2) -> [((fst pos - 1, snd pos), 1), ((fst pos + 1, snd pos), 4)]
      ('|', 8) -> [((fst pos - 1, snd pos), 1), ((fst pos + 1, snd pos), 4)]
      ('|', 1) -> [((fst pos - 1, snd pos), 1)]
      ('|', 4) -> [((fst pos + 1, snd pos), 4)]
      ('-', 1) -> [((fst pos, snd pos - 1), 8), ((fst pos, snd pos + 1), 2)]
      ('-', 4) -> [((fst pos, snd pos - 1), 8), ((fst pos, snd pos + 1), 2)]
      ('-', 2) -> [((fst pos, snd pos + 1), 2)]
      ('-', 8) -> [((fst pos, snd pos - 1), 8)]
      _ -> []

invalidBounds :: (Int, Int) -> Matrix Cell -> Bool
invalidBounds (x, y) matrix = x < 1 || y < 1 || x > Matrix.ncols matrix || y > Matrix.nrows matrix

skip :: ((Int, Int), Int) -> Matrix Cell -> Bool
skip (pos, dir) matrix = invalidBounds pos matrix || Cell.lightExists dir (matrix Matrix.! pos)