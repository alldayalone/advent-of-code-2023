{-# LANGUAGE TupleSections #-}

module Main (main) where
import Data.Matrix as Matrix (Matrix, fromLists, extendTo, setElem)

main :: IO ()
main = do
  contents <- readFile "src/18_1/input_test.txt"
  print . buildMatrix . parse $ contents

type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

parse :: String -> [Instruction]
parse = map parseLine . lines

parseLine :: String -> Instruction
parseLine line = (direction, read number, color)
  where
    [direction, number, color] = words line

buildMatrix = foldl applyInstruction (Matrix.fromLists [["#"]], (1, 1))

applyInstruction :: (Matrix String, (Int, Int)) -> Instruction -> (Matrix String, (Int, Int))
applyInstruction (m, (x, y)) (direction, number, color) = (m'', pos')
  where 
    m' = uncurry (Matrix.extendTo ".") pos' m
    m'' = foldr (Matrix.setElem "#") m' (posrange (x,y) pos') 
    pos' = case direction of
      "R" -> (x, y + number)
      "L" -> (x, y - number)
      "U" -> (x - number, y)
      "D" -> (x + number, y)
      _ -> error "Wrong direction"

posrange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
posrange (x1, y1) (x2, y2)
  | x1 > x2 = map (,y1) [x1-1,x1-2..x2]
  | x1 < x2 = map (,y1) [x1+1,x1+2..x2]
  | y1 < y2 = map (x1,) [y1+1,y1+2..y2]
  | y1 > y2 = map (x1,) [y1-1,y1-2..y2]