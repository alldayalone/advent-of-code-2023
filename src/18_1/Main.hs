module Main (main) where

main :: IO ()
main = do
  contents <- readFile "src/18_1/input_test.txt"
  print . parse $ contents

type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

parse :: String -> [Instruction]
parse = map parseLine . lines

parseLine :: String -> Instruction
parseLine line = (direction, read number, color)
  where
    [direction, number, color] = words line
