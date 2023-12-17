import System.IO
import Data.List.Split (splitOn)

main = do
  contents <- readFile "src/01_1/input_test.txt"
  (print . sum . map parseLine . lines) contents


firstInt :: [Char] -> Int
firstInt [] = error "Int not found"
firstInt (x:xs)
  | x `elem` ['0'..'9'] = read [x]
  | otherwise = firstInt xs

lastInt :: [Char] -> Int
lastInt = firstInt . reverse

parseLine :: [Char] -> Int
parseLine line = firstInt line * 10 + lastInt line