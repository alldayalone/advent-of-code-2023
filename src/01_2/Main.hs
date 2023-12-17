import System.IO
import Data.List.Split (splitOn)
import Data.List (isPrefixOf, isSuffixOf)

main = do
  contents <- readFile "src/01_2/input.txt"
  (print . sum . map parseLine . lines) contents

firstInt :: [Char] -> Int
firstInt [] = error "Int not found"
firstInt all@(x:xs)
  | x `elem` ['0'..'9'] = read [x]
  | "one" `isPrefixOf` all = 1
  | "two" `isPrefixOf` all = 2
  | "three" `isPrefixOf` all = 3
  | "four" `isPrefixOf` all = 4
  | "five" `isPrefixOf` all = 5
  | "six" `isPrefixOf` all = 6
  | "seven" `isPrefixOf` all = 7
  | "eight" `isPrefixOf` all = 8
  | "nine" `isPrefixOf` all = 9
  | otherwise = firstInt xs


lastInt :: [Char] -> Int
lastInt [] = error "Int not found"
lastInt all
  | last' `elem` ['0'..'9'] = read [last']
  | "one" `isSuffixOf` all = 1
  | "two" `isSuffixOf` all = 2
  | "three" `isSuffixOf` all = 3
  | "four" `isSuffixOf` all = 4
  | "five" `isSuffixOf` all = 5
  | "six" `isSuffixOf` all = 6
  | "seven" `isSuffixOf` all = 7
  | "eight" `isSuffixOf` all = 8
  | "nine" `isSuffixOf` all = 9
  | otherwise = (lastInt . init) all
  where last' = last all

parseLine :: [Char] -> Int
parseLine line = firstInt line * 10 + lastInt line