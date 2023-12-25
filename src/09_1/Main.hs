module Main (main) where
import Data.List (find, findIndices)
import Debug.Trace (trace)

main :: IO ()
main = do
  contents <- readFile "src/09_1/input.txt"
  print . sum . map (backtrack . scan) . parse $ contents

parse :: String -> [[Int]]
parse = map (map read . words) . lines

process :: [Int] -> Int
process = backtrack . scan

scan :: [Int] -> [[Int]]
scan sequence = takeWhileInclusive (any (/=0)) $ scanl fn sequence [1..]

fn :: [Int] -> Int -> [Int]
fn sequence _ = map (uncurry (-)) (zipShift1 sequence)

zipShift1 :: [Int] -> [(Int, Int)]
zipShift1 sequence = zip (drop 1 sequence) sequence

backtrack :: [[Int]] -> Int
backtrack = sum . map last

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []