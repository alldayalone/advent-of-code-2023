module Main (main, solve) where
import Data.List.Split (splitOn,split)

main :: IO ()
main = do
  contents <- readFile "src/12_2/input_test.txt"
  print  . map (solve' . parse) . lines $ contents

parse :: String -> (String, [Int])
parse line = (hotStringsStr, lengths)
  where 
    (hotStringsStr : lengthStr : _) = splitOn " " line
    lengths = map read $ splitOn "," lengthStr

solve' :: (String, [Int]) -> Int
solve' (s, i) = classicSolution * unfoldedSolution ^ 4
  where 
    classicSolution = solve s i Nothing
    unfoldedSolution = solve (s++"?") i Nothing

solve :: String -> [Int] -> Maybe Char -> Int
solve [] [] _ = 1
solve [] _ _ = 0
solve ('.':s) _ (Just '#') = 0
solve ('.':s) i _ = solve s i Nothing
solve ('#':s) _ (Just '.') = 0
solve ('#':s) [] _ = 0
solve ('#':s) (1:is) _ = solve s is (Just '.')
solve ('#':s) (x:is) _ = solve s (x-1:is) (Just '#')
solve ('?':s) i Nothing = solve ('?':s) i (Just '#') + solve ('?':s) i (Just '.')
solve ('?':s) i (Just ch) = solve (ch:s) i (Just ch)

unfoldTheRecords :: [a] -> [a] -> Int -> [a]
unfoldTheRecords x _ 0 = x
unfoldTheRecords x separator a = unfoldTheRecords (x ++ separator ++ x) separator (a-1)