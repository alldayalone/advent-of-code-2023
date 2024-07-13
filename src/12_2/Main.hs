module Main (main, solve) where
import Data.List.Split (splitOn,split)

main :: IO ()
main = do
  contents <- readFile "src/12_2/input_test.txt"
  print . sum . map (solve' . parse) . take 1 . drop 1 . lines $ contents

parse :: String -> (String, [Int])
parse line = (unfoldedStrings, unfoldedNumbers)
  where 
    (hotStringsStr : lengthStr : _) = splitOn " " line
    lengths = map read $ splitOn "," lengthStr
    unfoldedStrings = unfoldTheRecords hotStringsStr "?" 5
    unfoldedNumbers = unfoldTheRecords lengths [] 5

solve' :: (String, [Int]) -> Int
solve' (s, i) = solve s i Nothing

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