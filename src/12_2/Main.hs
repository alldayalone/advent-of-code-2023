module Main (main, solve) where
import Data.List.Split (splitOn,split)

main :: IO ()
main = do
  contents <- readFile "src/12_2/input_test.txt"
  print .  map (solve' . parse) . lines $ contents

parse :: String -> (String, [Int])
parse line = (unfoldedStrings, unfoldedNumbers)
  where 
    (hotStringsStr : lengthStr : _) = splitOn " " line
    lengths = map read $ splitOn "," lengthStr
    unfoldedStrings = unfoldTheRecords hotStringsStr "?" 5
    unfoldedNumbers = unfoldTheRecords lengths [] 5

solve' :: (String, [Int]) -> Int
solve' (s, i) = solve s i Nothing (max 0 (sum i - count '#' s))

solve :: String -> [Int] -> Maybe Char -> Int -> Int
solve [] [] _ _ = 1
solve [] _ _ _ = 0
solve ('.':s) _ (Just '#') _ = 0
solve ('.':s) i _ c = solve s i Nothing c
solve ('#':s) _ (Just '.') _ = 0
solve ('#':s) [] _ _ = 0
solve ('#':s) (1:is) _ c = solve s is (Just '.') c
solve ('#':s) (x:is) _ c = solve s (x-1:is) (Just '#') c
solve ('?':s) i b 0 = solve ('.':s) i b 0
solve ('?':s) i Nothing c = solve ('?':s) i (Just '#') (c-1) + solve ('?':s) i (Just '.') c
solve ('?':s) i (Just ch) c = solve (ch:s) i (Just ch) c

unfoldTheRecords :: [a] -> [a] -> Int -> [a]
unfoldTheRecords x _ 0 = x
unfoldTheRecords x separator a = x ++ separator ++ unfoldTheRecords x separator (a-1)

count :: (Foldable t, Eq a, Num b) => a -> t a -> b
count a = foldr (\x sum -> if x == a then sum+1 else sum) 0
