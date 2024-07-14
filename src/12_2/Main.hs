module Main (main, solve) where
import Data.List.Split (splitOn,split)

main :: IO ()
main = do
  contents <- readFile "src/12_2/input.txt"
  mapM_ print . map (solve' . parse) . lines $ contents

parse :: String -> (String, [Int])
parse line = (unfoldedStrings, unfoldedNumbers)
  where 
    (hotStringsStr : lengthStr : _) = splitOn " " line
    lengths = map read $ splitOn "," lengthStr
    unfoldedStrings = unfoldTheRecords hotStringsStr "?" 2
    unfoldedNumbers = unfoldTheRecords lengths [] 2

solve' :: (String, [Int]) -> Int
solve' (s, i) = solve s i Nothing (brokenCount - hashesCount) (questionsCount - (brokenCount - hashesCount))
  where
    questionsCount = count '?' s
    hashesCount = count '#' s
    brokenCount = sum i

solve :: String -> [Int] -> Maybe Char -> Int -> Int -> Int
solve []     []      _          c g = 1
solve []      _      _          c g = 0
solve ('.':s) _      (Just '#') c g = 0
solve ('.':s) i      _          c g = solve s i Nothing c g
solve ('#':s) _      (Just '.') c g = 0
solve ('#':s) []     _          c g = 0
solve ('#':s) (1:is) _          c g = solve s is (Just '.') c g
solve ('#':s) (x:is) _          c g = solve s (x-1:is) (Just '#') c g
solve ('?':s) i      b          c 0 = solve ('#':s) i b (c - 1) 0
solve ('?':s) i      b          0 g = solve ('.':s) i b 0 (g - 1)
solve ('?':s) i      (Just '#') c g = solve ('#':s) i Nothing (c - 1) g
solve ('?':s) i      (Just '.') c g = solve ('.':s) i Nothing c (g - 1)
solve ('?':s) i      Nothing    c g = solve ('.':s) i Nothing c (g - 1) + solve ('#':s) i Nothing (c - 1) g

unfoldTheRecords :: [a] -> [a] -> Int -> [a]
unfoldTheRecords x _ 1 = x
unfoldTheRecords x separator a = x ++ separator ++ unfoldTheRecords x separator (a-1)

count :: (Foldable t, Eq a, Num b) => a -> t a -> b
count a = foldr (\x sum -> if x == a then sum+1 else sum) 0
