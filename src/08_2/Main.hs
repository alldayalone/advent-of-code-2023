module Main (main) where
import Data.List (find, findIndices)

main :: IO ()
main = do
  contents <- readFile "src/08_2/input.txt"
  print . foldl1 lcm . process . parseInput $ contents

parseInput :: String -> (String, [(String, String, String)])
parseInput = parse1 . lines

parse1 :: [String] -> (String, [(String, String, String)])
parse1 (p:_:list) = (p, map (tuplify3 . words . filter (`notElem` "=(),")) list)
parse1 _ = error "Invalid input"

tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)
tuplify3 _ = error "Invalid input"

fstOf3 :: (a, b, c) -> a
fstOf3 (x, _, _) = x

endpoint :: String -> Bool
endpoint = (== 'Z') . last

process :: (String, [(String, String, String)]) -> [Int]
process (p,list) = map ((\x -> x !! 1 - head x) . iterationsCount (p, list)) starts
  where starts = filter ((== 'A') . last) . map fstOf3 $ list

iterationsCount :: (String, [(String, String, String)]) -> String -> [Int]
iterationsCount (p, list) accStart = findIndices (any endpoint) $ scanl iterate [accStart] $ (take 100000 . cycle) p
  where
    iterate :: [String] -> Char -> [String]
    iterate current direction = map (`iterateOne` direction) current

    iterateOne :: String -> Char -> String
    iterateOne current direction = case find (\(x,_,_) -> x == current) list of
      Just (_,l,r) -> case direction of
        'L' -> l
        'R' -> r
        _ -> error "Invalid input"
      Nothing -> error "Invalid input"