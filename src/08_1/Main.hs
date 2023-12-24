module Main (main) where
import Data.List (find)

main :: IO ()
main = do
  contents <- readFile "src/08_1/input.txt"
  print . iterationsCount . parseInput $ contents

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

iterationsCount :: (String, [(String, String, String)]) -> Int
iterationsCount (p, list) = length . takeWhile (/= "ZZZ") $ scanl iterate "AAA" $ (take 100000 . cycle) p
  where
    iterate :: String -> Char -> String
    iterate current direction = case find (\(x,_,_) -> x == current) list of
      Just (_,l,r) -> case direction of
        'L' -> l
        'R' -> r
        _ -> error "Invalid input"
      Nothing -> error "Invalid input"
