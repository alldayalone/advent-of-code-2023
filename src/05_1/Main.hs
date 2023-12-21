import System.IO
import Data.List.Split (splitOn, splitOneOf)
import Data.List (find, subsequences, stripPrefix, isPrefixOf, isSuffixOf, permutations, elemIndex, intersect)
import GHC.Plugins (fstOf3, sndOf3, count, split)

main = do
  contents <- readFile "src/05_1/input.txt"
  (print . minimum . process . parseInput) contents

process :: ([Int], [[[Int]]]) -> [Int]
process (seeds, maps) = map (\seed -> foldl followMap seed maps) seeds

followMap :: Int -> [[Int]] -> Int
followMap x map = case find (isMatch x) map of
  Nothing -> x
  Just [dest, src, count] -> dest + x - src

isMatch x [_, src, count]  = x >= src && x <= src + count

parseInput :: String -> ([Int], [[[Int]]])
parseInput contents = (seeds, maps)
  where
    (seedStr:mapsStr) = splitOn "\n\n" contents
    seeds = (map read . splitOn " ") (splitOn ": " seedStr !! 1)
    maps = map (map (map read . splitOn " ") . (drop 1 . splitOn "\n")) mapsStr

