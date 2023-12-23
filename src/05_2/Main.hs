module Main (
  main,breakPointsToRanges,rangesToBreakPoints,splitRangeByBreakPoints,mapSeed,mapSeeds
) where
import Data.List.Split (splitOn)
import Data.List (sort, find)
import GHC.Plugins (sndOf3, sortWith)
import GHC.OldList (nub)
import qualified Data.Bifunctor

main :: IO ()
main = do
  contents <- readFile "src/05_2/input.txt"
  (print . minimum . map fst . process . parseInput) contents

process :: ([(Int, Int)], [[(Int, Int, Int)]]) -> [(Int, Int)]
process (seeds, mappers) = foldl mapSeeds seeds mappers

parseInput :: String -> ([(Int, Int)], [[(Int, Int, Int)]])
parseInput contents = (seeds, maps)
  where
    splitted = splitOn "\n\n" contents
    seedStr = head splitted
    mapsStr = tail splitted
    seeds = (constructRange . sortRange . map read . splitOn " ") (splitOn ": " seedStr !! 1)
    maps = map (constructRange3 . sortRange3 . concatMap (map read . splitOn " ") . drop 1 . splitOn "\n") mapsStr


constructRange :: [Int] -> [(Int,Int)]
constructRange [] = []
constructRange [_] = error "constructRange: odd number of elements"
constructRange (x:y:xs) = (x,y) : constructRange xs

deconstructRange :: [(Int,Int)] -> [Int]
deconstructRange [] = []
deconstructRange ((x,y):xs) = x:y:deconstructRange xs

sortRange' :: [(Int,Int)] -> [(Int,Int)]
sortRange' = sortWith fst

sortRange :: [Int] -> [Int]
sortRange = deconstructRange . sortRange' . constructRange

constructRange3 :: [Int] -> [(Int,Int,Int)]
constructRange3 [] = []
constructRange3 [_] = error "constructRange3: odd number of elements"
constructRange3 [_,_] = error "constructRange3: odd number of elements"
constructRange3 (x:y:z:xs) = (x,y,z) : constructRange3 xs

deconstructRange3 :: [(Int,Int,Int)] -> [Int]
deconstructRange3 [] = []
deconstructRange3 ((x,y,z):xs) = x:y:z:deconstructRange3 xs

sortRange3' :: [(Int,Int,Int)] -> [(Int,Int,Int)]
sortRange3' = sortWith sndOf3

sortRange3 :: [Int] -> [Int]
sortRange3 = deconstructRange3 . sortRange3' . constructRange3

-- All numbers should be unique and sorted ascending
breakPointsToRanges :: [Int] -> [(Int, Int)]
breakPointsToRanges [] = []
breakPointsToRanges [x] = breakPointsToRanges [x, x]
breakPointsToRanges [x, y] = [(x, y - x + 1)]
breakPointsToRanges (x:y:xs) = (x, y - x) : breakPointsToRanges (y:xs)

rangesToBreakPoints :: (Int, Int) -> [(Int, Int, Int)] -> [Int]
rangesToBreakPoints (x, 1) _ = [x]
rangesToBreakPoints (x, y) [] = [x, x + y - 1]
rangesToBreakPoints (x, y) ranges = sort . nub $ [x, x + y - 1] ++ concatMap (filter (\n -> x < n && n < x + y - 1) . (\(_, b, c) -> [b, b + c])) ranges

splitRangeByBreakPoints :: (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
splitRangeByBreakPoints range = breakPointsToRanges . rangesToBreakPoints range

mapSeed :: (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
mapSeed seed mapper = map (fn mapper) (splitRangeByBreakPoints seed mapper)
  where
    fn :: [(Int, Int, Int)] -> (Int, Int) -> (Int, Int)
    fn mapper' seed' = case find (\(_, src, len) -> src <= fst seed' && fst seed' < src + len) mapper' of Just (dest, src, _) -> Data.Bifunctor.first ((dest - src) +) seed'; Nothing -> seed'

mapSeeds :: [(Int, Int)] -> [(Int, Int, Int)]  -> [(Int, Int)]
mapSeeds seeds mapper = concatMap (`mapSeed` mapper) seeds