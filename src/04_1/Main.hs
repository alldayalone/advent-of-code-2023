{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
import System.IO
import Data.List.Split (splitOn, splitOneOf)
import Data.List (subsequences, stripPrefix, isPrefixOf, isSuffixOf, permutations, elemIndex, intersect)
import GHC.Plugins (fstOf3, sndOf3, count)

main = do
  contents <- readFile "src/04_1/input.txt"
  (print . sum . map (points . length . actualWinNums . map (splitOn " | ") . splitOn ": ") . lines) contents

points :: Int -> Int
points x = if x > 0 then 2 ^ (x-1) else 0
actualWinNums :: [[String]] -> [Int]
actualWinNums card = winNums card `intersect` cardNums card
cardId :: Read c => [[String]] -> c
cardId ((cardMeta:_):_) = (read . last . words) cardMeta
winNums card@(_:((winNumsRaw:_):_)) = (map read . words) winNumsRaw
cardNums card@(_:((_:cardNumsRaw:_):_)) = (map read . words) cardNumsRaw