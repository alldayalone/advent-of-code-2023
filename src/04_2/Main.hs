{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
import System.IO
import Data.List.Split (splitOn, splitOneOf)
import Data.List (subsequences, stripPrefix, isPrefixOf, isSuffixOf, permutations, elemIndex, intersect)
import GHC.Plugins (fstOf3, sndOf3, count)

main = do
  contents <- readFile "src/04_1/input.txt"
  (print . sum . countInstances . map (length . actualWinNums . map (splitOn " | ") . splitOn ": ") . lines) contents

points :: Int -> Int
points x = if x > 0 then 2 ^ (x-1) else 0
actualWinNums :: [[String]] -> [Int]
actualWinNums card = winNums card `intersect` cardNums card
cardId :: Read c => [[String]] -> c
cardId ((cardMeta:_):_) = (read . last . words) cardMeta
winNums card@(_:((winNumsRaw:_):_)) = (map read . words) winNumsRaw
cardNums card@(_:((_:cardNumsRaw:_):_)) = (map read . words) cardNumsRaw


countInstances :: [Int] -> [Int]
countInstances = snd . foldl fn (repeat 1, [0])
  where
    fn :: ([Int], [Int]) -> Int -> ([Int], [Int])
    fn (x:xs, acc) pts = (zipsum xs (replicate pts x), acc ++ [x])

zipsum :: [Int] -> [Int] -> [Int]
zipsum [] ys = ys
zipsum xs [] = xs
zipsum (x:xs) (y:ys) = (x + y) : zipsum xs ys
-- slice :: Int -> Int -> [a] -> [a]
-- slice from to xs = take (to - from + 1) (drop from xs)