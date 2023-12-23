module Main (main) where

main :: IO ()
main = do
  contents <- readFile "src/06_1/input.txt"
  (print . product . map solve . parseInput) contents

type Race = (Int, Int) -- (time, max distance)

parseInput :: String -> [Race]
parseInput inputFile = zip (head input) (input !! 1)
  where input = map (map read . tail . words) . lines $ inputFile

distance :: Int -> Int -> Int
distance t x = x * (t - x) 

-- How many x so that x^2 - tx + s < 0?
solve :: Race -> Int
solve (t, s) = x2 - x1 - 1
  where 
    discriminant = t^2 - 4 * s
    x1 = floor $ (fromIntegral t - sqrt (fromIntegral discriminant)) / 2
    x2 = ceiling $ (fromIntegral t + sqrt (fromIntegral discriminant)) / 2


