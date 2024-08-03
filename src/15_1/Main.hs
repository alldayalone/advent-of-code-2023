module Main (main, hash) where
import Data.Char (ord)

main :: IO ()
main = do
  contents <- readFile "src/15_1/input_test.txt"
  print . lines $ contents

hash :: String -> Int
hash = hash' 0
  where
    hash' :: Int -> String -> Int
    hash' currentValue [] = currentValue
    hash' val (x:xs) = hash' (flip mod 256 . (* 17) . (+ val) . ord $ x) xs
