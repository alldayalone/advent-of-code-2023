module Main (main, hash) where
import Data.Char (ord)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- readFile "src/15_2/input_test.txt"
  print . splitOn "," $ contents

hash :: String -> Int
hash = hash' 0
  where
    hash' :: Int -> String -> Int
    hash' currentValue [] = currentValue
    hash' val (x:xs) = hash' (flip mod 256 . (* 17) . (+ val) . ord $ x) xs
