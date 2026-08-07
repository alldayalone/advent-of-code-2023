module Main (main) where
import Control.Arrow
import Data.Time
import Data.List.Split

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/22_1/input_test.txt"
  writeFile ("src/22_1/output" ++ show utcNow ++ ".txt") . show . solve . parse $ contents

parse = id

solve = id