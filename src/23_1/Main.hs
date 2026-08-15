module Main (main) where
import Data.Time

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/23_1/input.txt"
  writeFile ("src/23_1/output" ++ show utcNow ++ ".txt") . show . solve . parse $ contents

parse = id

solve = id



