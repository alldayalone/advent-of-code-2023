module Main (main) where
import Data.Time
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/20_1/input_test.txt"
  writeFile ("src/20_1/output" ++ show utcNow ++ ".txt") . ppShow  . solve . parse $ contents

type Input = String
type Output = String

parse :: String -> Input
parse = id

solve :: Input -> Output
solve = id
