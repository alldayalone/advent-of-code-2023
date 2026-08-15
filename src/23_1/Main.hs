module Main (main) where
import Data.Time
import Control.Arrow
import Data.Matrix (Matrix)
import qualified Data.Matrix as M

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/23_1/input.txt"
  writeFile ("src/23_1/output" ++ show utcNow ++ ".txt") . show . solve . parse $ contents

parse :: String -> Matrix Char
parse = lines >>> M.fromLists

solve = id



