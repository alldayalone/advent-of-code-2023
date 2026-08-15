module Main (main) where
import Data.Time
import Control.Arrow
import Data.Maybe
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import qualified Data.Vector as V

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/23_1/input_test.txt"
  writeFile ("src/23_1/output" ++ show utcNow ++ ".txt") . show . solve . parse $ contents

parse :: String -> Matrix Char
parse = lines >>> M.fromLists

-- solve :: Matrix Char -> Int
solve m = bt [] [startPos]
  where
    startPos :: (Int, Int)
    startPos = M.getRow 1 >>> V.elemIndex '.' >>> fromJust >>> const 1 &&& (+1) $ m

    bt _ [x] = snd x



