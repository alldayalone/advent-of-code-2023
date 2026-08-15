module Main (main) where
import Data.Time
import Control.Arrow
import Data.Maybe
import Data.Matrix (Matrix)
import Data.List (maximumBy)
import Data.Ord (comparing)

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
solve m = bt [startPos]
  where
    startPos :: (Int, Int)
    startPos = M.getRow 1 >>> V.elemIndex '.' >>> fromJust >>> const 1 &&& (+1) $ m

    bt :: [(Int, Int)] -> [(Int, Int)]
    bt path
      | fst cur == M.nrows m = path
      | null candidates = []
      | otherwise = fmap ((:path) >>> bt) >>> maximumBy (comparing length) $ candidates
      where
        cur :: (Int, Int)
        cur = unsafeHead path

        candidates :: [(Int, Int)]
        candidates = filter (not . flip elem path) . filter (get >>> \x -> isJust x && fromJust x `elem` ".^<>v") . map (\f -> f cur) $ [first (+1), first (subtract 1), second (+1), second (subtract 1)]

        get :: (Int, Int) -> Maybe Char
        get (x, y) = M.safeGet x y m

unsafeHead :: [a] -> a
unsafeHead (x:_) = x
unsafeHead []    = error "Trust me bro, it's not empty"
