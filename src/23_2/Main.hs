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
  contents <- readFile "src/23_2/input_test.txt"
  writeFile ("src/23_2/output" ++ show utcNow ++ ".txt") . show . result . solve . parse $ contents

parse :: String -> Matrix Char
parse = lines >>> M.fromLists

result = length >>> subtract 1

solve :: Matrix Char -> [(Int, Int)]
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

        posList = [first (+1), first (subtract 1), second (+1), second (subtract 1)]
        charList = [flip elem ".<>v", flip elem ".<>^", flip elem ".>^v", flip elem ".<v^"]
        requirements = zip posList charList

        candidates :: [(Int, Int)]
        candidates = filter (not . flip elem path) .  map (\(f,_) -> f cur) . filter qualify $ requirements

        qualify :: ((Int, Int) -> (Int, Int), Char -> Bool) -> Bool
        qualify (f, _) = f >>> get >>> fmap (`elem` ".<>v^") >>> fromMaybe False $ cur

        get :: (Int, Int) -> Maybe Char
        get (x, y) = M.safeGet x y m

unsafeHead :: [a] -> a
unsafeHead (x:_) = x
unsafeHead []    = error "Trust me bro, it's not empty"
