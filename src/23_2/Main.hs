module Main (main) where
import Data.Time
import Control.Arrow
import Data.Maybe
import Data.Matrix (Matrix)
import Data.List (maximumBy)
import Data.Ord (comparing)

import Data.Graph.Types ((<->))
import Data.Graph (Edge)
import Data.Graph.UGraph
import qualified Data.Matrix as M
import qualified Data.Vector as V

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/23_2/input_test.txt"
  writeFile ("src/23_2/output" ++ show utcNow ++ ".txt") . show . result . solve . parse $ contents

type Vec2 = (Int, Int)

parse :: String -> Matrix Char
parse = lines >>> M.fromLists 

result :: [Vec2] -> Int
result = length >>> subtract 1

solve :: Matrix Char -> [Vec2]
solve m = bt [startPos]
  where
    startPos :: Vec2
    startPos = M.getRow 1 >>> V.elemIndex '.' >>> fromJust >>> const 1 &&& (+1) $ m

    graph :: UGraph Vec2 ()
    graph = fromEdgesList (concat [candidates (x, y) | x <- [1..M.nrows], y <- [1..M.ncols]])

    -- candidates :: Vec2 -> [Edge Vec2 ()]
    candidates cur = map (\f -> cur <-> f cur) . filter qualify $ [first (+1), second (+1)]
      where 
        qualify :: (Vec2 -> Vec2) -> Bool
        qualify f = f >>> get >>> fmap (`elem` ".<>v^") >>> fromMaybe False $ cur

    get :: Vec2 -> Maybe Char
    get (x, y) = M.safeGet x y m

    bt :: [(Int, Int)] -> [(Int, Int)]
    bt path
      | fst cur == M.nrows m = path
      | null candidates = []
      | otherwise = fmap ((:path) >>> bt) >>> maximumBy (comparing length) $ candidates
      where
        cur :: (Int, Int)
        cur = unsafeHead path

unsafeHead :: [a] -> a
unsafeHead (x:_) = x
unsafeHead []    = error "Trust me bro, it's not empty"
