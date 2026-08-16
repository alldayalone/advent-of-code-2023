module Main (main) where
import Data.Time
import Control.Arrow
import Data.Maybe
import Data.Matrix (Matrix)
import Data.List (maximumBy)
import Data.Ord (comparing)

import Data.Graph.Types ((<->), adjacentVertices)
import Data.Graph (Edge)
import Data.Graph.UGraph
import qualified Data.Matrix as M
import qualified Data.Vector as V

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/23_2/input.txt"
  writeFile ("src/23_2/output" ++ show utcNow ++ ".txt") . show . result . solve . parse $ contents

type Vec2 = (Int, Int)

parse :: String -> Matrix Char
parse = lines >>> M.fromLists 

result :: [Vec2] -> Int
result = length >>> subtract 1
-- result = prettyPrint

solve :: Matrix Char -> [Vec2]
solve m = bt [startPos]
-- solve = graph
  where
    startPos :: Vec2
    startPos = M.getRow 1 >>> V.elemIndex '.' >>> fromJust >>> const 1 &&& (+1) $ m

    graph :: UGraph Vec2 ()
    graph = fromEdgesList (concat [getEdges (x,y) | x <- [1..M.nrows m], y <- [1..M.ncols m]])

    getEdges pos@(x, y)
      | qualify pos = [pos <-> adj | adj <- [(x + 1, y), (x, y + 1)], qualify adj]
      | otherwise = []

    qualify :: Vec2 -> Bool
    qualify = get >>> fmap (`elem` ".<>v^") >>> fromMaybe False

    get :: Vec2 -> Maybe Char
    get (x, y) = M.safeGet x y m

    bt :: [Vec2] -> [Vec2]
    bt path
      | fst cur == M.nrows m = path
      | null candidates = []
      | otherwise = fmap ((:path) >>> bt) >>> maximumBy (comparing length) $ candidates
      where
        cur :: Vec2
        cur = unsafeHead path

        candidates :: [Vec2]
        candidates = filter (not . flip elem path) . adjacentVertices graph $ cur

unsafeHead :: [a] -> a
unsafeHead (x:_) = x
unsafeHead []    = error "Trust me bro, it's not empty"
