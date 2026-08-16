module Main (main) where
import Data.Time
import Control.Arrow
import Data.Maybe
import Data.Matrix (Matrix)
import Data.List (maximumBy)
import Data.Ord (comparing)

import Data.Graph.Types (Edge(..), tripleDestVertex, tripleAttribute, destinationVertex, (<->), adjacentVertices, adjacentVertices')
import Data.Graph.UGraph
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/23_2/input.txt"
  writeFile ("src/23_2/output" ++ show utcNow ++ ".txt") . show . result . solve . parse $ contents

type Vec2 = (Int, Int)

parse :: String -> Matrix Char
parse = lines >>> M.fromLists 

-- result :: [(Vec2, Vec2, Int)] -> Int
-- result = sum . map tripleAttribute
result = id

solve :: Matrix Char -> [(Vec2, Vec2, Int)]
solve m = bt graph2 (HS.singleton startPos) [startEdge]
-- solve m = graph2
  where
    startPos :: Vec2
    startPos = M.getRow 1 >>> V.elemIndex '.' >>> fromJust >>> const 1 &&& (+1) $ m

    startEdge :: (Vec2, Vec2, Int)
    startEdge = unsafeHead . adjacentVertices' graph2 $ startPos

    graph :: UGraph Vec2 ()
    graph = fromEdgesList (concat [getEdges (x,y) | x <- [1..M.nrows m], y <- [1..M.ncols m]])

    getEdges :: Vec2 -> [Edge Vec2 ()]
    getEdges pos@(x, y)
      | qualify pos = [pos <-> adj | adj <- fmap ($ pos) [first (+1), first (subtract 1), second (+1), second (subtract 1)], qualify adj]
      | otherwise = []

    graph2 :: UGraph Vec2 Int
    graph2 = tr HS.empty [[startPos]] []

    -- FIXME: add EDGES to set and correctly figure out the graph2
    tr :: HashSet Vec2 -> [[Vec2]] -> [Edge Vec2 Int] -> UGraph Vec2 Int
    tr _ [] edges = fromEdgesList edges
    tr visited (path:branches) edges = case adjs of
      [] -> tr visited_new branches (Edge cur curLast (length path - 1):edges)
      [adj] -> tr visited_new ((adj:path) : branches) edges
      _ -> tr visited_new (map (:path) adjs ++ branches) (Edge cur curLast (length path - 1):edges)
      where
        cur :: Vec2
        cur = unsafeHead path

        curLast :: Vec2
        curLast = unsafeLast path

        visited_new :: HashSet Vec2
        visited_new = HS.insert cur visited

        adjs :: [Vec2]
        adjs = filter (not . flip HS.member visited) . filterElem path . adjacentVertices graph $ cur

    qualify :: Vec2 -> Bool
    qualify = get >>> fmap (`elem` ".<>v^") >>> fromMaybe False

    get :: Vec2 -> Maybe Char
    get (x, y) = M.safeGet x y m

    bt :: UGraph Vec2 Int -> HashSet Vec2 -> [(Vec2, Vec2, Int)] -> [(Vec2, Vec2, Int)]
    bt graph visited path
      | fst cur == M.nrows m = path
      | null candidates = []
      | otherwise = fmap (\edge -> bt graph (HS.insert (tripleDestVertex edge) visited) (edge:path)) >>> maximumBy (comparing result) $ candidates
      where
        cur :: Vec2
        cur = tripleDestVertex . unsafeHead $ path

        candidates :: [(Vec2, Vec2, Int)]
        candidates = filter (not . flip HS.member visited . tripleDestVertex) . adjacentVertices' graph $ cur

unsafeHead :: [a] -> a
unsafeHead (x:_) = x
unsafeHead []    = error "Trust me bro, it's not empty"

unsafeLast = unsafeHead . reverse

filterElem bannedElems = filter (not . flip elem bannedElems)

