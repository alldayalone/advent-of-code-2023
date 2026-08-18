module Main (main) where
import Data.Time
import Control.Arrow
import Data.Maybe
import Data.Matrix (Matrix)
import Data.List (maximumBy)
import Data.Ord (comparing)

import Data.Graph.Types (Edge(..), tripleDestVertex, fromTriple, tripleOriginVertex, tripleAttribute, originVertex, destinationVertex, (<->), edgePairs, adjacentVertices, adjacentVertices')
import Data.Graph.UGraph
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.Bits (xor)

instance (Hashable v, Hashable e) => Hashable (Edge v e) where
  hashWithSalt salt (Edge u v attr) =
    -- Combine vertex hashes using addition or XOR so order doesn't matter
    let combinedVerticesHash = hash u `xor` hash v
    in salt `hashWithSalt` combinedVerticesHash `hashWithSalt` attr

main :: IO ()
main = do
  utcNow   <- getZonedTime
  contents <- readFile "src/23_2/input.txt"
  writeFile ("src/23_2/output" ++ show utcNow ++ ".txt") . show . result . solve . parse $ contents

type Vec2 = (Int, Int)

parse :: String -> Matrix Char
parse = lines >>> M.fromLists 

result :: [(Vec2, Vec2, Int)] -> Int
result = sum . map tripleAttribute
-- result = id

solve :: Matrix Char -> [(Vec2, Vec2, Int)]
solve m = bt graph2 (HS.singleton startPos) [startEdge graph2]
-- solve m = graph2
  where
    startPos :: Vec2
    startPos = M.getRow 1 >>> V.elemIndex '.' >>> fromJust >>> const 1 &&& (+1) $ m

    -- startEdge :: (Vec2, Vec2, Int)
    startEdge g = unsafeHead . adjacentVertices' g $ startPos

    graph :: UGraph Vec2 ()
    graph = fromEdgesList (concat [getEdges (x,y) | x <- [1..M.nrows m], y <- [1..M.ncols m]])

    getEdges :: Vec2 -> [Edge Vec2 ()]
    getEdges pos@(x, y)
      | qualify pos = [pos <-> adj | adj <- fmap ($ pos) [first (+1), first (subtract 1), second (+1), second (subtract 1)], qualify adj]
      | otherwise = []

    graph2 :: UGraph Vec2 Int
    graph2 = tr HS.empty [fromTriple (startEdge graph)] HS.empty

    trace :: [Edge Vec2 ()] -> ([Edge Vec2 ()], [Edge Vec2 ()])
    trace path@(last:_) = case adjs of
      [] -> (path, adjs)
      [adj] -> trace (adj:path)
      _ -> (path, adjs)
      where
        adjs :: [Edge Vec2 ()]
        adjs = reject (==last) . incidentEdges graph $ destinationVertex last


    -- tr :: HashSet (Edge Vec2 ()) -> [Edge Vec2 ()] -> HashSet (Edge Vec2 Int) -> UGraph Vec2 Int
    tr _ [] edges = fromEdgesList (HS.toList edges)
    -- tr _ [] edges = edges
    tr visited (next:queue) edges = tr visited_new (queue ++ HS.toList queue_push) edges_new
      where
        (path, adjs) = trace [next]

        visited_new = HS.union visited (HS.fromList path)

        queue_push = HS.difference (HS.fromList adjs) visited_new

        edges_new = HS.insert (fromPath path) edges

    fromPath :: [Edge Vec2 ()] -> Edge Vec2 Int
    fromPath p = Edge (originVertex . unsafeLast $ p) (destinationVertex . unsafeHead $ p) (length p)
    

    qualify :: Vec2 -> Bool
    qualify = get >>> fmap (`elem` ".<>v^") >>> fromMaybe False

    get :: Vec2 -> Maybe Char
    get (x, y) = M.safeGet x y m

    bt :: UGraph Vec2 Int -> HashSet Vec2 -> [(Vec2, Vec2, Int)] -> [(Vec2, Vec2, Int)]
    bt graph visited path
      | fst cur == M.nrows m = path
      | null candidates = []
      | otherwise = fmap (\edge -> bt graph (HS.insert (tripleOriginVertex edge) visited) (edge:path)) >>> maximumBy (comparing result) $ candidates
      where
        cur :: Vec2
        cur = tripleDestVertex . unsafeHead $ path

        -- visited_new = 

        candidates :: [(Vec2, Vec2, Int)]
        candidates = reject (flip HS.member visited . tripleDestVertex) . adjacentVertices' graph $ cur

unsafeHead :: [a] -> a
unsafeHead (x:_) = x
unsafeHead []    = error "Trust me bro, it's not empty"

unsafeLast = unsafeHead . reverse

filterElem bannedElems = filter (not . flip elem bannedElems)


reject :: (a -> Bool) -> [a] -> [a]
reject pred = filter (not . pred)