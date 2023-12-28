module Main (main) where
import Data.List.Split (splitOn)
import Data.List (zip5,delete,elemIndex,findIndex)
import GHC.Plugins (fstOf3)
import Debug.Trace (trace)
import Data.Graph ( Graph, Edge, Vertex, buildG, Bounds, scc )
-- import Data.Graph.Visualise (plotUGraphPng )

main :: IO ()
main = do
  contents <- readFile "src/10_1/input.txt"
  print . (`div` 2) . maximum . map length . scc . createGraph . concatMap getEdges . parse $ contents

-- mapM_ print .
type CharVertex = (Char, Vertex)
-- (original, up, right, down, left)
type InputUnit = (CharVertex, Maybe CharVertex, Maybe CharVertex, Maybe CharVertex, Maybe CharVertex)

createGraph :: [Edge] -> Graph
createGraph edges = buildG (getBounds edges) edges

getBounds :: [Edge] -> Bounds
getBounds [] = error "Empty edges"
getBounds [(x,y)] = (min x y, max x y)
getBounds ((x,y):xs) = (minimum [x,y,fst b], maximum [x,y,snd b])
  where b = getBounds xs

getEdges :: InputUnit -> [Edge]
getEdges input = case input of
  (('.',_),_,_,_,_) -> []
  (('|',v1),up,_,down,_) -> upEdges v1 up ++ downEdges v1 down
  (('-',v1),_,right,_,left) -> rightEdges v1 right ++ leftEdges v1 left
  (('L',v1),up,right,_,_) -> upEdges v1 up ++ rightEdges v1 right
  (('J',v1),up,_,_,left) -> upEdges v1 up ++ leftEdges v1 left
  (('7',v1),_,_,down,left) -> downEdges v1 down ++ leftEdges v1 left
  (('F',v1),_,right,down,_) -> rightEdges v1 right ++ downEdges v1 down
  (('S',v1),up,right,down,left) -> upEdges v1 up ++ rightEdges v1 right ++ downEdges v1 down ++ leftEdges v1 left

upEdges v1 up = case up of
  Just ('|', v2) -> [(v1,v2)]
  Just ('7', v2) -> [(v1,v2)]
  Just ('F', v2) -> [(v1,v2)]
  Just ('S', v2) -> [(v1,v2)]
  _ -> []

downEdges v1 down = case down of
  Just ('|', v2) -> [(v1,v2)]
  Just ('J', v2) -> [(v1,v2)]
  Just ('L', v2) -> [(v1,v2)]
  Just ('S', v2) -> [(v1,v2)]
  _ -> []

rightEdges v1 right = case right of
  Just ('-', v2) -> [(v1,v2)]
  Just ('7', v2) -> [(v1,v2)]
  Just ('J', v2) -> [(v1,v2)]
  Just ('S', v2) -> [(v1,v2)]
  _ -> []

leftEdges v1 left =  case left of
  Just ('-', v2) -> [(v1,v2)]
  Just ('F', v2) -> [(v1,v2)]
  Just ('L', v2) -> [(v1,v2)]
  Just ('S', v2) -> [(v1,v2)]
  _ -> []

-- zip5 with shifted lists to get neighbours
parse :: String -> [InputUnit]
parse contents = zip5 stream up right down left
  where
    Just n = elemIndex '\n' contents
    stream = zip (removeItem '\n' contents) [1..]
    maybeStream = map Just stream
    up =  replicate n Nothing ++ maybeStream
    right = replaceNth (n-1) Nothing (drop 1 maybeStream) ++ [Nothing]
    down = drop n maybeStream ++ replicate n Nothing
    left = Nothing : replaceNth (n-1) Nothing maybeStream

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n y xs = countdown n xs where
   countdown 0 xs = y:countdown n (drop 1 xs) -- reset to original n
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys