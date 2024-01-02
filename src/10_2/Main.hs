module Main (main) where
import Data.List.Split (splitOn, chunksOf)
import Data.List (zip5,delete,elemIndex,find,findIndex,maximumBy)
import GHC.Plugins (fstOf3)
import Debug.Trace (trace)
import Data.Ord (comparing)
import Data.Foldable (toList)
import Data.Graph ( Graph, Tree, Edge, Vertex, vertices, buildG, Bounds, scc )
-- import Data.Graph.Visualise (plotUGraphPng )

main :: IO ()
main = do
  contents <- readFile "src/10_2/input.txt"
  print . parse $ contents

-- mapM_ print .
type CharVertex = (Char, Vertex)
-- (original, up, right, down, left)
type InputUnit = (CharVertex, Maybe CharVertex, Maybe CharVertex, Maybe CharVertex, Maybe CharVertex)

findVertices :: [InputUnit] -> [Int]
findVertices = toList . maximumBy (comparing length) . scc . createGraph

createGraph :: [InputUnit] -> Graph
createGraph parsed = buildG (1, length parsed - 1) (concatMap getEdges parsed)

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
parse :: String -> Int
parse contents = sum . map (fstOf3 . foldl (folderfn vertices) (0,0,'?')) . chunksOf n $ zipped
  where
    Just n = elemIndex '\n' contents
    stream = zip (removeItem '\n' contents) [1..]
    maybeStream = map Just stream
    up =  replicate n Nothing ++ maybeStream
    right = replaceNth (n-1) Nothing (drop 1 maybeStream) ++ [Nothing]
    down = drop n maybeStream ++ replicate n Nothing
    left = Nothing : replaceNth (n-1) Nothing maybeStream
    zipped = zip5 stream up right down left
    vertices = findVertices zipped

folderfn :: [Int] -> (Int,Int,Char) -> InputUnit -> (Int,Int,Char) 
folderfn vertices (total,wallscount,ch) unit@((chnext,rank),_,_,_,_) = case find (==rank) vertices of 
  Just _ -> case (if chnext == 'S' then getSChar unit else chnext) of 
    '|' -> (total, wallscount + 1, chnext)
    'F' -> (total, wallscount, chnext)
    'J' -> case ch of
      'F' -> (total, wallscount + 1, chnext)
      _ -> (total, wallscount, chnext)
    'L' -> (total, wallscount, chnext)
    '7' -> case ch of 
      'L' -> (total, wallscount + 1, chnext)
      _ -> (total, wallscount, chnext)
    '-' -> (total, wallscount, ch)
    _ -> (total, wallscount, chnext)
  _ -> if wallscount `mod` 2 == 1 then (total+1,wallscount,chnext) else (total,wallscount,chnext)

getSChar (_,up,right,down,left)
  | isUp && isDown = '|'
  | isUp && isLeft = 'J'
  | isUp && isRight = 'L'
  | isDown && isRight = 'F'
  | isDown && isLeft = '7'
  | isRight && isLeft = '-'
  | otherwise = '.'

  where 
    isUp = case up of 
      Just ('|',_) -> True
      Just ('7',_) -> True
      Just ('F',_) -> True
      _ -> False
    isDown = case up of 
      Just ('|',_) -> True
      Just ('J',_) -> True
      Just ('L',_) -> True
      _ -> False
    isRight = case up of 
      Just ('-',_) -> True
      Just ('J',_) -> True
      Just ('7',_) -> True
      _ -> False
    isLeft = case up of 
      Just ('-',_) -> True
      Just ('F',_) -> True
      Just ('L',_) -> True
      _ -> False


replaceNth :: Int -> a -> [a] -> [a]
replaceNth n y xs = countdown n xs where
   countdown 0 xs = y:countdown n (drop 1 xs) -- reset to original n
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys