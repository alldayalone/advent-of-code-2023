{-# LANGUAGE TupleSections #-}

module Main (main) where
import Data.Matrix as Matrix (Matrix (nrows, ncols), (!), fromLists, extendTo, setElem, matrix, mapPos)
import Data.Matrix ((<|>), (<->))
import Data.Time
import Data.Maybe (isNothing, isJust)
import Data.PSQueue as PSQ (PSQ(..), empty, fromList, size, lookup, insert, minView, null, adjust)
import Data.PSQueue.Internal (Binding(..))
import Data.List (find)

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/18_1/input.txt"
  writeFile ("src/18_1/output" ++ show utcNow ++ ".txt") . show . solve . buildMatrix . parse $ contents

type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

parse :: String -> [Instruction]
parse = map parseLine . lines

parseLine :: String -> Instruction
parseLine line = (direction, read number, color)
  where
    [direction, number, color] = words line

buildMatrix :: [Instruction] -> Matrix String
buildMatrix = fst . foldl applyInstruction (Matrix.fromLists [["#"]], (1, 1))

applyInstruction :: (Matrix String, (Int, Int)) -> Instruction -> (Matrix String, (Int, Int))
applyInstruction (m, (x, y)) (direction, number, color) = (m'', last range)
  where
    m' = uncurry (safeExtendTo ".") pos' m
    range = safeposrange (x,y) pos'
    m'' = foldr (Matrix.setElem "#") m' range
    pos' = case direction of
      "R" -> (x, y + number)
      "L" -> (x, y - number)
      "U" -> (x - number, y)
      "D" -> (x + number, y)
      _ -> error "Wrong direction"

safeExtendTo :: a -> Int -> Int -> Matrix a -> Matrix a
safeExtendTo a x y m
  | x < 1 = safeExtendTo a 1 y (upBlock <-> m)
  | y < 1 = safeExtendTo a x 1 (leftBlock <|> m)
  | otherwise = Matrix.extendTo a x y m
  where
    upBlock = Matrix.matrix (1 - x) (Matrix.ncols m) (const a)
    leftBlock = Matrix.matrix (Matrix.nrows m) (1 - y) (const a)

solve :: Matrix String -> Int
solve m = bfs m q v
  where
    q = PSQ.fromList . map (posToBinding m) . filter (\pos -> m Matrix.! pos == ".") $ perimeter (1, 1) (Matrix.nrows m, Matrix.ncols m)
    v = PSQ.empty

bfs :: Matrix String -> PSQ Int (Int, Int) -> PSQ Int (Int, Int) -> Int
bfs m q v 
  | PSQ.null q = Matrix.ncols m * Matrix.nrows m - PSQ.size v
  | otherwise = bfs m q'' v'
 where
   (Just (_ :-> pos, q')) = PSQ.minView q
   newNbrs = map (posToBinding m) . filter (isNotVisited m v) . filter (isDot m) . filter (validBounds m) $ posneighbours pos
   q'' = foldr insertBinding q' newNbrs
   v' = foldr insertBinding v (posToBinding m pos:newNbrs)

insertBinding :: Binding Int (Int, Int) -> PSQ Int (Int, Int) -> PSQ Int (Int, Int)
insertBinding (k :-> p) = PSQ.insert k p

posToBinding :: Matrix String -> (Int, Int) -> Binding Int (Int, Int)
posToBinding m pos = posToKey m pos :-> pos

posToKey :: Matrix String -> (Int, Int) -> Int
posToKey m (x, y) = x * Matrix.ncols m + y

isDot :: Matrix String -> (Int, Int) -> Bool
isDot m pos = m Matrix.! pos == "."

isNotVisited :: Matrix String -> PSQ Int (Int, Int) -> (Int, Int) -> Bool
isNotVisited m v pos = isNothing $ PSQ.lookup (posToKey m pos) v

isVisited :: Matrix String -> PSQ Int (Int, Int) -> (Int, Int) -> Bool
isVisited m v pos = isJust $ PSQ.lookup (posToKey m pos) v

validBounds :: Matrix a -> (Int, Int) -> Bool
validBounds m (x, y) = x > 0 && y > 0 && x <= Matrix.nrows m && y <= Matrix.ncols m

-- Utils
posrange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
posrange (x1, y1) (x2, y2)
  | x1 > x2 = map (,y1) [x1-1,x1-2..x2]
  | x1 < x2 = map (,y1) [x1+1,x1+2..x2]
  | y1 < y2 = map (x1,) [y1+1,y1+2..y2]
  | y1 > y2 = map (x1,) [y1-1,y1-2..y2]


safeposrange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
safeposrange (x1, y1) (x2, y2)
  | x2 < 1 = safeposrange (x1 + 1 -x2, y1) (1, y2)
  | y2 < 1 = safeposrange (x1, y1 + 1 - y2) (x2, 1)
  | otherwise = posrange (x1, y1) (x2, y2)

posneighbours :: (Int, Int) -> [(Int, Int)]
posneighbours  (x, y) = [up, down, left, right]
  where
    up = (x+1, y)
    down = (x-1, y)
    left = (x, y-1)
    right = (x, y+1)

perimeter :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
perimeter (x1, y1) (x2, y2) = concat $ zipWith posrange pts (rotate 1 pts)
  where 
    pts = [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

-- Amazing *v* https://stackoverflow.com/a/55743500
rotate :: Int -> [a] -> [a]
rotate  =  drop <> take

