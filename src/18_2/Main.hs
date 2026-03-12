{-# LANGUAGE TupleSections #-} 
{-# LANGUAGE MultilineStrings #-} 

module Main (main, walk, walk', extendGrid,extendMatrix) where
import Data.Matrix as Matrix (Matrix (nrows, ncols), (!), fromLists, toList, toLists, extendTo, setElem, matrix, mapPos, fromList, transpose)
import Data.Matrix ((<|>), (<->), fromList)
import Data.Vector as Vector (Vector, fromList, (!))
import Data.Time
import Data.Maybe (isNothing, isJust)
import Data.PSQueue as PSQ (PSQ(..), empty, fromList, size, lookup, insert, minView, null, adjust)
import Data.PSQueue.Internal (Binding(..))
import Data.List (find, nub, sort)
import Data.Char (isDigit)
import Data.Tuple.Extra (thd3)


import Data.Bifunctor (first, bimap)

parseInstruction = parseInstructionV2
file = "./src/18_2/input.txt"

walk :: ([Int], Int) -> Int -> ([Int], Int)
walk ([], pos) steps = error "Empty array"
walk (arr, pos) steps
  | steps < 0 = reversePair (walk (reversePair (arr, pos)) (-steps))
  | steps == 0 = (arr, pos)
  | arr == [1] && steps == 1 = (arr ++ [1], pos + 1)
  | arr == [1] && steps > 1 = (arr ++ [steps - 1, 1], pos + 2)
  | y == 1 = first ([x] ++) (walk (drop 1 arr, pos + 1) (steps - 1))
  | steps >= x+y = first ([x, y] ++) (walk (drop 2 arr, pos + 2) (steps - (x+y)))
  | steps == 1 = ([1,1,y-1] ++ drop 2 arr, pos + 1)
  | steps == y = ([1,y-1,1] ++ drop 2 arr, pos + 2)
  | steps > 1 && steps < y = ([1,steps-1,1,y-steps] ++ drop 2 arr, pos + 2)
  | otherwise = error "Impossible state"
    where
      [x,y] = take 2 arr
      reversePair (arr, pos) = (reverse arr, length arr - pos + 1)

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile file
  print . solve . buildMatrix . parse $ contents

type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

parse :: String -> [Instruction]
parse = map parseLine . lines

parseLine :: String -> Instruction
parseLine line = (direction, read number, color)
  where
    [direction, number, color] = words line

type Position = (Int, Int)
type Context = (Position, [Int], [Int], Matrix String, [(Int, Int)])

buildMatrix :: [Instruction] -> Context
buildMatrix = foldl applyInstruction ((1, 1), [1], [1], Matrix.fromList 1 1 ["#"], [])

extendGrid :: ([a], [Int], [Int], a) -> [a]
extendGrid ([], [], [], _) = []
extendGrid ([], [], (z:zs), filler) = [filler] ++ extendGrid ([], [], zs, filler)
extendGrid ((x:xs), (y:ys), (z:zs), filler)
  | z < y = [x] ++ extendGrid ((x:xs), (y-z:ys), zs, filler)
  | otherwise = [x] ++ extendGrid (xs,ys,zs,filler)

extendMatrix :: Matrix String -> [Int] -> [Int] -> Matrix String
extendMatrix m a a' = Matrix.fromLists (extendGrid (lists, a, a', replicate (Matrix.ncols m) "."))
  where 
    lists = Matrix.toLists m

-- Launches walk from a specified position (walk assumes it's always first element)
walk' :: ([Int], Int) -> Int -> ([Int], Int)
walk' (arr, pos) steps
  | steps > 0 = first ((take (pos-1) arr) ++) (walk (drop (pos-1) arr, pos) steps)
  | steps == 0 = (arr, pos)
  | steps < 0 = first (++(drop pos arr)) (walk (take pos arr, pos) steps)
  | otherwise = error "Impossible state"

applyInstruction :: Context -> Instruction -> Context
applyInstruction (pos, cols, rows, m, _) instruction = (pos', cols', rows', m'', range)
  where
    -- TODO - we are here, figure out correct transformation
    (x,y) = pos
    (direction, number, _) = parseInstruction instruction
    (cols', x') = case direction of
      "R" -> walk' (cols, x) number
      "L" -> walk' (cols, x) (-number)
      "U" -> (cols, x)
      "D" -> (cols, x)
      _ -> error "Wrong direction"
    (rows', y') = case direction of 
      "R" -> (rows, y)
      "L" -> (rows, y)
      "U" -> walk' (rows, y) (-number)
      "D" -> walk' (rows, y) number
      _ -> error "Wrong direction"
    pos' = (x', y')
    m' = case direction of
      "R" -> Matrix.transpose (extendMatrix (Matrix.transpose m) cols cols')
      "L" -> Matrix.transpose (extendMatrix (Matrix.transpose m) cols cols')
      "U" -> extendMatrix m rows rows'
      "D" -> extendMatrix m rows rows'
      _ -> error "Wrong direction"
      
      -- uncurry (safeExtendTo ".") (length rows', length cols') m
    delta_x = length cols' - length cols
    delta_y = length rows' - length rows
    -- range = [(x', y'), (x, y), (delta_x, delta_y)]
    range = case direction of
      "R" -> safeposrange pos pos'
      "L" -> safeposrange (x + delta_x, y) pos' 
      "U" -> safeposrange (x, y + delta_y) pos'
      "D" -> safeposrange pos pos'
      _ -> error "Wrong direction"
    m'' = foldr (Matrix.setElem "#" . fliP) m' range
  
fliP (a,b) = (b,a)

parseInstructionV1 :: Instruction -> Instruction
parseInstructionV1 = id

parseInstructionV2 :: Instruction -> Instruction
parseInstructionV2 (_, _, color) = (direction, number, color)
  where
    ['(','#',s1,s2,s3,s4,s5,s6,')'] = color
    number = parseHexString [s1,s2,s3,s4,s5]
    direction = case s6 of
      '0' -> "R"
      '1' -> "D"
      '2' -> "L"
      '3' -> "U"
      _   -> error "Wrong direction"
    

findIndexInMatrix :: (a -> Bool) -> Matrix a -> Maybe (Int, Int)
findIndexInMatrix p m =
    let (rows, cols) = dim m
    in findFirstMatch p m rows cols

dim :: Matrix a -> (Int, Int)
dim m = (nrows m, ncols m)

findFirstMatch :: (a -> Bool) -> Matrix a -> Int -> Int -> Maybe (Int, Int)
findFirstMatch p m rows cols =
    let allIndices = [(r, c) | r <- [1..rows], c <- [1..cols]]
    in find (\(r, c) -> p (m Matrix.! (r, c))) allIndices

parseHexString :: String -> Int
parseHexString = fst . foldr (f . parseHexChar) (0,1)
  where
    f x (accSum, accFactor) = (accSum + x * accFactor, accFactor * 16)

parseHexChar :: Char -> Int
parseHexChar c
  | isDigit c = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise = error $ "Error: " Prelude.++ [c]

safeExtendTo :: a -> Int -> Int -> Matrix a -> Matrix a
safeExtendTo a x y m
  | x < 1 = safeExtendTo a 1 y (upBlock <-> m)
  | y < 1 = safeExtendTo a x 1 (leftBlock <|> m)
  | otherwise = Matrix.extendTo a x y m
  where
    upBlock = Matrix.matrix (1 - x) (Matrix.ncols m) (const a)
    leftBlock = Matrix.matrix (Matrix.nrows m) (1 - y) (const a)

solve :: Context -> Int
solve c = sum $ Matrix.toList values_m
  where
    (_,cols,rows, m, _) = c
    cols_v = Vector.fromList cols
    rows_v = Vector.fromList rows
    values_m = Matrix.mapPos (\(x,y) _ -> if isVisited m v (x,y) then 0 else cols_v Vector.! (y - 1) * rows_v Vector.! (x - 1)) m
    q = PSQ.fromList . map (posToBinding m) . filter (\pos -> m Matrix.! pos == ".") $ perimeter (1, 1) (Matrix.nrows m, Matrix.ncols m)
    v = bfs m q PSQ.empty

bfs :: Matrix String -> PSQ Int (Int, Int) -> PSQ Int (Int, Int) -> PSQ Int (Int, Int)
bfs m q v
  | PSQ.null q = v
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
