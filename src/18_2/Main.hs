{-# LANGUAGE TupleSections #-}

module Main (main) where
import Data.Matrix as Matrix (Matrix (nrows, ncols), (!), fromLists, extendTo, setElem, matrix, mapPos, fromList)
import Data.Matrix ((<|>), (<->), fromList)
import Data.Time
import Data.Vector as Vector (Vector)
import Data.Maybe (isNothing, isJust)
import Data.PSQueue as PSQ (PSQ(..), empty, fromList, size, lookup, insert, minView, null, adjust)
import Data.PSQueue.Internal (Binding(..))
import Data.List (find, nub, sort)
import Data.Char (isDigit)
import Data.Tuple.Extra (thd3)

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "./src/18_2/input_test.txt"
  print . buildMatrix . parse $ contents




-- R 6 (#70c710)
-- D 5 (#0dc571)
-- L 2 (#5713f0)
-- D 2 (#d2c081)












type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

parse :: String -> [Instruction]
parse = map parseLine . lines

parseLine :: String -> Instruction
parseLine line = (direction, read number, color)
  where
    [direction, number, color] = words line

buildMatrix :: [Instruction] -> Matrix (Int, Int, Int)
buildMatrix instructions = grid
  where
    chain = fst $ foldl applyInstruction ([], (1, 1)) instructions
    coords = map snd chain
    xs = sort . nub . map fst $ coords
    ys = sort . nub . map snd $ coords
    grid = Matrix.fromList (length xs) (length ys) [(x, y, if isJust (find (== (x,y)) coords) then 1 else 0) | x <- xs, y <- ys]
    -- matrix = foldl f 


buildCoords :: Foldable t => t Instruction -> [(Int, Int)]
buildCoords instructions = coords
  where
    chain = fst $ foldl applyInstruction ([], (1, 1)) instructions
    coords = map snd chain
    xs = sort . nub . map fst $ coords
    ys = sort . nub . map snd $ coords
    grid = Matrix.fromList (length xs) (length ys) [(x, y, if isJust (find (== (x,y)) coords) then 1 else 0) | x <- xs, y <- ys]

    -- matrix = foldl f 


findIndexInMatrix :: (a -> Bool) -> Matrix a -> Maybe (Int, Int)
findIndexInMatrix p m =
    let (rows, cols) = dim m
    in findFirstMatch p m rows cols

dim :: Matrix a -> (Int, Int)
dim m = (nrows m, ncols m)

findFirstMatch :: (a -> Bool) -> Matrix a -> Int -> Int -> Maybe (Int, Int)
findFirstMatch p m rows cols =
    let allIndices = [(r, c) | r <- [1..rows], c <- [1..cols]]
    in find (\(r, c) -> p (m ! (r, c))) allIndices

applyInstruction :: ([(Direction, (Int, Int))], (Int, Int)) -> Instruction -> ([(Direction, (Int, Int))], (Int, Int))
applyInstruction (v, pos@(x,y)) instruction = (v', pos')
  where
    (direction, number, _) = instruction
    -- (direction, number) = parseColor . thd3 $ instruction
    v' = v ++ [(direction, pos')]
    pos' = case direction of
      "R" -> (x, y + number)
      "L" -> (x, y - number)
      "U" -> (x - number, y)
      "D" -> (x + number, y)
      _ -> error "Wrong direction"

parseColor :: Color -> (Direction, Int)
parseColor ['(','#',s1,s2,s3,s4,s5,s6,')'] = (hexToDir s6, parseHexString [s1,s2,s3,s4,s5])
parseColor _ = error "Wrong input"

parseHexString :: String -> Int
parseHexString = fst . foldr (f . parseHexChar) (0,1)
  where
    f x (accSum, accFactor) = (accSum + x * accFactor, accFactor * 16)

parseHexChar :: Char -> Int
parseHexChar c
  | isDigit c = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise = error $ "Error: " ++ [c]


hexToDir :: Char -> Direction
hexToDir '0' = "R"
hexToDir '1' = "D"
hexToDir '2' = "L"
hexToDir '3' = "U"
hexToDir _ = error "Wrong direction"

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
