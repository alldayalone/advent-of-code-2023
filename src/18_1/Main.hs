{-# LANGUAGE TupleSections #-}

module Main (main) where
import Data.Matrix as Matrix (Matrix (nrows, ncols), (!), fromLists, extendTo, setElem)
import Data.PSQueue as PSQ (PSQ(..), empty, fromList, size, lookup, insert, minView, null, adjust)
import Data.PSQueue.Internal (Binding(..))

main :: IO ()
main = do
  contents <- readFile "src/18_1/input_test.txt"
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

buildMatrix :: [Instruction] -> Matrix String
buildMatrix = fst . foldl applyInstruction (Matrix.fromLists [["#"]], (1, 1))

applyInstruction :: (Matrix String, (Int, Int)) -> Instruction -> (Matrix String, (Int, Int))
applyInstruction (m, (x, y)) (direction, number, color) = (m'', pos')
  where
    m' = uncurry (Matrix.extendTo ".") pos' m
    m'' = foldr (Matrix.setElem "#") m' (posrange (x,y) pos')
    pos' = case direction of
      "R" -> (x, y + number)
      "L" -> (x, y - number)
      "U" -> (x - number, y)
      "D" -> (x + number, y)
      _ -> error "Wrong direction"

solve :: Matrix String -> Int
solve m = bfs m q v
  where
    q = PSQ.fromList . map (\(x, y) -> (x * Matrix.nrows m + y) :-> (x, y)) . filter (\pos -> m Matrix.! pos == ".") $ perimeter (1, 1) (Matrix.nrows m, Matrix.ncols m)
    v = PSQ.empty

bfs :: Matrix String -> PSQ Int (Int, Int) -> PSQ Int (Int, Int) -> Int
bfs m q v 
  | PSQ.null q = Matrix.ncols m * Matrix.nrows m - PSQ.size v
  | otherwise = bfs m q'' v'
 where
   (Just (_ :-> pos, q')) = PSQ.minView q
   q'' = q'-- push all not visited "." neighbours of pos
   v' = v -- mark pos visited


-- Utils
posrange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
posrange (x1, y1) (x2, y2)
  | x1 > x2 = map (,y1) [x1-1,x1-2..x2]
  | x1 < x2 = map (,y1) [x1+1,x1+2..x2]
  | y1 < y2 = map (x1,) [y1+1,y1+2..y2]
  | y1 > y2 = map (x1,) [y1-1,y1-2..y2]

perimeter :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
perimeter (x1, y1) (x2, y2) = concat $ zipWith posrange pts (rotate 1 pts)
  where 
    pts = [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

-- Amazing *v* https://stackoverflow.com/a/55743500
rotate :: Int -> [a] -> [a]
rotate  =  drop <> take
