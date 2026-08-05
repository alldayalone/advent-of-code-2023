{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Data.Matrix as Matrix
import Control.Arrow ((&&&), (***), (>>>), first, second)
import Data.Time
import Data.Maybe 

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/21_2/input.txt"
  writeFile ("src/21_2/output" ++ show utcNow ++ ".txt") . show . metasolve . parse $ contents


type Pos = (Int, Int)
type TimeToReach = Int
type Elem = (Pos, TimeToReach) 

data Cell = Wall | Unvisited | Only Int | Both Int Int 
  deriving (Eq)

instance Show Cell where
  show Wall = "#"
  show Unvisited = "."
  -- show (Only x) = show . flip mod 2 $ x
  show (Only x) = show x
  show (Both x y) = show y
type Field = Matrix Cell


-- metasolve :: Matrix Cell -> [Int]
-- metasolve field = [stepsPartialSide, stepsPartialMixedSide, n, n_half, n_half_index, m, r, nFullEven, countFullEven, nFullOdd, countFullOdd, nPartialSide, countS, countN, countE, countW, nPartialMixedSide, countSE,  countSW, countNE, countNW] -- nFullEven * countFullEven + nFullOdd * countFullOdd + nPartialSide * (countS + countN + countE + countW) + nPartialMixedSide * (countSE + countSW + countNE + countNW)
metasolve field = nFullEven * countFullEven + nFullOdd * countFullOdd + nPartialSide * (countS + countN + countE + countW) + nPartialSide2 * (countS2 + countN2 + countE2 + countW2) + nPartialMixedSide * (countSE + countSW + countNE + countNW) + nPartialMixedSide2 * (countSE2 + countSW2 + countNE2 + countNW2)
  where
    n = Matrix.ncols field -- размер матрицы
    n_half = n `div` 2
    n_half_index = n_half + 1
    m = nTotalSteps -- общее кол-во шагов
    r = (m + 1) `div` n -- "радиус" заполнения ромба
    nFullEven = (r - r `mod` 2) ^ 2 - nPartialSide2*4*(if even r then 1 else 0) -- кол-во четных заполненных полей (n x n)
    nFullOdd = (r - (r+1) `mod` 2) ^ 2 - nPartialSide2*4*(if odd r then 1 else 0) -- кол-во нечетных заполненных (например, центр)
    nPartialSide = 1 -- старт с (0, (n-1)/2) и тд
    nPartialMixedSide = max (r-1) 0 -- старт с (0, 0) и тд

    nPartialSide2 = 1
    nPartialMixedSide2 = max r 0

    solvedFieldCenter = solve n (Matrix.setElem (Only 0) (n_half_index, n_half_index) field)
    countFullOdd = countResults doesCountEven solvedFieldCenter
    countFullEven = countResults doesCountOdd solvedFieldCenter

    stepsPartialSide = m - n_half - (r-1)*n - 1
    stepsPartialMixedSide = m - 2*n_half - (r-2)*n - 2
    solvedFieldS = solve stepsPartialSide (Matrix.setElem (Only 0) (1, n_half_index) field)
    solvedFieldN = solve stepsPartialSide (Matrix.setElem (Only 0) (n, n_half_index) field)
    solvedFieldE = solve stepsPartialSide (Matrix.setElem (Only 0) (n_half_index, 1) field)
    solvedFieldW = solve stepsPartialSide (Matrix.setElem (Only 0) (n_half_index, n) field)

    solvedFieldSE = solve stepsPartialMixedSide (Matrix.setElem (Only 0) (1, 1) field)
    solvedFieldSW = solve stepsPartialMixedSide (Matrix.setElem (Only 0) (1, n) field)
    solvedFieldNE = solve stepsPartialMixedSide (Matrix.setElem (Only 0) (n, 1) field)
    solvedFieldNW = solve stepsPartialMixedSide (Matrix.setElem (Only 0) (n, n) field)

    doesCountSide = if odd r then doesCountEven else doesCountOdd
    countS = countResults doesCountSide solvedFieldS 
    countN = countResults doesCountSide solvedFieldN 
    countE = countResults doesCountSide solvedFieldE 
    countW = countResults doesCountSide solvedFieldW 

    doesCountMixedSide = if even r then doesCountEven else doesCountOdd
    countSE = countResults doesCountMixedSide solvedFieldSE 
    countSW = countResults doesCountMixedSide solvedFieldSW 
    countNE = countResults doesCountMixedSide solvedFieldNE 
    countNW = countResults doesCountMixedSide solvedFieldNW 

    -- Extra layer!!!!
    stepsPartialSide2 = m - n_half - (r-2)*n - 1
    stepsPartialMixedSide2 = m - 2*n_half - (r-1)*n - 2
    solvedFieldS2 = solve stepsPartialSide2 (Matrix.setElem (Only 0) (1, n_half_index) field)
    solvedFieldN2 = solve stepsPartialSide2 (Matrix.setElem (Only 0) (n, n_half_index) field)
    solvedFieldE2 = solve stepsPartialSide2 (Matrix.setElem (Only 0) (n_half_index, 1) field)
    solvedFieldW2 = solve stepsPartialSide2 (Matrix.setElem (Only 0) (n_half_index, n) field)

    solvedFieldSE2 = solve stepsPartialMixedSide2 (Matrix.setElem (Only 0) (1, 1) field)
    solvedFieldSW2 = solve stepsPartialMixedSide2 (Matrix.setElem (Only 0) (1, n) field)
    solvedFieldNE2 = solve stepsPartialMixedSide2 (Matrix.setElem (Only 0) (n, 1) field)
    solvedFieldNW2 = solve stepsPartialMixedSide2 (Matrix.setElem (Only 0) (n, n) field)

    doesCountSide2 = if even r then doesCountEven else doesCountOdd
    countS2 = countResults doesCountSide2 solvedFieldS2
    countN2 = countResults doesCountSide2 solvedFieldN2 
    countE2 = countResults doesCountSide2 solvedFieldE2 
    countW2 = countResults doesCountSide2 solvedFieldW2 

    doesCountMixedSide2 = if odd r then doesCountEven else doesCountOdd
    countSE2 = countResults doesCountMixedSide2 solvedFieldSE2 
    countSW2 = countResults doesCountMixedSide2 solvedFieldSW2 
    countNE2 = countResults doesCountMixedSide2 solvedFieldNE2 
    countNW2 = countResults doesCountMixedSide2 solvedFieldNW2


isWall Wall = True
isWall _ = False
isVisitable = not . isWall

doesCountEven :: Cell -> Bool
doesCountEven Wall = False
doesCountEven Unvisited = False
doesCountEven (Only x) = even (x + nTotalSteps)
doesCountEven (Both x y) = True

doesCountOdd :: Cell -> Bool
doesCountOdd Wall = False
doesCountOdd Unvisited = False
doesCountOdd (Only x) = odd (x + nTotalSteps)
doesCountOdd (Both x y) = True

nTotalSteps :: Int
nTotalSteps = 26501365

parse :: String -> Field
parse = lines >>> fmap (fmap parseChar) >>> Matrix.fromLists
  where 
    parseChar '#' = Wall
    parseChar _ = Unvisited

findAllPos :: Int -> Field -> [Elem]
findAllPos stepCount = fmap fromJust . filter isJust . Matrix.toList . Matrix.mapPos mapper
  where 
    mapper :: Pos -> Cell -> Maybe Elem
    mapper pos Wall = Nothing
    mapper pos Unvisited = Nothing
    mapper pos (Only x) = if isEntryPoint x then Just (pos, stepCount+1) else Nothing
    mapper pos (Both x y) = if any isEntryPoint [x, y] then Just (pos, stepCount+1) else Nothing

    isEntryPoint :: Int -> Bool
    isEntryPoint = (== stepCount)

countResults :: (Cell -> Bool) -> Field -> Int
countResults doesCount = length . filter id . fmap doesCount . Matrix.toList

solve :: Int -> Field -> Field
solve nSteps m = fst . (!! nSteps) . iterate step $ (m, 0)

step :: (Field, Int) -> (Field, Int)
step (m, stepCount) = (propagateAll m (findAllPos stepCount m), stepCount + 1)

propagateAll :: Field -> [Elem] -> Field
propagateAll = foldr propagate


propagate :: Elem -> Field -> Field
propagate (p1, ttr) m = Matrix.mapPos mapper m
  where 
    addition x y = x + y
    -- modcol = addition 1 . flip mod (Matrix.ncols m) . subtract 1
    -- modrow = addition 1 . flip mod (Matrix.nrows m) . subtract 1
    modcol = id
    modrow = id

    mapper :: Pos -> Cell -> Cell
    mapper p2 element = case element of
      Wall -> element
      Unvisited
        | indexMatch -> Only ttr
        | otherwise -> element
      (Only x)
        | indexMatch && even (x + ttr) -> Only (min x ttr)
        -- | indexMatch && odd (x + ttr) -> Both (min x ttr) (max x ttr)
        | otherwise -> element
      (Both x y)
        | indexMatch && even (x + ttr) -> Both (min x ttr) y
        | indexMatch && even (y + ttr) -> Both x (min y ttr)
        | otherwise -> element
      where
        indexMatch
          | p2 == first  (modrow . addition 1) p1 = True
          | p2 == first  (modrow . subtract 1) p1 = True
          | p2 == second (modcol . addition 1) p1 = True
          | p2 == second (modcol . subtract 1) p1 = True
          | otherwise = False
      --   | 
      -- | isWall element = element
      -- | isOnly
     
      -- | otherwise = element