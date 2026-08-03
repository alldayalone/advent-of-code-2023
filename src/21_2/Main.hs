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
  writeFile ("src/21_2/output" ++ show utcNow ++ ".txt") . show . (id &&& countResults) . solve . parse $ contents


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


isWall Wall = True
isWall _ = False
isVisitable = not . isWall

doesCount :: Cell -> Bool
doesCount Wall = False
doesCount Unvisited = False
doesCount (Only x) = even (x + nSteps)
doesCount (Both x y) = True

nSteps :: Int
nSteps = 130

parse :: String -> Field
parse = lines >>> fmap (fmap parseChar) >>> Matrix.fromLists
  where 
    parseChar '#' = Wall
    parseChar 'S' = Only 0
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

countResults :: Field -> Int
countResults = length . filter id . fmap doesCount . Matrix.toList

solve :: Field -> Field
solve m = fst . (!! nSteps) . iterate step $ (m, 0)

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