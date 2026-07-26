{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Data.Matrix as Matrix (Matrix, fromLists, mapPos)
import Control.Arrow ((&&&), (>>>), second)
import Data.Time
import Data.Maybe 

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/21_1/input_test.txt"
  writeFile ("src/21_1/output" ++ show utcNow ++ ".txt") . show . solve . parse $ contents


type Pos = (Int, Int)
type TimeToReach = Int
type Elem = (Pos, TimeToReach) 

maxTime :: Int
maxTime = 99
-- maxTime = maxBound

parse :: String -> (Matrix (Maybe Int), [Elem])
parse = lines >>> fmap (fmap parseChar) >>> Matrix.fromLists &&& findAllPos isEntryPoint
  where 
    parseChar '#' = Nothing
    parseChar 'S' = Just 0
    parseChar _ = Just maxTime

    isEntryPoint :: Maybe Int -> Bool
    isEntryPoint x
      | fmap (< maxTime) x == Just True = True
      | otherwise = False

findAllPos :: (Maybe Int -> Bool) -> [[Maybe Int]] -> [Elem]
findAllPos cond lists = foldr f [] (zip lists [1..])
  where 
    f :: ([Maybe Int], Int) -> [Elem] -> [Elem]
    f (ls, y) acc = acc ++ matches
      where
        matches = fmap (\(element, x) -> ((x, y), fromJust element)) . filter (\(element, _) -> cond element) $ zip ls [1..]

solve :: (Matrix (Maybe Int), [Elem]) -> Matrix (Maybe Int)
solve (m, queue) = foldr propagate m queue


-- propagateAll (Matrix (Maybe Int), [Pos]) -> Matrix (Maybe Int)

propagate :: Elem -> Matrix (Maybe Int) -> Matrix (Maybe Int)
propagate ((x1, y1), ttr) = Matrix.mapPos mapper
  where 
    mapper (x2, y2) element
      | isNothing element = element
      | x2 == x1-1 && y2 == y1 = fmap (min (ttr + 1)) element
      | x2 == x1+1 && y2 == y1 = fmap (min (ttr + 1)) element
      | x2 == x1 && y2 == y1-1 = fmap (min (ttr + 1)) element
      | x2 == x1 && y2 == y1+1 = fmap (min (ttr + 1)) element
      | otherwise = Just maxTime