{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Data.Matrix as Matrix
import Control.Arrow ((&&&), (>>>), second)
import Data.Time
import Data.Maybe 

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/21_1/input.txt"
  writeFile ("src/21_1/output" ++ show utcNow ++ ".txt") . show . (id &&& countResults) . solve . parse $ contents


type Pos = (Int, Int)
type TimeToReach = Int
type Elem = (Pos, TimeToReach) 

maxSteps :: Int
maxSteps = 99
-- maxSteps = maxBound

nSteps :: Int
nSteps = 64

parse :: String -> Matrix (Maybe Int)
parse = lines >>> fmap (fmap parseChar) >>> Matrix.fromLists
  where 
    parseChar '#' = Nothing
    parseChar 'S' = Just 0
    parseChar _ = Just maxSteps

findAllPos :: (Maybe Int -> Bool) -> Matrix (Maybe Int) -> [Elem]
findAllPos cond = fmap fromJust . filter isJust . Matrix.toList . Matrix.mapPos mapper
  where 
    mapper :: Pos -> Maybe Int -> Maybe Elem
    mapper pos element = if cond element then Just (pos, fromJust element) else Nothing


countResults :: Matrix (Maybe Int) -> Int
countResults = length . filter id . fmap ((\x ->x<maxSteps && even (x + nSteps)) . fromMaybe maxSteps) . Matrix.toList

solve :: Matrix (Maybe Int) -> Matrix (Maybe Int)
solve m = fst . (!! nSteps) . iterate step $ (m, 0)

step :: (Matrix (Maybe Int), Int) -> (Matrix (Maybe Int), Int)
step (m, stepCount) = (propagateAll m (findAllPos isEntryPoint m), stepCount + 1)
  where
    isEntryPoint :: Maybe Int -> Bool
    isEntryPoint x
      | fmap (== stepCount) x == Just True = True
      | otherwise = False

propagateAll :: Matrix (Maybe Int) -> [Elem] -> Matrix (Maybe Int)
propagateAll = foldr propagate

propagate :: Elem -> Matrix (Maybe Int) -> Matrix (Maybe Int)
propagate ((x1, y1), ttr) = Matrix.mapPos mapper
  where 
    mapper (x2, y2) element
      | isNothing element = element
      | x2 == x1-1 && y2 == y1 = fmap (min (ttr + 1)) element
      | x2 == x1+1 && y2 == y1 = fmap (min (ttr + 1)) element
      | x2 == x1 && y2 == y1-1 = fmap (min (ttr + 1)) element
      | x2 == x1 && y2 == y1+1 = fmap (min (ttr + 1)) element
      | otherwise = element