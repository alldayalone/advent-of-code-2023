{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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

parse :: String -> (Matrix (Maybe Int), [Pos])
parse = lines >>> (Matrix.fromLists >>> fmap parseChar) &&& findAllPos (=='S')
  where 
    parseChar '#' = Nothing
    parseChar _ = Just 0

findAllPos :: forall a. (a -> Bool) -> [[a]] -> [Pos]
findAllPos cond lists = foldr f [] (zip lists [1..])
  where 
    f :: ([a], Int) -> [Pos] -> [Pos]
    f (ls, x) acc = acc ++ fmap (id &&& const x) matches
      where
        matches = fmap snd . filter (\(element, _) -> cond element) $ zip ls [1..]

solve :: (Matrix (Maybe Int), [Pos]) -> Matrix (Maybe Int)
solve (m, queue) = foldr solve' m queue

solve' ::  Pos -> Matrix (Maybe Int) -> Matrix (Maybe Int)
solve' (x1, y1) = Matrix.mapPos mapper
  where 
    mapper (x2, y2) element
      | isNothing element = element
      | x2 == x1-1 && y2 == y1 = fmap (+1) element
      | x2 == x1+1 && y2 == y1 = fmap (+1) element
      | x2 == x1 && y2 == y1-1 = fmap (+1) element
      | x2 == x1 && y2 == y1+1 = fmap (+1) element
      | otherwise = element