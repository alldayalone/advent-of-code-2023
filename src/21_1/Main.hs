{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Data.Matrix as Matrix (Matrix, fromLists)
import Control.Arrow ((&&&), (>>>), second)
import Data.Time

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
findAllPos cond lists = foldr f [] (zip lists [0..])
  where 
    f :: ([a], Int) -> [Pos] -> [Pos]
    f (ls, x) acc = acc ++ fmap (id &&& const x) matches
      where
        matches = fmap snd . filter (\(element, _) -> cond element) $ zip ls [0..]

solve = id