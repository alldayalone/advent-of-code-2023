module Main (main) where
import Data.Matrix as Matrix (Matrix, fromLists, getCol)
import Data.Vector as Vector (Vector, toList)
import Debug.Trace (trace)

main :: IO ()
main = do
  contents <- readFile "src/14_1/input_test.txt"
  print . solve . parse $ contents

parse :: String -> Matrix Char
parse = Matrix.fromLists . lines

solve :: Matrix Char -> Int
solve = solveV . Vector.toList . Matrix.getCol 1

solveV :: String -> Int
solveV v = fn 0 0 (length v) (length v) v

fn :: Int -> Int -> Int -> Int -> String -> Int
fn tv lc tcd lcd v | trace ("fn " ++ show tv ++ " " ++ show lc ++ " " ++ show tcd ++ " " ++ show lcd ++ " " ++ v) False = undefined
fn tv lc tcd lcd  [] = tv
fn tv lc tcd lcd ('#':xs) = fn (tv + value lcd lc) 0        (tcd - 1) tcd xs
fn tv lc tcd lcd ('O':xs) = fn                 tv  (lc + 1) (tcd - 1) lcd xs
fn tv lc tcd lcd ('.':xs) = fn                 tv  lc       (tcd - 1) lcd xs

value :: Int -> Int -> Int
value a n = n * (2 * a - n + 1) `div` 2
