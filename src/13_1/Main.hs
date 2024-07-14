{-# LANGUAGE TupleSections #-}

module Main (main) where
import Data.Vector as Vector (Vector, eqBy, take, tail, reverse, fromList, length, replicate, slice, (//), empty, singleton)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- readFile "src/13_1/input_test.txt"
  print . map (longestPalindrome . (,0) . Vector.fromList . lines) . splitOn "\n\n" $ contents


-- @todo: memo
longestPalindrome :: Eq a => (Vector a, Int) -> (Vector a, Int)
longestPalindrome (v, pos) = case n of
  0 -> (Vector.empty, pos)
  1 -> (Vector.empty, pos) -- only odd length palindromes allowed
  _ -> if isPalindrome v then (v, pos) else if Vector.length leftLongest > Vector.length rightLongest then (leftLongest, pos + posLeft) else (rightLongest, pos + posRight + 1)
    where 
      (leftLongest, posLeft) = longestPalindrome (Vector.take (n-1) v, pos)
      (rightLongest, posRight) = longestPalindrome (Vector.tail v, pos)
  where 
    n = Vector.length v
   
isPalindrome :: Eq a => Vector a -> Bool
isPalindrome v = Vector.eqBy (==) v (Vector.reverse v)