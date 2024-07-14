module Main (main, longestPalindrome) where
import Data.Vector as Vector (Vector, eqBy, take, tail, reverse, fromList, length, replicate, slice, (//), empty, singleton)
import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
  contents <- readFile "src/13_1/input.txt"
  print . sum . map (solve . lines) . splitOn "\n\n" $ contents

solve :: Eq a => [[a]] -> Int
solve x = if lenHorizontal > lenVertical then 100 * (lenHorizontal `div` 2 + snd lpHorizontal) else lenVertical `div` 2 + snd lpVertical
  where 
    lpHorizontal = longestPalindrome (Vector.fromList x)
    lpVertical = longestPalindrome (Vector.fromList (transpose x))
    lenHorizontal = Vector.length (fst lpHorizontal)
    lenVertical = Vector.length (fst lpVertical)


-- @todo: memo
longestPalindrome :: Eq a => Vector a -> (Vector a, Int)
longestPalindrome v = case n of
  0 -> (Vector.empty, 0)
  1 -> (Vector.empty, 0) -- only odd length palindromes allowed
  _ -> if isPalindrome v then (v, 0) else if Vector.length leftLongest > Vector.length rightLongest then (leftLongest, 0) else (rightLongest, posRight)
    where 
      leftLongest = longestPalindromeLeft (Vector.take (n-1) v)
      (rightLongest, posRight) = longestPalindromeRight (Vector.tail v, 1)
  where 
    n = Vector.length v

longestPalindromeLeft :: Eq a => Vector a -> Vector a
longestPalindromeLeft v = case n of
  0 -> Vector.empty
  1 -> Vector.empty -- only odd length palindromes allowed
  _ -> if isPalindrome v then v else longestPalindromeLeft (Vector.take (n-1) v)
  where 
    n = Vector.length v

longestPalindromeRight :: Eq a => (Vector a, Int) -> (Vector a, Int)
longestPalindromeRight (v, pos) = case n of
  0 -> (Vector.empty, 0)
  1 -> (Vector.empty, 0) -- only odd length palindromes allowed
  _ -> if isPalindrome v then (v, pos) else longestPalindromeRight (Vector.tail v, pos + 1)
  where 
    n = Vector.length v
   
isPalindrome :: Eq a => Vector a -> Bool
isPalindrome v = Vector.eqBy (==) v (Vector.reverse v)