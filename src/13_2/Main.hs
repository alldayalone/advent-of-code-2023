{-# LANGUAGE DerivingStrategies, DeriveGeneric, TypeOperators, TypeFamilies #-}

module Main (main, longestPalindrome) where
import Data.Vector as Vector (Vector, eqBy, take, tail, reverse, fromList, length, replicate, slice, (//), empty, singleton)
import Data.Matrix as Matrix
import Data.List.Split (splitOn)
import Data.List (transpose, find)
import Data.Maybe (fromMaybe,isJust,fromJust,isNothing)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.MemoTrie 
import GHC.Generics (Generic)

main :: IO ()
main = do
  contents <- readFile "src/13_2/input_test.txt"
  print . sum . map (solveWithSmudge . Matrix.fromLists . lines) . splitOn "\n\n" $ contents

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

tupleToList :: (a, a) -> [a]
tupleToList (a1, a2) = [a1, a2]

solveWithSmudge m = lpValue . fromJust $ theOtherSolution
  where 
    originalSolutions = solve Nothing m
    originalSolution = if fst3 (fst originalSolutions) == 0 then snd originalSolutions else fst originalSolutions
    allPossibleSmudges = Matrix.toList $ Matrix.mapPos (fixSmudgeWith (toggle '.' '#') m) m
    allPossibleSolutions = map (solve (Just originalSolution)) allPossibleSmudges
    theOtherSolution = find (\sol -> fst3 sol > 0) . concatMap (tupleToList . solve (Just originalSolution)) $ allPossibleSmudges


lpValue :: Solution -> Int
lpValue (len, pos, Horizontal) = 100 * len `div` 2 + pos
lpValue (len, pos, Vertical) = len `div` 2 + pos

fixSmudgeWith :: Eq a => (a -> a) -> Matrix a -> (Int, Int) -> a -> Matrix a
fixSmudgeWith fn m pos ch = Matrix.setElem (fn ch) pos m

toggle :: Eq a => a -> a -> a -> a
toggle v1 v2 x = if x == v1 then v2 else v1

type Solution = (Int, Int, Direction)

solve :: Eq a => Maybe Solution -> Matrix a -> (Solution, Solution)
solve origSol x = ((lenHorizontal, snd lpHorizontal, Horizontal), (lenVertical, snd lpVertical, Vertical))
  where 
    origHorizontal = if isJust origSol && (thd3 . fromJust $ origSol) == Horizontal then origSol else Nothing
    origVertical = if isJust origSol && (thd3 . fromJust $ origSol) == Vertical then origSol else Nothing
    lpHorizontal = longestPalindrome origHorizontal . Vector.fromList . Matrix.toLists $ x
    lpVertical = longestPalindrome origVertical . Vector.fromList . Matrix.toLists . Matrix.transpose $ x
    lenHorizontal = Vector.length . fst $ lpHorizontal
    lenVertical = Vector.length . fst $ lpVertical

data Direction = Horizontal | Vertical deriving (Show, Eq)

longestPalindrome :: Eq a => Maybe Solution -> Vector a -> (Vector a, Int)
longestPalindrome origSol v = case n of
  0 -> (Vector.empty, 0)
  1 -> (Vector.empty, 0) -- only odd length palindromes allowed
  _ -> if isPalindrome origSol 0 v then (v, 0) else if Vector.length leftLongest > Vector.length rightLongest then (leftLongest, 0) else (rightLongest, posRight)
    where 
      leftLongest = longestPalindromeLeft origSol (Vector.take (n-1) v)
      (rightLongest, posRight) = longestPalindromeRight origSol (Vector.tail v, 1)
  where 
    n = Vector.length v

longestPalindromeLeft :: Eq a => Maybe Solution -> Vector a -> Vector a
longestPalindromeLeft origSol v = case n of
  0 -> Vector.empty
  1 -> Vector.empty -- only odd length palindromes allowed
  _ -> if isPalindrome origSol 0 v then v else longestPalindromeLeft origSol (Vector.take (n-1) v)
  where 
    n = Vector.length v

longestPalindromeRight :: Eq a => Maybe Solution -> (Vector a, Int) -> (Vector a, Int)
longestPalindromeRight origSol (v, pos) = case n of
  0 -> (Vector.empty, 0)
  1 -> (Vector.empty, 0) -- only odd length palindromes allowed
  _ -> if isPalindrome origSol pos v then (v, pos) else longestPalindromeRight origSol (Vector.tail v, pos + 1)
  where 
    n = Vector.length v
   
isPalindrome :: Eq a => Maybe Solution -> Int -> Vector a -> Bool
isPalindrome origSol pos v = Vector.eqBy (==) v (Vector.reverse v) && (isNothing origSol || isJust origSol && ((fst3 . fromJust $ origSol) /= Vector.length v || (snd3 . fromJust $ origSol) /= pos))