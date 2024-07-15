{-# LANGUAGE DerivingStrategies, DeriveGeneric, TypeOperators, TypeFamilies #-}

module Main (main, longestPalindrome) where
import Data.Vector as Vector (Vector, eqBy, take, tail, reverse, fromList, length, replicate, slice, (//), empty, singleton)
import Data.Matrix as Matrix
import Data.List.Split (splitOn)
import Data.List (transpose, find)
import Data.Maybe (fromJust)
import GHC.Plugins (fstOf3, sndOf3, thdOf3)
import Data.MemoTrie 
import GHC.Generics (Generic)

main :: IO ()
main = do
  contents <- readFile "src/13_2/input_unit2.txt"
  mapM_ print . map (solveWithSmudge . Matrix.fromLists . lines) . splitOn "\n\n" $ contents

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

tupleToList :: (a, a) -> [a]
tupleToList (a1, a2) = [a1, a2]

solveWithSmudge m = fstOf3 . fromJust $ theOtherSolution
  where 
    originalSolutions = solve m
    originalSolution = if fstOf3 (fst originalSolutions) == 0 then snd originalSolutions else fst originalSolutions
    allPossibleSmudges = Matrix.toList $ Matrix.mapPos (fixSmudgeWith (toggle '.' '#') m) m
    -- allPossibleSolutions = map solve allPossibleSmudges
    theOtherSolution = find (\(value, pos, dir) -> value > 0 && (dir /= thdOf3 originalSolution || pos /= sndOf3 originalSolution) ) . concatMap (tupleToList . solve) $ allPossibleSmudges


lpValue :: Eq a => (Vector [a], Int) -> Int
lpValue (v, pos) = Vector.length v `div` 2 + pos

fixSmudgeWith :: Eq a => (a -> a) -> Matrix a -> (Int, Int) -> a -> Matrix a
fixSmudgeWith fn m pos ch = Matrix.setElem (fn ch) pos m

toggle :: Eq a => a -> a -> a -> a
toggle v1 v2 x = if x == v1 then v2 else v1

type Solution a = (Int, Int, Direction)

solve :: Eq a => Matrix a -> (Solution a, Solution a)
solve x = ((100 * lpValue lpHorizontal, snd lpHorizontal, Horizontal), (lpValue lpVertical, snd lpVertical, Vertical))
  where 
    lpHorizontal = longestPalindrome . Vector.fromList . Matrix.toLists $ x
    lpVertical = longestPalindrome . Vector.fromList . Matrix.toLists . Matrix.transpose $ x
    lenHorizontal = Vector.length . fst $ lpHorizontal
    lenVertical = Vector.length . fst $ lpVertical

data Direction = Horizontal | Vertical deriving (Show, Eq)

-- instance HasTrie (Vector a) where
--   newtype (Vector a :->: b) = NodeTrie { unNodeTrie :: Reg (Vector a) :->: b } 
--   trie      = 
--   untrie    = untrieGeneric unNodeTrie
--   enumerate = enumerateGeneric unNodeTrie



-- longestPalindromeLeftM :: Eq a => Matrix a -> (Solution a, Solution a)
-- longestPalindromeLeftM = memo longestPalindromeLeft

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