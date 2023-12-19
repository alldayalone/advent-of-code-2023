{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
import System.IO
import Data.List.Split (splitOn, splitOneOf)
import Data.List (subsequences, stripPrefix, isPrefixOf, isSuffixOf, permutations, elemIndex)
import GHC.Plugins (fstOf3, sndOf3)

main = do
  contents <- readFile "src/03_1/input.txt"
  (print . sumIndexedNumber . filterNumbers . parseInput . zipXy) contents

-- char, x, y
type IndexedChar = (Char, Int, Int)
-- number, x-start, x-end, y
type IndexedNumber = (Int, Int, Int, Int)

-- sumIndexedNumber :: [IndexedNumber] -> Int
sumIndexedNumber = sum . map (\(x,_,_,_) -> x)

parseNumbers :: [IndexedChar] -> [IndexedNumber]
parseNumbers = snd . foldl foldLine ([], [])

parseChars :: [IndexedChar] -> [IndexedChar]
parseChars = filter ((`notElem` ".\n0123456789") . fstOf3)

parseInput :: [IndexedChar] -> ([IndexedChar], [IndexedNumber])
parseInput x = (parseChars x, parseNumbers x)

foldLine :: ([IndexedChar], [IndexedNumber]) -> IndexedChar -> ([IndexedChar], [IndexedNumber])
foldLine (acc, numbers) el@(ch, x, y)
  | ch `elem` ['0'..'9'] = (acc ++ [el], numbers)
  | null (map fstOf3 acc) = (acc, numbers)
  | otherwise = ([], numbers ++ [(read (map fstOf3 acc), (minimum . map sndOf3) acc, (maximum . map sndOf3) acc, y)])

zipXy :: [Char] -> [IndexedChar]
zipXy x = zip3 x (cycle [0..n]) (concatMap (replicate (n+1)) [0..])
  where Just n = elemIndex '\n' x

filterNumbers :: ([IndexedChar], [IndexedNumber]) -> [IndexedNumber]
filterNumbers (chars, nums) = filter (isTouchingChar chars) nums

isTouchingChar :: [IndexedChar] -> IndexedNumber -> Bool
isTouchingChar chars (_,x1,x2,y1) = any (\(_,x,y) -> x1-1 <= x && x <= x2+1 && y1-1 <= y && y <= y1+1) chars

debugReplaceNums :: [Char] -> [IndexedNumber] -> [Char]
debugReplaceNums contents nums = map (debugReplacer n nums) (zip contents [0..])
  where 
    Just n = elemIndex '\n' contents

debugReplacer :: Int -> [IndexedNumber] -> (Char, Int) -> Char
debugReplacer n nums (ch, i) = if any (\(_,x1,x2,y1) -> x1 <= x && x <= x2 && y1==y) nums then 'X' else ch
  where
    x = i `mod` (n + 1)
    y = i `div` (n + 1)
   
