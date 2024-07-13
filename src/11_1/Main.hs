{-# LANGUAGE TupleSections #-}

module Main (main) where
import Data.Vector as Vector (Vector, replicate, slice, (//))
import GHC.Plugins (sndOf3, thdOf3)

main :: IO ()
main = do
  contents <- readFile "src/11_1/input.txt"
  print . solve . filterGalaxies . zipXY $ contents

type Galaxy = (Int, Int)
type IndexedChar = (Char, Int, Int)

zipXY' :: Int -> Int -> String -> [IndexedChar]
zipXY' _ _ [] = []
zipXY' x y (ch:rest) = (ch, x, y) : zipXY' nextX nextY rest
  where nextX = if ch == '\n' then 0 else x+1
        nextY = if ch == '\n' then y+1 else y
zipXY = zipXY' 0 0

isGalaxy :: IndexedChar -> Bool
isGalaxy (ch, _, _) = ch == '#'
filterGalaxies = filter isGalaxy

solve :: [IndexedChar] -> Int
solve galaxyList = sum $ map expandedDist galaxyPairs
  where xs = map sndOf3 galaxyList
        ys = map thdOf3 galaxyList
        lenX = maximum xs + 1
        lenY = maximum ys + 1
        initialExpanderX = Vector.replicate lenX 1
        initialExpanderY = Vector.replicate lenY 1
        expanderX = initialExpanderX // map (, 0) xs
        expanderY = initialExpanderY // map (, 0) ys
        galaxyPairs = pairs galaxyList
        expandedDist = dist expanderX expanderY

buildExpander :: Vector Int -> [Int] -> Vector Int
buildExpander expander [] = expander
build_expander expander (x:xs) = build_expander (expander ++ [x]) xs 

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x,) xs ++ pairs xs

dist :: Vector Int -> Vector Int -> (IndexedChar, IndexedChar) ->  Int
dist expanderX expanderY ((ch1, x1, y1), (ch2, x2, y2)) = distX + distY + expansionX + expansionY
  where 
    distX = abs $ x1 - x2
    distY = abs $ y1 - y2
    expansionX = sum $ Vector.slice (min x1 x2) distX expanderX
    expansionY = sum $ Vector.slice (min y1 y2) distY expanderY