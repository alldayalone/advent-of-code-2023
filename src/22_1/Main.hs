-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Control.Arrow
import Data.Time
import Data.List.Split
import Text.Show.Pretty
import Data.List.Unique
import Data.Sort

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/22_1/input.txt"
  writeFile ("src/22_1/output" ++ show utcNow ++ ".txt") . show . solve . settle . parse $ contents

data Vec3 = Vec3
  { x :: Int,
    y :: Int,
    z :: Int }
    deriving (Eq)
instance Show Vec3 where
  show Vec3 {..} = show x ++ "," ++ show y ++ "," ++ show z

data Block = Block
  {
    start :: Vec3,
    end :: Vec3
  }
  deriving (Eq)
instance Show Block where
  show Block {..} = show start ++ "~" ++ show end

parse = lines >>> fmap (splitOneOf ",~" >>> fmap read >>> toBlock)
  where toBlock [x1,y1,z1,x2,y2,z2] = Block { start = Vec3 { x=x1, y=y1, z=z1 }, end = Vec3 { x=x2, y=y2, z=z2 }}


settle = sortOn base >>> settle' []

base = start >>> z

shiftZ :: (Int -> Int) -> Block -> Block
shiftZ f (Block {..}) = Block { start = start { z = f (z start) }, end = end { z = f (z end) } }

shiftZSub1 = shiftZ (subtract 1)

findSupporters :: Block -> [Block] -> [Block]
findSupporters b = filter (isSupporter b)
  
-- First arg - the upper block, second arg - potential supporter block
isSupporter :: Block -> Block -> Bool
isSupporter (Block {..}) (Block { start=start2, end=end2}) = x end >= x start2 && x start <= x end2 && y end >= y start2 && y start <= y end2 && z start == z end2 + 1

findSupportees :: Block -> [Block] -> [Block]
findSupportees b = filter (isSupportee b)

-- First arg - the down block, second arg - potential supportee block
isSupportee :: Block -> Block -> Bool
isSupportee (Block {..}) (Block { start=start2, end=end2}) = x end >= x start2 && x start <= x end2 && y end >= y start2 && y start <= y end2 && z start == z end2 - 1

solve' :: [Block] -> Int
solve' bs = length bs - (length . uniq . concat . filter (length >>> (==1)) $ [findSupporters b bs | b <- bs])

solve = sortOn base >>> solve'

settle' :: [Block] -> [Block] -> [Block]
settle' settled (b:bs)
  | base b > 1 && null supporters = settle' settled (shiftZSub1 b:bs)
  | otherwise = settle' (settled ++ [b]) bs
    where supporters = findSupporters b settled
settle' settled [] = settled
