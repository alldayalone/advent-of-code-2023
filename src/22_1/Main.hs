-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Control.Arrow
import Data.Time
import Data.List.Split
import Text.Show.Pretty
import Data.Sort

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/22_1/input_test.txt"
  writeFile ("src/22_1/output" ++ show utcNow ++ ".txt") . ppShowList . solve . settle . parse $ contents

data Vec3 = Vec3
  { x :: Int,
    y :: Int,
    z :: Int }
instance Show Vec3 where
  show Vec3 {..} = show x ++ "," ++ show y ++ "," ++ show z

data Block = Block
  {
    start :: Vec3,
    end :: Vec3
  }
instance Show Block where
  show Block {..} = show start ++ "~" ++ show end

parse = lines >>> fmap (splitOneOf ",~" >>> fmap read >>> toBlock)
  where toBlock [x1,y1,z1,x2,y2,z2] = Block { start = Vec3 { x=x1, y=y1, z=z1 }, end = Vec3 { x=x2, y=y2, z=z2 }}


settle = sortOn (start >>> z) 

findSupporters :: Block -> [Block] -> [Block]
findSupporters b = filter (isSupporter b)
  
-- First arg - the upper block, second arg - porential supporter block
isSupporter :: Block -> Block -> Bool
isSupporter (Block {..}) (Block { start=start2, end=end2}) = x end >= x start2 && x start <= x end2 && y end >= y start2 && y start <= y end2 && z start == z end2 + 1

solve :: [Block] -> [[Block]]
solve bs = [findSupporters b bs | b <- bs]
