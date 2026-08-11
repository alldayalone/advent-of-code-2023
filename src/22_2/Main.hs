-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where
import Control.Arrow
import Data.Function
import Data.Time
import Data.List.Split
import Text.Show.Pretty
import Data.List.Unique (complex)
import Data.Sort
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict as HashMap (HashMap, (!))
import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Maybe
import GHC.Generics (Generic)
import Data.Char

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/22_2/input.txt"
  writeFile ("src/22_2/output" ++ show utcNow ++ ".txt") . show . solve . settle . parse $ contents

data Vec3 = Vec3
  { x :: Int,
    y :: Int,
    z :: Int }
    deriving (Eq, Generic, Hashable)
instance Show Vec3 where
  show Vec3 {..} = show x ++ "," ++ show y ++ "," ++ show z

data Block = Block
  {
    start :: Vec3,
    end :: Vec3
  }
  deriving (Eq, Generic, Hashable)
instance Show Block where
  show Block {..} = show start ++ "~" ++ show end

parse = lines >>> fmap (splitOneOf ",~" >>> fmap read >>> toBlock)
  where toBlock [x1,y1,z1,x2,y2,z2] = Block { start = Vec3 { x=x1, y=y1, z=z1 }, end = Vec3 { x=x2, y=y2, z=z2 }}


settle = sortOn base >>> settle' []

base = start >>> z

shiftZ :: (Int -> Int) -> Block -> Block
shiftZ f (Block {..}) = Block { start = start { z = f (z start) }, end = end { z = f (z end) } }

shiftZSub1 = shiftZ (subtract 1)

findSupporters :: [Block] -> Block -> [Block]
findSupporters bs b = filter (isSupporter b) bs

findSupportersHS :: HashSet Block -> Block -> HashSet Block
findSupportersHS bs b = HS.filter (isSupporter b) bs
  
-- First arg - the upper block, second arg - potential supporter block
isSupporter :: Block -> Block -> Bool
isSupporter (Block {..}) (Block { start=start2, end=end2}) = x end >= x start2 && x start <= x end2 && y end >= y start2 && y start <= y end2 && z start == z end2 + 1

findSupportees :: [Block] -> Block -> [Block]
findSupportees bs b = filter (isSupportee b) bs

findSupporteesHS :: HashSet Block -> Block -> HashSet Block
findSupporteesHS bs b = HS.filter (isSupportee b) bs

-- First arg - the down block, second arg - potential supportee block
isSupportee :: Block -> Block -> Bool
isSupportee (Block {..}) (Block { start=start2, end=end2}) = x end >= x start2 && x start <= x end2 && y end >= y start2 && y start <= y end2 && z end == z start2 - 1

-- solve' :: [Block] -> Int
-- solve' bs = length bs - (length . uniq . concat . filter (length >>> (==1)) $ [findSupporters bs b | b <- bs])

solve = sortOn base >>> solve'

settle' :: [Block] -> [Block] -> [Block]
settle' settled (b:bs)
  | base b > 1 && null supporters = settle' settled (shiftZSub1 b:bs)
  | otherwise = settle' (settled ++ [b]) bs
    where supporters = findSupporters settled b
settle' settled [] = settled


type Memo = HashMap (HashSet Block) Int
solve' :: [Block] -> Int
solve' bs = sum . fmap (bt HS.empty . HS.singleton) $ bs
  where 
    bsHS = HS.fromList bs

    isSingletonHS :: HashSet Block -> Int -> Bool
    isSingletonHS hs _ = HS.size hs == 1

    toCountZeroDrops:: HashSet Block -> Int -> Bool
    toCountZeroDrops hs drops = HS.size hs == 1 && drops == 0

    setToLetter :: HashSet Block -> String
    setToLetter = concatMap blockToLetter . HS.toList

    blockToLetter :: Block -> String
    blockToLetter = (letters !)

    letters :: HashMap Block String
    letters = HM.fromList (zip bs [[c1, c2, c3] | c1 <- ['A'..'Z'], c2 <- ['A'..'Z'], c3 <- ['A'..'Z']])

    supporters :: HashMap Block (HashSet Block)
    supporters = HM.fromList [(b, findSupportersHS bsHS b) | b <- bs]

    supportees :: HashMap Block (HashSet Block)
    supportees = HM.fromList [(b, findSupporteesHS bsHS b) | b <- bs]

    -- all blocks (that are dropped from the beginning of chain drop)
    -- fwdbs (forward blocks, aka front line of destruction)
    bt :: HashSet Block -> HashSet Block -> Int
    bt allbs fwdbs
      | HS.null fwdbs = HS.size allbs - 1 -- (minus one because we only count OTHER dropped blocks)
      | otherwise = bt nextallbs nextfwdbs
      where
        nextfwdbs = dropped fwdbs
        nextallbs = allbs <> fwdbs

        -- {B,C} -> {D,E} -> filter D -> {B,C} -> {B,C} - {B,C} -> [] -> True -> {D,E}
        dropped :: HashSet Block -> HashSet Block
        dropped = foldMap (supportees !) >>> HS.filter ((supporters !) >>> flip HS.difference nextallbs >>> null)

-- uniq = complex >>> fst3

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

intToLetter :: Int -> Char
intToLetter n = chr (n + 64)

-- Blackbird operator
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)