{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where
import Data.Matrix as Matrix (Matrix, fromLists)
import Debug.Trace (trace)
import Data.List (transpose)
import Data.HashMap.Strict as HashMap (HashMap, insert, empty)
import Data.Hashable (Hashable, hash)
import GHC.Generics (Generic)

-- Plan:
-- Rewrite solve into 2 separate functions:
--  1. [x] Moving Os
--  2. [x] Calculating the load
-- [x] Write a function that takes current matrix and direction and returns the next matrix
-- [x] Start iterating cycles
-- Find the pattern - when matrix + direction is repeated
-- Skip unnecessary cycles mathematically

main :: IO ()
main = do
  contents <- readFile "src/14_2/input_test.txt"
  print . applyNtimes 4 Main.iterate . initialStep . parse $ contents

initialStep :: State -> IterationStep 
initialStep s = IterationStep HashMap.empty s 1

parse :: String -> State
parse = northState . transpose . lines

solve :: State -> Int
solve (State m _) = sum . map calcLoad $ m 

data Direction = North | West | South | East deriving (Show, Bounded, Eq, Generic, Enum, Hashable)
data State = State { matrix :: [String], direction :: Direction } deriving (Eq, Generic, Hashable)

instance Show State where
  show (State m West) = unlines m
  show s@(State m North) = show . turn $ s
  show s@(State m East) = show . turn . turn $ s
  show s@(State m South) = show . turn . turn . turn $ s

next :: (Eq a, Bounded a, Enum a) => a -> a
next x = if x == maxBound then minBound else succ x

northState :: [String] -> State
northState m = State m North

eastState :: [String] -> State
eastState m = State m East

data IterationStep = IterationStep { memory :: HashMap State Int, currentState :: State, step :: Int } deriving (Show, Eq, Generic, Hashable)


iterate :: IterationStep -> IterationStep
iterate (IterationStep memory currentState step) = IterationStep (HashMap.insert currentState step memory) (turn . tilt $ currentState) (step + 1)

turn :: State -> State
-- turn s | trace ("turn " ++ show s) False = undefined
turn (State m East) = State (transpose . map reverse $ m) North
turn (State m North) = State (transpose m) West
turn (State m West) = State (transpose . reverse $ m) South
turn (State m South) = State (map reverse . reverse . transpose $ m) East

spinCycle :: State -> State
spinCycle = applyNtimes 4 (turn . tilt)

tilt :: State -> State
-- tilt s | trace ("tilt\n" ++ show s) False = undefined
tilt (State m dir) = State (map tiltRow m) dir

tiltRow :: String -> String
tiltRow = tiltRow' 0 0

tiltRow' :: Int -> Int -> String -> String
tiltRow' cO cDot [] = generateSection cO cDot
tiltRow' cO cDot ('#':xs) = generateSection cO cDot ++ "#" ++ tiltRow xs
tiltRow' cO cDot ('O':xs) = tiltRow' (cO + 1) cDot xs
tiltRow' cO cDot ('.':xs) = tiltRow' cO (cDot + 1) xs

generateSection :: Int -> Int -> String
generateSection cO cDot = replicate cO 'O' ++ replicate cDot '.'

calcLoad :: String -> Int
-- calcLoad v | trace ("fn " ++ v) False = undefined
calcLoad [] = 0
calcLoad ('O':xs) = length xs + 1 + calcLoad xs
calcLoad (_:xs) = calcLoad xs

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)