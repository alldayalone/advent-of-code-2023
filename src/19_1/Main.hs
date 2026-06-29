{-# LANGUAGE TupleSections #-}

module Main (main) where
import Data.HashMap.Strict as HashMap (HashMap, insert, empty, lookup)
import Control.Arrow ((***))
import Data.Matrix as Matrix (Matrix (nrows, ncols), (!), fromLists, extendTo, setElem, matrix, mapPos)
import Data.Matrix ((<|>), (<->))
import Data.Time
import Data.Maybe (isNothing, isJust)
import Data.PSQueue as PSQ (PSQ(..), empty, fromList, size, lookup, insert, minView, null, adjust)
import Data.PSQueue.Internal (Binding(..))
import Data.List (find)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/19_1/input_test.txt"
  writeFile ("src/19_1/output" ++ show utcNow ++ ".txt") . show . solve . parse $ contents

type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

type WorkflowMap = HashMap String String
type Part = String

parse :: String -> (WorkflowMap, [Part])
parse = (parseWorkflowMap *** parseParts) . unsafePair . splitOn "\n\n" 
 
unsafePair :: [a] -> (a, a)
unsafePair (x : y : _) = (x, y)

parseWorkflowMap :: String -> WorkflowMap
parseWorkflowMap = foldr folder HashMap.empty . lines
  where folder x = HashMap.insert x x

parseParts :: String -> [Part]
parseParts = lines

-- HashMap.insert currentState step memory
-- parseLine :: String -> Instruction
-- parseLine line = (direction, read number, color)
  -- where
    -- [direction, number, color] = words line


-- data Step = (Int) | Accept | Reject

-- parse _ = []
solve x = x

-- runWorkflow :: Worflow -> Part -> [String]
-- runWorkflow (step, ...steps) part = case step of 
--   Decision A -> true
--   Decision R -> false
--   otherwise = false