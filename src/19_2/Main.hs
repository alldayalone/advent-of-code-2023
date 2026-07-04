{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Main (main, splitSegment, splitSegmentUnsigned, perm, slicesToPart) where
import Data.HashMap.Strict as HashMap (HashMap, insert, empty, lookup, toList, fromList, adjust, map, elems)
import Control.Arrow ((&&&))
import Data.Matrix ((<|>), (<->))
import Data.Time
import Data.Maybe (isNothing, isJust, fromJust)
import Data.PSQueue as PSQ (PSQ(..), empty, fromList, size, lookup, insert, minView, null, adjust)
import Data.PSQueue.Internal (Binding(..))
import Data.List (find)
import Data.List.Split (splitOn)
import Text.Regex.TDFA     
import Text.Show.Pretty (pPrint, ppShow)

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/19_2/input.txt"
  writeFile ("src/19_2/output" ++ show utcNow ++ ".txt") . ppShow   . parse $ contents

type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

type WorkflowLabel = String
data WorkflowStep = Accept | Reject | Go String | MaybeGo (Int, String, Int, String)
  deriving (Show)
type WorkflowMap = HashMap WorkflowLabel [WorkflowStep]
type Part = ([Int], Int)

parse :: String -> (WorkflowMap, [[Slice]])
parse = (id &&& generateParts) . parseWorkflowMap . head . splitOn "\n\n" 

parseWorkflowMap :: String -> WorkflowMap
parseWorkflowMap = foldr (uncurry HashMap.insert . parseWorkflow) init . lines
  where 
    init = HashMap.fromList [("A", [Accept]), ("R", [Reject])]

parseWorkflow :: String -> (WorkflowLabel, [WorkflowStep])
parseWorkflow s = (label, steps) 
  where
    regex = "^([a-zA-Z0-9_]+){(([ARa-z<>:0-9]+,?)+)}$"
    (_, _, _, groups) = s =~ regex :: (String, String, String, [String])
    (label:contentsBlock:_) = groups
    steps = Prelude.map parseStep . splitOn "," $ contentsBlock

parseStep :: String -> WorkflowStep
parseStep "A" = Accept
parseStep "R" = Reject
parseStep s = case condition of
  "" -> Go workflowLabel
  _ -> MaybeGo (varLabelToIndex varLabel, sign, read valueStr, workflowLabel)
  where 
    regex = "(([xmas])([<>])([0-9]+):)?([a-zAR]+)"
    (_, _, _, groups) = s =~ regex :: (String, String, String, [String])
    (condition:varLabel:sign:valueStr:workflowLabel:_) = groups

type Segment = (Int, Int)
type SegmentMap = HashMap Int [Segment]

type Slice = (Int, Int)
type SliceMap = HashMap Int [Slice]

perm :: Show a => [[a]] -> [[a]]
perm [] = []
perm [x] = Prelude.map (:[]) x
perm ([x]:ys) = Prelude.map (x:) (perm ys)
perm ((x:xs):ys) = perm ([x]:ys) ++ perm (xs:ys)

slicesToPart :: [Slice] -> ([Int], Int)
slicesToPart = Prelude.map fst &&& product . Prelude.map snd

generateParts :: WorkflowMap -> [[Slice]]
generateParts = HashMap.elems . HashMap.map (Prelude.map segmentToSlice) . generateSegmentMap
  
segmentToSlice :: Segment -> Slice
segmentToSlice (start, end) = (start, end - start + 1)

initSegment = (1, 4000)

generateSegmentMap :: WorkflowMap -> SegmentMap
generateSegmentMap = foldr folder init . concatMap snd . HashMap.toList
  where
    init = HashMap.fromList (zip [0..] (replicate 4 [initSegment]))
    folder :: WorkflowStep -> SegmentMap -> SegmentMap
    folder (MaybeGo (varIndex, sign, value, _)) = HashMap.adjust (splitSegment sign value) varIndex
    folder _ = id

splitSegment :: String -> Int -> [Segment] -> [Segment]
splitSegment ">" val xs  = splitSegmentUnsigned val xs 
splitSegment "<" val xs = splitSegmentUnsigned (val - 1) xs 

splitSegmentUnsigned :: Int -> [Segment] -> [Segment]
splitSegmentUnsigned val ((l,r):xs)
  | r < val = (l,r):splitSegmentUnsigned val xs
  | r == val || l == val = (l,r):xs
  | r > val = (l,val):(val + 1, r):xs


varLabelToIndex :: String -> Int
varLabelToIndex "x" = 0
varLabelToIndex "m" = 1
varLabelToIndex "a" = 2
varLabelToIndex "s" = 3
varLabelToIndex label = error ("Invalid variable label \"" ++ label ++ "\"")

solve :: (WorkflowMap, [Part]) -> [Int]
solve (_, []) = []
solve (wfMap, (part,partCount):parts) = runWorkflow (go "in") : solve (wfMap, parts)
  where 
    go :: String -> [WorkflowStep]
    go label = fromJust $ HashMap.lookup label wfMap

    runWorkflow :: [WorkflowStep] -> Int
    runWorkflow (step:steps) = case step of 
      Accept -> partCount
      Reject -> 0
      Go label -> runWorkflow (go label)
      MaybeGo (varIndex, sign, value, workflowLabel) ->
        if doesPartMatch varIndex sign value then runWorkflow (go workflowLabel)
        else runWorkflow steps 
      
    doesPartMatch :: Int -> String -> Int -> Bool
    doesPartMatch varIndex "<" value = part !! varIndex < value
    doesPartMatch varIndex ">" value = part !! varIndex > value