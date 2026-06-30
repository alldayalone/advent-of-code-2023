{-# LANGUAGE TupleSections #-}

module Main (main) where
import Data.HashMap.Strict as HashMap (HashMap, insert, empty, lookup)
import Control.Arrow ((***))
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
  contents <- readFile "src/19_1/input_test.txt"
  writeFile ("src/19_1/output" ++ show utcNow ++ ".txt") . ppShow . solve . parse $ contents

type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

type WorkflowLabel = String
data WorkflowStep = Accept | Reject | Go String | MaybeGo (Int, String, Int, String)
  deriving (Show)
type WorkflowMap = HashMap WorkflowLabel [WorkflowStep]
type Part = [Int]

parse :: String -> (WorkflowMap, [Part])
parse = (parseWorkflowMap *** parseParts) . unsafePair . splitOn "\n\n" 

unsafePair :: [a] -> (a, a)
unsafePair (x : y : _) = (x, y)

parseWorkflowMap :: String -> WorkflowMap
parseWorkflowMap = foldr (uncurry HashMap.insert . parseWorkflow) init . lines
  where 
    init = HashMap.insert "A" [Accept] (HashMap.insert "R" [Reject] HashMap.empty)

parseWorkflow :: String -> (WorkflowLabel, [WorkflowStep])
parseWorkflow s = (label, steps) 
  where
    regex = "^([a-zA-Z0-9_]+){(([ARa-z<>:0-9]+,?)+)}$"
    (_, _, _, groups) = s =~ regex :: (String, String, String, [String])
    (label:contentsBlock:_) = groups
    steps = map parseStep . splitOn "," $ contentsBlock

parseStep :: String -> WorkflowStep
parseStep "A" = Accept
parseStep "R" = Reject
parseStep s = case condition of
  "" -> Go workflowLabel
  otherwise -> MaybeGo (varLabelToIndex varLabel, sign, read valueStr, workflowLabel)
  where 
    regex = "(([xmas])([<>])([0-9]+):)?([a-zAR]+)"
    (_, _, _, groups) = s =~ regex :: (String, String, String, [String])
    (condition:varLabel:sign:valueStr:workflowLabel:_) = groups
  

varLabelToIndex :: String -> Int
varLabelToIndex "x" = 0
varLabelToIndex "m" = 1
varLabelToIndex "a" = 2
varLabelToIndex "s" = 3
varLabelToIndex label = error ("Invalid variable label \"" ++ label ++ "\"")

parseParts :: String -> [Part]
parseParts = map parsePart . lines

parsePart :: String -> Part
parsePart s = map read (getAllTextMatches (s =~ "[0-9]+"))

solve :: (WorkflowMap, [Part]) -> [[String]]
solve (_, []) = []
solve (wfMap, (part:parts)) = (runWorkflow (go "in") : solve (wfMap, parts))
  where 
    go :: String -> [WorkflowStep]
    go label = fromJust $ HashMap.lookup label wfMap

    runWorkflow :: [WorkflowStep] -> [String]
    runWorkflow (step:steps) = case step of 
      Accept -> ["A"]
      Reject -> ["R"]
      Go label -> [label] ++ runWorkflow (go label)
      MaybeGo (varIndex, sign, value, workflowLabel) ->
        if doesPartMatch varIndex sign value then [workflowLabel] ++ runWorkflow (go workflowLabel)
        else runWorkflow steps 
      
    doesPartMatch :: Int -> String -> Int -> Bool
    doesPartMatch varIndex "<" value = part !! varIndex < value
    doesPartMatch varIndex ">" value = part !! varIndex > value