{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Main (main) where
import Data.HashMap.Strict as HashMap (HashMap, insert, empty, lookup, toList, fromList, adjust, elems)
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
  writeFile ("src/19_2/output" ++ show utcNow ++ ".txt") . ppShow  . solve . parse $ contents

type Color = String
type Direction = String
type Instruction = (Direction, Int, Color)

type WorkflowLabel = String
data WorkflowStep = Accept | Reject | Go String | MaybeGo (Int, String, Int, String)
  deriving (Show)
type WorkflowMap = HashMap WorkflowLabel [WorkflowStep]
type Slice = (Int, Int)
type Part = HashMap Int Slice

initSlice :: Slice
initSlice = (1, 4000)

initPart :: Part
initPart = HashMap.fromList [(0, initSlice), (1, initSlice), (2, initSlice), (3, initSlice)]

parse :: String -> WorkflowMap
parse = parseWorkflowMap . head . splitOn "\n\n" 

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
    steps = map parseStep . splitOn "," $ contentsBlock

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

varLabelToIndex :: String -> Int
varLabelToIndex "x" = 0
varLabelToIndex "m" = 1
varLabelToIndex "a" = 2
varLabelToIndex "s" = 3
varLabelToIndex label = error ("Invalid variable label \"" ++ label ++ "\"")

solve :: WorkflowMap -> Int
solve wfMap = runWorkflow (Just initPart) (go "in") 
  where 
    go :: String -> [WorkflowStep]
    go label = fromJust $ HashMap.lookup label wfMap

    runWorkflow :: Maybe Part -> [WorkflowStep] -> Int
    runWorkflow Nothing _ = 0
    runWorkflow (Just part) (step:steps) = case step of 
      Accept -> product . map len . HashMap.elems $ part
      Reject -> 0
      Go label -> runWorkflow (Just part) (go label)
      MaybeGo (varIndex, sign, value, workflowLabel) ->
        runWorkflow truthy (go workflowLabel) + runWorkflow falsey steps 
        where      
          (falsey, truthy) = split (fromJust $ HashMap.lookup varIndex part) value sign 

          split :: Slice -> Int -> String -> (Maybe Part, Maybe Part)
          split (start, end) middle "<" = swap $ split (start, end) (middle - 1) ">"
          split (start, end) middle ">"
            | middle <= start = (Nothing, Just part)
            | end <= middle = (Just part, Nothing)
            | otherwise = (Just (HashMap.insert varIndex (start, middle) part), Just (HashMap.insert varIndex (middle + 1, end) part))


len :: Slice -> Int
len (a, b) = b - a + 1

swap :: (a, a) -> (a, a)
swap (a,b) = (b,a)

