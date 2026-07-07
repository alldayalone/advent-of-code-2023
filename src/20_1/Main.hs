module Main (main) where
import Data.Time
import Text.Show.Pretty (ppShowList)

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/20_1/input_test.txt"
  writeFile ("src/20_1/output" ++ show utcNow ++ ".txt") . ppShowList . solve . parse $ contents

type Input = String
type Output = Log

parse :: String -> Input
parse = id

solve :: Input -> Output
solve _ = replicate 10 LogEntry { from = "broadcaster", signal = LOW, to = "a" }


data Signal = HIGH | LOW 
  deriving Show

data LogEntry = LogEntry 
  { from :: String,
    to :: String,
    signal :: Signal }
instance Show LogEntry where
  show (LogEntry { from = from, to = to, signal = signal}) = from ++ " -" ++ show signal ++ "-> " ++ to


type Log = [LogEntry]
type State = String
type Queue = String

tick :: Log -> State -> Queue -> Log
tick log state queue = log