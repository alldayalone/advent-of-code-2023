{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Data.Time
import Text.Show.Pretty (ppShowList)
import Data.List (intercalate)
import Data.HashMap.Strict as HashMap (HashMap, fromList, insert, lookup, empty, adjust, elems)
import Data.Maybe (fromJust)
import Data.Char

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
solve _ = tick [] initState [initSignal]

initSignal :: Signal
initSignal = Signal { from = "button", signalKind = Low, to = "broadcaster" }
initState :: State
initState = HashMap.fromList 
  [ ("broadcaster", Broadcast { name = "broadcaster", dests = ["a", "b", "c"] })
  , ("a", FlipFlop { name = "a",  dest = "b", activated = False}) 
  , ("b", FlipFlop { name = "b",  dest = "c", activated = False}) 
  , ("c", FlipFlop { name = "c",  dest = "inv", activated = False})
  , ("inv", Conjunction { name = "inv",  dest = "a", memory = HashMap.fromList [("a", Low), ("b", Low), ("c", Low)] }) ]

data SignalKind = High | Low 
  deriving (Show, Eq)

data Signal = Signal 
  { from :: String,
    to :: String,
    signalKind :: SignalKind }
instance Show Signal where
  show (Signal { from, to, signalKind }) = from ++ " -" ++ (map toLower . show) signalKind ++ "-> " ++ to

type ConjunctionMemory = HashMap String SignalKind
data Module = 
  Broadcast { name :: String, dests :: [String] }
  | FlipFlop { name :: String, dest :: String, activated :: Bool }
  | Conjunction { name :: String, dest :: String, memory :: ConjunctionMemory }
instance Show Module where
  show (Broadcast { name, dests }) = name ++ " -> " ++ intercalate ", " dests
  show (FlipFlop { name, dest }) = "%" ++ name ++ " -> " ++ dest
  show (Conjunction { name, dest }) = "&" ++ name ++ " -> " ++ dest

type Log = [Signal]
type State = HashMap String Module
type Queue = [Signal]

tick :: Log -> State -> Queue -> Log
tick log state [] = log
tick log state (signal:queue) = tick (log ++ [signal]) newState (queue ++ newSignals)
  where 
    newState = HashMap.adjust stateUpdater signal.to state
    mod = fromJust $ HashMap.lookup signal.to newState
    newSignals = case mod of 
      Broadcast { .. } -> [ Signal { from=signal.to, to = dest, signalKind=signal.signalKind } | dest <- dests ]
      FlipFlop { .. } -> [ Signal {from = signal.to, to = dest, signalKind = flipFlopSignal activated} | signal.signalKind == Low ]
      Conjunction {..} -> [ Signal {from = signal.to, to = dest, signalKind = conjunctionSignal memory}]

    stateUpdater :: Module -> Module
    stateUpdater m@(FlipFlop {..}) = m{ activated = not activated }
    stateUpdater m@(Conjunction{..}) = m{memory = HashMap.insert signal.from signal.signalKind memory}
    stateUpdater m = m

flipFlopSignal :: Bool -> SignalKind
flipFlopSignal True = High
flipFlopSignal False = Low

conjunctionSignal :: ConjunctionMemory -> SignalKind
conjunctionSignal memory = if all (== High) (HashMap.elems memory) then Low else High