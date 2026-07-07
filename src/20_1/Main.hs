{-# LANGUAGE InstanceSigs #-}
module Main (main) where
import Data.Time
import Text.Show.Pretty (ppShowList)
import Data.List (intercalate)
import Data.HashMap.Strict as HashMap (HashMap, fromList, lookup)
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
  [ ("broadcaster", Module { name = "broadcaster", moduleKind = Broadcast, destinations = ["a", "b", "c"], activated = False })
  , ("a", Module { name = "a", moduleKind = FlipFlop, destinations = ["b"], activated = False}) 
  , ("b", Module { name = "b", moduleKind = FlipFlop, destinations = ["c"], activated = False}) 
  , ("c", Module { name = "c", moduleKind = FlipFlop, destinations = ["inv"], activated = False})
  , ("inv", Module { name = "inv", moduleKind = Conjunction, destinations = ["a"], activated = False}) ]

data SignalKind = High | Low 
  deriving (Show, Eq)

data Signal = Signal 
  { from :: String,
    to :: String,
    signalKind :: SignalKind }
instance Show Signal where
  show (Signal { from = from, to = to, signalKind = signalKind}) = from ++ " -" ++ (map toLower . show) signalKind ++ "-> " ++ to

data ModuleKind = Broadcast | FlipFlop | Conjunction
  deriving (Eq)
instance Show ModuleKind where
  show :: ModuleKind -> String
  show Broadcast = ""
  show FlipFlop = "%"
  show Conjunction = "&"
data Module = Module
  { moduleKind :: ModuleKind,
    name :: String,
    destinations :: [String],
    activated :: Bool }
instance Show Module where
  show (Module { moduleKind = moduleKind, name = name, destinations = destinations }) = show moduleKind ++ name ++ " -> " ++ intercalate ", " destinations

type Log = [Signal]
type State = HashMap String Module
type Queue = [Signal]

tick :: Log -> State -> Queue -> Log
tick log state [] = log
tick log state (signal@(Signal { from=from, to=to, signalKind=signalKind }):queue) = tick (log ++ [signal]) state (queue ++ newSignals)
  where 
    mod@(Module { name=name, destinations=destinations, moduleKind=moduleKind, activated=activated }) = fromJust $ HashMap.lookup to state
    newSignals
      | moduleKind == Broadcast = map (\dest -> Signal { from = to, to = dest, signalKind = signalKind }) destinations
      | moduleKind == FlipFlop && signalKind == High = []
      | moduleKind == FlipFlop && signalKind == Low = map (\dest -> Signal { from = to, to = dest, signalKind = flipFlopSignal activated }) destinations

    newState = state

flipFlopSignal :: Bool -> SignalKind
flipFlopSignal False = High
toggle True = Low