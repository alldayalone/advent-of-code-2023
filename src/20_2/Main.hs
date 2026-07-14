{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Data.Time
import Text.Show.Pretty (ppShowList, ppShow)
import Data.List (intercalate)
import Data.HashMap.Strict as HashMap (HashMap, lookupDefault, keys, elems, fromList, insert, insertWith, lookup, empty, adjust, elems)
import Data.Maybe (fromMaybe, fromJust)
import Data.Char
import Text.Regex.TDFA     
import Data.List.Split (splitOn)
import Control.Arrow ((&&&), (***), first, second)

main :: IO ()
main = do
  utcNow   <- getCurrentTime
  contents <- readFile "src/20_2/input.txt"
  writeFile ("src/20_2/output" ++ show utcNow ++ ".txt") . ppShowList . snd . start 1 ([], []) . parse $ contents

type ActivationMap = HashMap String Int
type Input = (State, ActivationMap)
type Output = Int

parse :: String -> Input
parse str = (fmap addConjunctionMemory state, activationMap)
  where
    state = HashMap.fromList . map parseLine . lines $ str
    addConjunctionMemory m@(Conjunction {}) = m{ memory }
      where
        memory = HashMap.fromList . map ((id &&& const Low) . name) . filter (elem m.name . dests) $ HashMap.elems state
    addConjunctionMemory m = m
    activationMap = HashMap.fromList . map (name &&& const 0). filter f . HashMap.elems $ state
      where
        f FlipFlop {} = True
        f _ = False

parseLine :: String -> (String, Module)
parseLine s = case moduleType of
  "" -> (name, Broadcast { name, dests })
  "%" -> (name, FlipFlop { name, dests, activated = False })
  "&" -> (name, Conjunction { name, dests, memory = HashMap.empty })
  where
    regex = "([%&]?)([a-z]+) -> ([a-z, ]+)$"
    (_, _, _, groups) = s =~ regex :: (String, String, String, [String])
    (moduleType:name:destsStr:_) = groups
    dests = splitOn ", " destsStr


countResult :: String -> (State, ActivationMap) -> [Int]
countResult ogName (state, activationMap) = case mod of
  FlipFlop { name } -> [fromJust . HashMap.lookup name $ activationMap]
  Conjunction { name } -> concat . fmap (\k -> countResult k (state, activationMap)) . HashMap.keys . memory . fromJust . HashMap.lookup name $ state
  _ -> [1] 
  where
    mod = HashMap.lookupDefault Undefined ogName state 

start :: Int -> ([State], [Log]) -> Input -> (State, ActivationMap)
-- start 10000 (states, _) _ = fmap stateToStr states
--   where
--     stateToStr :: State -> String
--     stateToStr = concatMap modToStr . HashMap.elems
--     modToStr (Broadcast { name, dests }) = "b"
--     modToStr (FlipFlop { activated }) = if activated then "1" else "0"
--     modToStr (Conjunction { name, dests }) = "&"
start x (states, logs) (state, activationMap) = if activationMapFull then (newState, activationMap) else start (x+1) (states ++ [state], logs ++ [log]) (newState, newActivationMap)
  where
    (log, newState) = tick ([], state) [initSignal]
    matchesTarget (Signal { to="rx", signalKind=Low}) = True
    matchesTarget _                 = False

    newActivationMap = foldr upd activationMap . filter f . HashMap.elems $ newState
      where
        f :: Module -> Bool
        f FlipFlop { activated } = activated 
        f _ = False

    upd :: Module -> HashMap String Int -> HashMap String Int
    upd mod = HashMap.insertWith f mod.name x
      where f new old = if old == 0 then new else old

    activationMapFull = all (>0) $ HashMap.elems activationMap
   

cont :: [Log] -> [(Int, Int)]
cont = map countLog

countTotal :: [(Int, Int)] -> Int
countTotal = uncurry (*) . (sum *** sum) . unzip . take 1000 . cycle

countLog :: Log -> (Int, Int)
countLog = foldr countHighLow (0,0)

mulTuple = uncurry (*)


countHighLow :: Signal -> (Int, Int) -> (Int, Int)
countHighLow (Signal { signalKind = Low }) = first (+1)
countHighLow (Signal { signalKind = High }) = second (+1)

initSignal :: Signal
initSignal = Signal { from = "button", signalKind = Low, to = "broadcaster" }

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
  | FlipFlop { name :: String, dests :: [String], activated :: Bool }
  | Conjunction { name :: String, dests :: [String], memory :: ConjunctionMemory }
  | Undefined
  deriving (Eq)
instance Show Module where
  show (Broadcast { name, dests }) = name ++ " -> " ++ intercalate ", " dests
  show (FlipFlop { name, dests }) = "%" ++ name ++ " -> " ++ intercalate ", " dests
  show (Conjunction { name, dests }) = "&" ++ name ++ " -> " ++ intercalate ", " dests

type Log = [Signal]
type State = HashMap String Module
type Queue = [Signal]

tick :: (Log, State) -> Queue -> (Log, State)
tick (log, state) [] = (log, state)
tick (log, state) (signal:queue) = tick (log ++ [signal], newState) (queue ++ newSignals)
  where 
    newState = HashMap.adjust stateUpdater signal.to state
    mod = fromMaybe Undefined (HashMap.lookup signal.to newState)
    newSignals = case mod of 
      Broadcast { .. } -> [ Signal { from=signal.to, to = dest, signalKind=signal.signalKind } | dest <- dests ]
      FlipFlop { .. } -> [ Signal {from = signal.to, to = dest, signalKind = flipFlopSignal activated} | signal.signalKind == Low, dest <- dests]
      Conjunction {..} -> [ Signal {from = signal.to, to = dest, signalKind = conjunctionSignal memory} | dest <- dests ]
      Undefined -> []

    stateUpdater :: Module -> Module
    stateUpdater m@(FlipFlop {..}) = if signal.signalKind == High then m else m{ activated = not activated }
    stateUpdater m@(Conjunction{..}) = m{memory = HashMap.insert signal.from signal.signalKind memory}
    stateUpdater m = m

flipFlopSignal :: Bool -> SignalKind
flipFlopSignal True = High
flipFlopSignal False = Low

conjunctionSignal :: ConjunctionMemory -> SignalKind
conjunctionSignal memory = if all (== High) (HashMap.elems memory) then Low else High