module Main (main, hash) where
import Data.Char (ord)
import Data.List.Split (splitOn, split, oneOf)
import Data.Vector as Vector (Vector, (!), (//), replicate, imap, sum)
import Debug.Trace (trace)

main :: IO ()
main = do
  contents <- readFile "src/15_2/input.txt"
  print . focusingPower . foldl applyInstruction emptyLensSeries . map parse . splitOn "," $ contents

type LensSeries = Vector [Lens]
type Lens = (String, Int) -- (label, focal length)

emptyLensSeries :: LensSeries
emptyLensSeries = Vector.replicate 256 []

applyInstruction :: LensSeries -> Instruction  -> LensSeries
-- applyInstruction lensSeries _ | trace (show lensSeries) False = undefined
applyInstruction lensSeries (label, Set, Just focalLength) = lensSeries // [(boxIndex, setLens box (label, focalLength))]
  where
    boxIndex = hash label
    box = lensSeries ! boxIndex
applyInstruction lensSeries (label, Remove, Nothing) = lensSeries // [(boxIndex, removeLens box label)]
  where
    boxIndex = hash label
    box = lensSeries ! boxIndex

setLens :: [Lens] -> Lens -> [Lens]
setLens [] newLens = [newLens]
setLens (lens:box) newLens = if fst lens == fst newLens then newLens:box else lens:setLens box newLens

removeLens :: [Lens] -> String -> [Lens]
removeLens [] _ = []
removeLens (lens:box) label = if fst lens == label then box else lens:removeLens box label

focusingPower :: LensSeries -> Int
focusingPower = Vector.sum . Vector.imap (\i box -> (i + 1) * (Prelude.sum . zipWith (\slot lens -> slot * snd lens) [1..] $ box)) 

type Instruction = (String, Op, Maybe Int) -- (label, operation, focal length)
data Op = Set | Remove deriving (Show)

parse :: String -> Instruction
parse s = case op of
  "=" -> (label, Set, Just (read focalLength))
  "-" -> (label, Remove, Nothing)
  _ -> error "Invalid operation"
  where 
    [label,op,focalLength] = split (oneOf "=-") s

hash :: String -> Int
hash = hash' 0
  where
    hash' :: Int -> String -> Int
    hash' currentValue [] = currentValue
    hash' val (x:xs) = hash' (flip mod 256 . (* 17) . (+ val) . ord $ x) xs

