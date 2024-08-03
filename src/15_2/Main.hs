module Main (main, hash) where
import Data.Char (ord)
import Data.List.Split (splitOn, split, oneOf)

main :: IO ()
main = do
  contents <- readFile "src/15_2/input_test.txt"
  print . map parse . splitOn "," $ contents

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
