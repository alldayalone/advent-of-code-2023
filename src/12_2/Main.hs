{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}

module Main (main, solve) where
import Data.List.Split (splitOn,split)
import Data.MemoTrie
import GHC.Generics (Generic) 

main :: IO ()
main = do
  contents <- readFile "src/12_2/input.txt"
  mapM_ print . map (solve' . parse) . lines $ contents

parse :: String -> (String, [Int])
parse line = (unfoldedStrings, unfoldedNumbers)
  where 
    (hotStringsStr : lengthStr : _) = splitOn " " line
    lengths = map read $ splitOn "," lengthStr
    unfoldedStrings = unfoldTheRecords hotStringsStr "?" 5
    unfoldedNumbers = unfoldTheRecords lengths [] 5

solve' :: (String, [Int]) -> Int
solve' (s, i) = solveMemoized (Node (s, i, Nothing))
newtype Node = Node (String, [Int], Maybe Char)
  deriving (Generic) 

solve :: Node -> Int
solve (Node ([],      [], _       )) = 1
solve (Node ([],       _, _       )) = 0
solve (Node ('.':s,    _, Just '#')) = 0
solve (Node ('.':s,   i , _       )) = solve (Node (s, i,      Nothing ))
solve (Node ('#':s,   _ , Just '.')) = 0
solve (Node ('#':s,   [], _       )) = 0
solve (Node ('#':s, 1:is, _       )) = solve (Node (s, is,     Just '.'))
solve (Node ('#':s, x:is, _       )) = solve (Node (s, x-1:is, Just '#')) 
solve (Node ('?':s,    i, Just '#')) = solve (Node ('#':s, i,  Nothing ))
solve (Node ('?':s,    i, Just '.')) = solve (Node ('.':s, i,  Nothing )) 
solve (Node ('?':s,    i, Nothing )) = solve (Node ('.':s, i,  Nothing )) + solve (Node ('#':s,i, Nothing))

instance HasTrie Node where
  newtype (Node :->: b) = NodeTrie { unNodeTrie :: Reg Node :->: b } 
  trie      = trieGeneric NodeTrie 
  untrie    = untrieGeneric unNodeTrie
  enumerate = enumerateGeneric unNodeTrie

solveMemoized :: Node -> Int
solveMemoized = memo solve

unfoldTheRecords :: [a] -> [a] -> Int -> [a]
unfoldTheRecords x _ 1 = x
unfoldTheRecords x separator a = x ++ separator ++ unfoldTheRecords x separator (a-1)

count :: (Foldable t, Eq a, Num b) => a -> t a -> b
count a = foldr (\x sum -> if x == a then sum+1 else sum) 0
