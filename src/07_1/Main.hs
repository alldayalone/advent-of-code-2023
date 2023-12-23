module Main (main, CardRank(..), Hand(..), parseHand, parseCardRank, count) where
import Data.List (sort,sortBy,group)
import Data.Ord (compare)
import Control.Arrow ((&&&))
import Data.Function (on)

main :: IO ()
main = do
  contents <- readFile "src/07_1/input.txt"
  (print . calculateTotal . sortBy (compare `on` fst) . parseInput) contents

parseInput :: String -> [(Hand, Int)]
parseInput = map (parseRow . words) . lines
parseRow :: [String] -> (Hand, Int)
parseRow [x,y] = (parseHand x, read y)
parseRow _ = error "Invalid input"

data CardRank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)
data Hand =
  HighCard [CardRank]
  | OnePair [CardRank]
  | TwoPair [CardRank]
  | ThreeOfAKind [CardRank]
  | FullHouse [CardRank]
  | FourOfAKind [CardRank]
  | FiveOfAKind [CardRank]
  deriving (Show, Eq, Ord)

parseCardRank :: Char -> CardRank
parseCardRank '2' = Two
parseCardRank '3' = Three
parseCardRank '4' = Four
parseCardRank '5' = Five
parseCardRank '6' = Six
parseCardRank '7' = Seven
parseCardRank '8' = Eight
parseCardRank '9' = Nine
parseCardRank 'T' = Ten
parseCardRank 'J' = Jack
parseCardRank 'Q' = Queen
parseCardRank 'K' = King
parseCardRank 'A' = Ace
parseCardRank _ = error "Invalid card rank"

parseHand :: String -> Hand
parseHand handRaw = case count hand of
  [(_,5)] -> FiveOfAKind hand
  [(_,4),(_,1)] -> FourOfAKind hand
  [(_,3),(_,2)] -> FullHouse hand
  [(_,3),(_,1),(_,1)] -> ThreeOfAKind hand
  [(_,2),(_,2),(_,1)] -> TwoPair hand
  [(_,2),(_,1),(_,1),(_,1)] -> OnePair hand
  _ -> HighCard hand
  where hand = map parseCardRank handRaw


count :: Ord a => [a] -> [(a, Int)]
count = sortBy (flip compare `on` snd) . map (head &&& length) . group . sort

calculateTotal :: [(Hand, Int)] -> Int
calculateTotal = sum . zipWith (\rank (_,bid) -> rank * bid) [1..]