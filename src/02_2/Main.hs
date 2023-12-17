import System.IO 
import Data.List.Split (splitOn, splitOneOf)
import Data.List (stripPrefix, isPrefixOf, isSuffixOf)

main = do
  contents <- readFile "src/02_1/input.txt"
  (print . sum . map getPower . map evalGame  . map parseLine . lines) contents

data CubeSet = CubeSet {
  red :: Int,
  green :: Int,
  blue :: Int
} deriving (Show)
data Game = Game {
  id' :: Int,
  cubeSets :: [CubeSet]
} deriving (Show)

getId Game { id' } = id'

getRed CubeSet { red } = red
getBlue CubeSet { green } = green
getGreen CubeSet { blue } = blue
getPower CubeSet { red, green, blue } = red * green * blue

evalGame Game { cubeSets } = CubeSet { red = (maximum . map getRed) cubeSets, blue = (maximum . map getBlue) cubeSets, green = (maximum . map getGreen) cubeSets }

(CubeSet { red = red1, green = green1,  blue = blue1 }) `enoughCubes `(CubeSet { red = red2, green = green2,  blue = blue2 }) = red1 >= red2 && green1 >= green2 && blue1 >= blue2;

parseLine :: [Char] -> Game
parseLine line = Game {
  id' = id,
  cubeSets = cubeSets
}
  where
    (gamePart:setsPart:_) = splitOn ": " line
    id = read (drop 5 gamePart)
    setsList = map (map parseCube .splitOn ", ") (splitOn "; " setsPart)
    cubeSets = map (foldl addCube (CubeSet { red = 0, green = 0, blue = 0 })) setsList

addCube (CubeSet { red = red1, green = green1,  blue = blue1 }) (CubeSet { red = red2, green = green2,  blue = blue2 }) = CubeSet { red = red1+red2, green = green1+green2,  blue = blue1+blue2 }

parseCube x = case color of 
  "red" -> CubeSet { red = read numb, blue = 0, green = 0 }
  "blue" -> CubeSet { red = 0, blue = read numb, green = 0 }
  "green" -> CubeSet { red = 0, blue = 0, green = read numb }
  _ -> CubeSet { red = 0, blue = 0, green = 0 }
  where (numb:color:_) = splitOn " " x

