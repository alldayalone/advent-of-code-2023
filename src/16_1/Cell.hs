module Cell (Cell(..), empty, north, west, east, south, addLight, lightExists) where

import Data.Bits

data Cell = Cell { 
  char  :: Char, -- Possible values: empty space (.), mirrors (/ and \), and splitters (| and -).
  light :: Int -- 0000 each bit contains light direction : north, east, south, west. E.g. 5 = 0101 means east and west 
}


empty :: Char -> Cell
empty char = Cell { char = char, light = 0 }

addLight :: Int -> Cell -> Cell
addLight direction cell = cell { light = light cell .|. direction }
north :: Int
north = 1
east :: Int
east = 2
south :: Int
south = 4
west :: Int
west = 8

lightExists :: Int -> Cell  -> Bool
lightExists direction cell = light cell .&. direction /= 0

instance Show Cell where
  -- show (Cell _ light) = if light == 0 then "." else "#" -- view energized cells
  show (Cell '.' 0) = "."
  show (Cell '.' 1) = "^"
  show (Cell '.' 2) = ">"
  show (Cell '.' 4) = "v"
  show (Cell '.' 8) = "<"
  show (Cell '.' light) = show . popCount $ light
  show (Cell char _) = [char]
