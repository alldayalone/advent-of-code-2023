module Cell (Cell, empty) where

data Cell = Cell { 
  char  :: Char, -- Possible values: empty space (.), mirrors (/ and \), and splitters (| and -).
  light :: Int -- 0000 each bit contains light direction : north, east, south, west. E.g. 5 = 0101 means east and west 
}

empty :: Char -> Cell
empty char = Cell { char = char, light = 0 }

instance Show Cell where
  show (Cell char light) = show char