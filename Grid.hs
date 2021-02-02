module Grid where

data Grid = Grid [Row]
  deriving (Show, Eq, Read)
type Row = [Cell] 
data Cell = Blank AdjBombs Clicked | Bomb Clicked
  deriving (Show, Eq, Read)
type AdjBombs = Int -- number of bombs to above, below or either side
type Flag = Bool -- has the user flagged the cell (right click)
type Clicked = Bool -- has the user clicked on the cell (left click)
