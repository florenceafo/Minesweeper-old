module Game where

import Test.QuickCheck
import Data.List

import System.Random


data Grid = Grid [Row]
  deriving (Show, Eq, Read)
type Row = [Cell] 
data Cell = Blank AdjBombs Clicked | Bomb
  deriving (Show, Eq, Read)
type AdjBombs = Int -- number of bombs to above, below or either side
type Flag = Bool -- has the user flagged the cell (right click)
type Clicked = Bool -- has the user clicked on the cell (left click)

  
-- turns the Grid into a nested list
rows :: Grid -> [Row]
rows (Grid g) = g

-- example Grids for testing
ex1, ex2, ex3, ex4 :: Grid
ex1 = Grid  [
              [Blank 0 False , Blank 1 False , Blank 0 False ],
              [Blank 2 False , Bomb, Blank 3 False ],
              [Blank 0 False , Blank 4 False , Blank 0 False ]
            ]

ex2 =  Grid [
  [Blank 0 True,Blank 0 True,Bomb,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Bomb,Blank 0 True],
  [Bomb,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Bomb,Blank 0 True,Blank 0 True,Blank 0 True],
  [Bomb,Blank 0 True,Bomb,Blank 0 True,Blank 0 True,Bomb,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Bomb,Bomb,Blank 0 True,Blank 0 True,Blank 0 True,Bomb,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Bomb,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Bomb,Blank 0 True,Blank 0 True,Bomb,Blank 0 True,Bomb,Blank 0 True,Blank 0 True],
  [Blank 0 True,Bomb,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Bomb],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Bomb,Bomb,Bomb,Bomb,Blank 0 True]
  ]

ex3 =  Grid [
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 1 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 1 True,Bomb,Blank 1 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 1 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
  [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True]
  ]

ex4 = Grid  [
              [Blank 0 False , Blank 0 False , Blank 0 False ],
              [Blank 0 False , Bomb, Blank 0 False ],
              [Blank 0 False , Blank 0 False , Blank 0 False ]
            ]


--- creates an arbitrary Grid
instance Arbitrary Grid where
  arbitrary = fmap Grid (vectorOf 10 (vectorOf 10 cell))

-- generates an arbitrary cell in a Minesweeper grid
cell :: Gen Cell
cell = frequency [(3, emptyCell), (1, bomb)]
  where 
    emptyCell = elements [Blank 0 False]
    bomb = return Bomb

-- creates a blank Grid with the height and width of the given Integer
-- Implements a maximum size of 10 x 10 and a minimum size of 5 x 5
allBlankGrid :: Int -> Grid
allBlankGrid i = Grid [[Blank 0 False | _ <- [1..j]] | _ <- [1..j]]
  where 
    j 
     | i < 5 = 5
     | i > 10 = 10
     | otherwise = i

-- checks if the blank grid generated contains all blanks, 
-- with no bombs or adjacency numbers
prop_allBlankGrid :: Grid -> Bool
prop_allBlankGrid grid = all (== Blank 0 False) $ concat $ rows grid

-- prints the Grid in the console
showGrid :: Grid -> IO()
showGrid (Grid g) = mapM_ (putStrLn.unwords.map showCell) g
  where 
    --Decides how to print cells
    showCell :: Cell -> String
    showCell (Blank i True) = show i
    showCell _              = "_"


-- property to test if the grid is valid
-- if the Grid is square
prop_validGrid :: Grid -> Bool
prop_validGrid grid = isSquare grid && withinRange grid

  where 
    g' = rows grid

    withinRange :: Grid -> Bool
    withinRange g = length g' >= 5 && length g' <= 10

    isSquare :: Grid -> Bool
    isSquare g =  all (==lengthCol) (lengthAllRows g')
      where 
        lengthCol = length g'
        lengthAllRows g' = [length x | x <- g']

-- returns a nested list of the locations of all bombs
detectAllBombs :: Grid -> [[Int]] 
detectAllBombs grid = [ elemIndices Bomb x | x <- grid']  
  where 
    grid' = rows grid
    update row 0 = Bomb : add (row !! 1 ) : drop 8 row
    add Bomb = Bomb
    add (Blank n c) = Blank (n+1) c 