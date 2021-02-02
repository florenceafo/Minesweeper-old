module Game where

import Test.QuickCheck
import Data.List

import System.Random
import RunGame
import Grid



  
-- turns the Grid into a nested list
rows :: Grid -> [Row]
rows (Grid g) = g

-- example Grids for testing
ex3, ex4 :: Grid
-- ex1 = Grid  [
--               [Blank 0 False , Blank 1 False , Blank 0 False ],
--               [Blank 2 False , Bomb False, Blank 3 False ],
--               [Blank 0 False , Blank 4 False , Blank 0 False ]
--             ]

-- ex2 =  Grid [
--   [Blank 0 True,Blank 0 True,Bomb False,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Bomb False,Blank 0 True],
--   [Bomb False,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Bomb False,Blank 0 True,Blank 0 True,Blank 0 True],
--   [Bomb False,Blank 0 True,Bomb False,Blank 0 True,Blank 0 True,Bomb False,Blank 0 True,Blank 0 True,Blank 0 True],
--   [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
--   [Bomb False,Bomb False,Blank 0 True,Blank 0 True,Blank 0 True,Bomb False,Blank 0 True,Blank 0 True,Blank 0 True],
--   [Blank 0 True,Bomb False,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True],
--   [Blank 0 True,Bomb False,Blank 0 True,Blank 0 True,Bomb False,Blank 0 True,Bomb False,Blank 0 True,Blank 0 True],
--   [Blank 0 True,Bomb False,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Bomb False],
--   [Blank 0 True,Blank 0 True,Blank 0 True,Blank 0 True,Bomb False,Bomb False,Bomb False,Bomb False,Blank 0 True]
--   ]

ex3 = Grid [
            [Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False],[
              Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False,Bomb False False,Blank 0 False False],[
              Blank 0 False False,Bomb False False,Bomb False False,Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False,Bomb False False,Blank 0 False False,Blank 0 False False],[
              Blank 0 False True,Blank 0 False False,Bomb False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Bomb False False,Bomb False False,Blank 0 False False],[
              Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False],[
              Bomb False False,Bomb False False,Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Bomb False False],[
              Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False,Bomb False False],[
              Blank 0 False False,Bomb False False,Bomb False False,Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False,Bomb False False,Blank 0 False False,Blank 0 False False],[
              Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False,Blank 0 False False,Bomb False False,Blank 0 False False],
              [Blank 0 False False,Blank 0 False False,Bomb False False,Bomb False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Blank 0 False False,Bomb False False]
          ]



ex4 = Grid  [
              [Blank 0 False False , Blank 0 False False , Blank 0 False False ],
              [Blank 0 False False , Bomb False False, Blank 0 False False ],
              [Blank 0 False False , Blank 0 False False , Blank 0 False False ]
            ]


--- creates an arbitrary Grid
instance Arbitrary Grid where
  arbitrary = fmap Grid (vectorOf 10 (vectorOf 10 cell))

-- generates an arbitrary cell in a Minesweeper grid
cell :: Gen Cell
cell = frequency [(3, emptyCell), (1, bomb)]
  where 
    emptyCell = elements [Blank 0 False False]
    bomb = return (Bomb False False)

-- creates a blank Grid with the height and width of the given Integer
-- Implements a maximum size of 10 x 10 and a minimum size of 5 x 5
allBlankGrid :: Int -> Grid
allBlankGrid i = Grid [[Blank 0 False False | _ <- [1..j]] | _ <- [1..j]]
  where 
    j 
     | i < 5 = 5
     | i > 10 = 10
     | otherwise = i

-- checks if the blank grid generated contains all blanks, 
-- with no bombs or adjacency numbers
prop_allBlankGrid :: Grid -> Bool
prop_allBlankGrid grid = all (== Blank 0 False False) $ concat $ rows grid

-- prints the Grid in the console
showGrid :: Grid -> IO()
showGrid (Grid g) = mapM_ (putStrLn.unwords.map showCell) g
  where 
    --Decides how to print cells
    showCell :: Cell -> String
    showCell (Blank _ _ True) = "⚐"
    showCell (Bomb _ True) = "⚐"
    showCell (Blank i True f) = show i
    showCell _              = "_"

-- prints the Grid in the console
showGrid' :: Grid -> IO()
showGrid' (Grid g) = mapM_ (putStrLn.unwords.map showCell) g
  where 
    --Decides how to print cells
    showCell :: Cell -> String
    showCell (Blank i _ _) = show i
    showCell (Bomb _ _)            = "*"

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
-- detectAllBombs :: Grid -> [(Int, Int)]
-- detectAllBombs grid = concat $ 
--   filter (not . null) 
--   [ zip (elemIndices (Bomb False) x ) [0..length grid' -1]  |  x <- grid']
--   where 
--     grid' = rows grid
 
detectAllBombs :: Grid -> [(Int, Int)]
detectAllBombs grid = concat [ ([(x, y) | grid' !! y !! x == Bomb False False]) | x <- [0..8], y <- [0..8]]  
  where 
    grid' = rows grid
    -- update row 0 = Bomb False : add (row !! 1 ) : drop 8 row
    -- add (Bomb c ) = Bomb c
    -- add (Blank n c) = Blank (n+1) c 

prop_detectAllBombs :: Grid -> Bool
prop_detectAllBombs grid = undefined 

-- updates adjacent cells with an increased count
-- only attempts to change bombs, not surrounding cells
-- updateWithBombs :: Grid -> [(Int, Int)] -> Grid
updateWithBombs :: Grid -> [(Int, Int)] -> Grid
updateWithBombs grid [] = grid
updateWithBombs grid ((col, row):rest) = updateWithBombs 
  (removeBorder $ Grid $ startRow ++ [above, middle, below] ++ endRow ) 
  rest
  where 
    grid' = rows (addBorder grid)
    -- row before the row with the current bomb
    startRow = take (row) grid' 
    -- row after the row with the current bomb
    endRow = drop (row + 3) grid'
    -- update the row with the current bomb
    above = addBombsNumbers (grid' !! row) (col+1)
    middle = addBombsNumbers (grid' !! (row+1)) (col+1)
    below = addBombsNumbers (grid' !! (row+2)) (col+1)
    addBombsNumbers r colIndex = take (colIndex-1) r ++ 
             [increaseBombCount $ r !! (colIndex-1),
             increaseBombCount $ r !! colIndex,
             increaseBombCount $ r !! (colIndex+1)]  
             ++ drop (colIndex + 2) r 

-- Update the Blank cell with the number of bombs surrounding it
-- Ignores cells that are Bombs
increaseBombCount :: Cell -> Cell
increaseBombCount (Blank i c f) = Blank (i+1) c f
increaseBombCount  (Bomb c f)     = Bomb c f

addBorder :: Grid -> Grid
addBorder grid = Grid $ [replicate (l+2) (Blank 0 False False)] ++ 
                  [ [(Blank 0 False False)]++x++[(Blank 0 False False)] | x <- grid']
                  ++ [replicate (l+2) (Blank 0 False False)]
  where
    grid' = rows grid
    l = length grid'

removeBorder :: Grid -> Grid
removeBorder grid = Grid $ [ (tail . init) x | x <- (tail . init) grid']
  where
    grid' = rows grid
    l = length grid'



-- adds bombs to a blank grid, given an integer
addBombs :: Int -> Grid -> IO Grid
addBombs 0 grid = return grid
addBombs i grid = do 
  g <- newStdGen 
  let (grid', seed) = insertBombs g (rows grid)
  addBombs (i-1)  grid'

-- given a grid and an integer, returns a Grid with bombs inserted
-- to be added: integer is the ratio of Blanks to Bombs, with Bombs always being one
-- insertBombs :: Grid -> Int -> Grid
-- insertBombs :: Grid -> Int -> IO Int
insertBombs :: RandomGen g => g -> [Row]-> (Grid, g)
insertBombs g grid = do
  let (r,s) = getRandomNumber g n
  let pre = take r $ concat grid
  let post = drop (r+1) $ concat grid
  let newGrid = Grid $ splitIntoSublist l ( pre ++ [Bomb False False] ++ post )
  (newGrid, s) 
  where 
    n = length ( concat grid) - 1
    l = length grid
    splitIntoSublist n list 
      | n <= 0 || null list = []
      | otherwise = take n list : splitIntoSublist n (drop n list)

getRandomNumber :: RandomGen g => g -> Int -> (Int, g)
getRandomNumber g n = randomR (0,n::Int) g

-- updates a cell as clicked on
clicked :: Cell -> Cell 
clicked (Blank i c True) = Blank i c True
clicked (Bomb c True) = Bomb c True
clicked (Blank i False False) = Blank i True False
clicked (Bomb False False)    = Bomb True False
clicked cell            = cell

flagged :: Cell -> Cell 
flagged (Blank i c False) = Blank i c True
flagged (Bomb c False)    = Bomb c True 
flagged cell              = cell

gameOver :: Grid -> Bool 
gameOver grid = elem (Bomb True False) (concat grid')
  where grid' = rows grid

score :: Grid -> Int 
score grid = 0

implementation = Interface
  { iAllBlankGrid    = allBlankGrid,
    iAddBombs        = addBombs,
    iScore           = score,  
    iGameOver        = gameOver,
    iShowGrid        = showGrid',
    iUpdateWithBombs = updateWithBombs,
    iDetectAllBombs  = detectAllBombs
  }


