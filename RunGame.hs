module RunGame where

import Grid
import System.Random

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

data Interface = Interface
  { iAllBlankGrid    :: Int -> Grid
  , iAddBombs        :: Int -> Grid -> IO Grid
  , iScore           :: Grid -> Int
  , iGameOver        :: Grid -> Bool
  , iShowGrid        :: Grid -> IO()
  , iUpdateWithBombs :: Grid -> [(Int, Int)] -> Grid
  , iDetectAllBombs :: Grid -> [(Int, Int)]
  -- , iWins         :: Grid -> Bool
  }

runGame :: Interface -> IO()
runGame i = do
  putStrLn "Welcome to a new game of minesweeper"
  g <- newStdGen
  putStrLn "Choose a level between 5 (easy) and 10 (hard)"
  input <- getLine 
  let l = (read input)
  currentGrid <- iAddBombs i l (iAllBlankGrid i 10)
  let bombs = iDetectAllBombs i currentGrid
  let updatedGrid = iUpdateWithBombs i currentGrid bombs
  gameLoop i  updatedGrid


gameLoop :: Interface -> Grid -> IO()
gameLoop i grid = do
  iShowGrid i grid
  putStrLn "Enter the coordinates of the cell you wish to click on"
  -- input <- getLine 

  
