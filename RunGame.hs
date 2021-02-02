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
  -- , iWins         :: Grid -> Bool
  }

runGame :: Interface -> IO()
runGame i = do
  putStrLn "Welcome to a new game of minesweeper"
  g <- newStdGen
  putStrLn "Choose a level between 1 (easy) and 5 (hard)"
  input <- getLine 
  let l = (read input)
  currentGrid <- (iAddBombs i l (iAllBlankGrid i 10))
  gameLoop i  currentGrid
  -- print g



gameLoop :: Interface -> Grid -> IO()
gameLoop i grid = do
  putStrLn "In game loop"
  iShowGrid i grid
