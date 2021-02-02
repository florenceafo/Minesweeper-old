module Main where



import Grid
import Game
import RunGame
import System.Random

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main :: IO ()
main = runGame implementation

data Interface = Interface
  { iAllBlankGrid    :: Int -> Grid
  , iUpdateWithBombs :: Grid -> [(Int, Int)] -> Grid
  , iScore           :: Grid -> Int
  , iGameOver        :: Grid -> Bool
  -- , iWins         :: Grid -> Bool
  }

-- runGame :: Interface -> IO()
-- runGame i = do
--   putStrLn "Welcome to a new game of minesweeper"
--   g <- newStdGen
--   -- gameLoop i (iAllBlankGrid 5)
--   print "a"



-- gameLoop :: Interface -> Grid -> IO()
-- gameLoop i grid = do
--   putStrLn "In game loop"


