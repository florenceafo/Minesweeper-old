module Main where



import Grid
import Game
import RunGame
import System.Random

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main :: IO ()
main = runGame implementation



-- runGame :: Interface -> IO()
-- runGame i = do
--   putStrLn "Welcome to a new game of minesweeper"
--   g <- newStdGen
--   -- gameLoop i (iAllBlankGrid 5)
--   print "a"



-- gameLoop :: Interface -> Grid -> IO()
-- gameLoop i grid = do
--   putStrLn "In game loop"


