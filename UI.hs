import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.Maybe
import Game
import Grid
import RunGame
--import Main 

canWidth,canHeight :: Num a => a
canWidth  = 500
canHeight = 500

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas         <- mkCanvas canWidth canHeight   -- The drawing area
     

     getBody window #+ [column [pure canvas]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     --pure input # set style [("fontSize","14pt")]
     setCanvas canvas

setCanvas :: Element -> UI()
setCanvas canvas = 
  do 
    -- Prints the current canvas size on the UI
    --set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
   -- UI.fillText ("browser") (100,100) canvas
    let l1 = [(50,0), (50,500)]
    let l2 = [(100,0), (100,500)]
    let l3 = [(150,0), (150,500)]
    let l4 = [(200,0), (200,500)]
    let l5 = [(250,0), (250,500)]
    let l6 = [(300,0), (300,500)]
    let l7 = [(350,0), (350,500)]
    let l8 = [(400,0), (400,500)]
    let l9 = [(450,0), (450,500)]
   -- let l10 = [(50,0), (50,500)]
    path "black" l1 canvas
    path "black" l2 canvas
    path "black" l3 canvas
    path "black" l4 canvas
    path "black" l5 canvas
    path "black" l6 canvas
    path "black" l7 canvas
    path "black" l8 canvas
    path "black" l9 canvas
    --path "black" l10 canvas

    let l11 = [(0,50), (500,50)]
    let l12 = [(0,100), (500,100)]
    let l13 = [(0,150), (500,150)]
    let l14 = [(0,200), (500,200)]
    let l15 = [(0,250), (500,250)]     
    let l16 = [(0,300), (500,300)]
    let l17 = [(0,350), (500,350)]
    let l18 = [(0,400), (500,400)]
    let l19 = [(0,450), (500,450)]
    path "black" l11 canvas
    path "black" l12 canvas
    path "black" l13 canvas
    path "black" l14 canvas
    path "black" l15 canvas
    path "black" l16 canvas
    path "black" l17 canvas
    path "black" l18 canvas
    path "black" l19 canvas


-- lineTo :: Point -> Canvas -> UI () 
-- lineTo point canvas