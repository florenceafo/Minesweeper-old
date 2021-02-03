import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.Maybe
import Game
import Grid
import RunGame
--import Main 

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas         <- mkCanvas canWidth canHeight   -- The drawing area
     

     --getBody window #+ [column [pure canvas,pure formula,pure draw, pure differentiate, pure slider]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     --pure input # set style [("fontSize","14pt")]
     setCanvas 0.04 canvas

setCanvas :: Double -> Element -> UI()
setCanvas zoomDouble canvas = 
  do 
    -- Prints the current canvas size on the UI
    set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
    UI.fillText ("browser") (100,100) canvas
