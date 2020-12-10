module Editor where
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad

canvasSize = 400


editor = do
    startGUI defaultConfig
        { 
            jsStatic = Just "static"
        } setup


setup :: Window -> UI ()
setup window = do
  return window # set title "Haskell GUI"

  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set style [("border", "solid black 1px"), ("background", "#eee")]

  button <- UI.button #+ [string "Clear"]

  getBody window #+
    [
    column [element canvas],
    element canvas,
    element button
    ]

  on UI.mousedown canvas $ \(x, y) -> do
    -- Need to create a point x y and add it to a list here
    canvas # set' UI.strokeStyle "blue"
    canvas # UI.lineTo (x,y)
    canvas # UI.stroke
        

  on UI.click button $ const $ do
    -- Need to get the list of points here
    canvas # UI.clearCanvas
    canvas # UI.beginPath

  return ()