module Interface
(interface) 
where

import qualified Graphics.UI.Threepenny      as UI
import Graphics.UI.Threepenny.Core
    ( defaultConfig,
      (#),
      (#+),
      (#.),
      column,
      element,
      getBody,
      on,
      set,
      string,
      style,
      title,
      delete,
      startGUI,
      Config(jsStatic),
      ReadWriteAttr(set'),
      UI,
      Window )
import Data.IORef
import Control.Monad
import Fem
    ( beforeResultat, forInterface, listLiaison, resultat, zoomPoint )
import Example2
import Operations (findIndex)

{-----------------------------------------------------------------------------
    Threepenny
    Hello world!
------------------------------------------------------------------------------}

-- imports


interface = do
    startGUI defaultConfig
        { 
            jsStatic = Just "static"
        } setup

canvasSize = 400

setup :: Window -> UI ()
setup window = do
    return window # set title "Canvas - Examples"

    UI.addStyleSheet window "style.css"

    canvas <- UI.canvas
        # set UI.height (2*canvasSize)
        # set UI.width  (3*canvasSize)
        # set style [("border", "solid black 1px"), ("background", "#eee")]


    drawRects <- UI.button #+ [string "Add some rectangles."]
    drawTriangleForce  <- UI.button #+ [string "Apply force on triangle"]
    drawTriangle <- UI.button #+ [string "Add triangle."]
    drawGraph <- UI.button #+ [string "Add graph"]
    drawGraphForce <- UI.button #+ [string "Add force on graph"]
    drawPie   <- UI.button #+ [string "Must have pie!"]
    clear     <- UI.button #+ [string "Clear the canvas."]

    getBody window #+
        [ UI.div #. "wrapper" #+
            [UI.div #. "canvas_div" #+ [element canvas]
            , UI.div #. "buttons_div " #+
                    [ column [element canvas]
                    , element drawTriangle
                    , element drawGraph
                    , column[element clear]
                    ]
            ]
        ]
        
              

    on UI.click clear $ const $ do
        canvas # UI.clearCanvas
        delete drawTriangleForce
        delete drawGraphForce
       
    -- draw a pie chart
    on UI.click drawPie $ const $ do
        let
            center = (fromIntegral canvasSize / 2, fromIntegral (canvasSize+30) / 2)
            radius = 100

            drawSlice start end color = do
                canvas # set' UI.fillStyle (UI.htmlColor color)
                canvas # set' UI.strokeStyle "white"
                canvas # UI.beginPath
                canvas # UI.arc center radius start end
                canvas # UI.lineTo center
                canvas # UI.closePath
                canvas # UI.fill
                canvas # UI.stroke

            radian angle = angle * pi / 180

            normalizeAngles xs = map (\(x,y) -> (360 * x/total,y)) xs
                where total = sum $ map fst xs

            pieData = normalizeAngles
                [ (100, "#1f77b4")
                , (45, "#ff7f0e")
                , (80, "#2ca02c")
                , (10, "#d62728")
                , (105,"#9467bd")
                , (20, "#8c564b")
                ]

        UI.timestamp -- measure drawing performance for fun
        foldM (\start (delta, col) -> do
            let end = start+delta
            drawSlice (radian start) (radian end) col
            return end) 0 pieData
        UI.timestamp


    -- draw some rectangles
    on UI.click drawRects $ const $ do
        let rects = [ (20 , 130, 15, 120, "teal")
                    , (345, 110, 15, 90, "lightblue")
                    , (220, 360, 95, 15, "teal")
                    ]

        forM_ rects $ \(x,y,w,h,color) -> do
            canvas # set' UI.fillStyle (UI.htmlColor color)
            canvas # UI.fillRect (x,y) w h

    -- draw triangle force
    on UI.click drawTriangleForce $ const $ do
        canvas # set' UI.strokeStyle "red"
        canvas # UI.beginPath
        forM_ listLiaison $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface resultat)) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface resultat)) canvasSize)
        canvas # UI.stroke
        

    -- draw the triangle
    on UI.click drawTriangle $ const $ do
        getBody window #+ [element drawTriangleForce]
        canvas # set' UI.strokeStyle "blue"
        canvas # UI.beginPath
        forM_ listLiaison $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface beforeResultat)) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface beforeResultat)) canvasSize)
        canvas # UI.stroke
    
    on UI.click drawGraph $ const $ do
        getBody window #+ [element drawGraphForce]
        canvas # set' UI.strokeStyle "green"
        canvas # UI.beginPath
        forM_ listLiaisonex2 $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (concat listPointex2))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (concat listPointex2))) canvasSize)
        canvas # UI.stroke

    on UI.click drawGraphForce $ const $ do
        canvas # set' UI.strokeStyle "red"
        canvas # UI.beginPath
        forM_ listLiaisonex2 $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface resultat2)) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface resultat2)) canvasSize)
        canvas # UI.stroke