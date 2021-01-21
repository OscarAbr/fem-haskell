module Interface
(interface) 
where
import qualified Graphics.UI.Threepenny      as UI
import Graphics.UI.Threepenny.Core
    (get, value, attr,  defaultConfig,
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

import Control.Monad
import Fem
    ( beforeResultat, forInterface, listLiaison, resultat, zoomPoint )
import Example2
import ExampleMaillage
import Operations (findIndex)
import ForceSlide
import Interpret

{-----------------------------------------------------------------------------
    Threepenny
    Hello world!
    Notre interface utilise Threepenny on a laissé quelques exemples dont on s'est inspiré (pie chart, some rectangles...)
------------------------------------------------------------------------------}

-- imports
type Point = (Double,Double)

interface :: IO ()
interface  = do
    content <- readFile ("test.txt")
    startGUI defaultConfig
        { 
            jsStatic = Just "static"
        } (setup content)
    putStrLn "setup done"
canvasSize = 400


setup :: String -> Window -> UI ()
setup content window = do
    return window # set title "FEM"

    canvas <- UI.canvas
        # set UI.height (2*canvasSize)
        # set UI.width  (3*canvasSize)
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    drawTriangleForce  <- UI.button #+ [string "Apply force on triangle"]
    drawTriangle <- UI.button #+ [string "Add triangle."]
    
    drawMeshedCircle3 <- UI.button #+ [string "Add car."]
    drawMeshedCircleForce3 <- UI.button #+ [string "Crash the car ! (and your CPU ...) "]
    
    sliderMesh  <- UI.input # set UI.type_ "range"
                           # set (attr "min") (show 0)
                           # set (attr "max") (show 5)
                           # set value (show 0)
    sliderForce  <- UI.input # set UI.type_ "range"
                           # set (attr "min") (show 0)
                           # set (attr "max") (show 20)
                           # set value (show 0)
    meshForce <- UI.button #+ [string "Apply force on mesh"]
    drawGraph <- UI.button #+ [string "Add graph"]
    drawGraphForce <- UI.button #+ [string "Add force on graph"]
    clear     <- UI.button #+ [string "Clear the canvas."]
    --button   <- UI.button #+ [string "fetch points."]
    readShape <- UI.button #+ [string "Read Shape"] 

    getBody window #+
        [ UI.div #. "wrapper" #+
            [UI.div #. "canvas_div" #+ [element canvas]
            , UI.div #. "buttons_div " #+
                    [ column [element canvas]
                    , element drawTriangle
                    , element drawGraph
                    , element drawMeshedCircle3
                    --, element button
                    , element readShape
                    , column[element clear
                    , element sliderMesh
                    , element sliderForce]
                    
                    ]
            ]
        ]
        
    --on UI.mousedown canvas $ \(x, y) -> do
      --UI.liftIO $ modifyIORef' pointsRef ((x, y) :)
    --  canvas # UI.lineTo (x,y)
    --  canvas # UI.stroke



    on UI.click sliderForce $ const $ do
        canvas # set' UI.strokeStyle "red"
        canvas # UI.beginPath

        inValuef  <- get value sliderForce
        let f = read inValuef :: Int
        inValuen  <- get value sliderMesh
        let n = read inValuen :: Int

        forM_ (listLiaisonsMaillage n) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (resultatTriangleMaillageSlide n f))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (resultatTriangleMaillageSlide n f))) canvasSize)
        canvas # UI.stroke

    on UI.click sliderMesh $ const $ do
        canvas # UI.clearCanvas
        canvas # set' UI.strokeStyle "black"
        canvas # UI.beginPath
        inValue  <- get value sliderMesh
        let n = read inValue :: Int
        forM_ (listLiaisonsMaillage n) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (concat (trianglePointsOnly n)))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (concat (trianglePointsOnly n)))) canvasSize)
        canvas # UI.stroke


    on UI.click clear $ const $ do
        canvas # UI.clearCanvas
        canvas # UI.beginPath
        delete drawTriangleForce
        delete drawGraphForce
       
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
    
    on UI.click drawMeshedCircle3 $ const $ do
        --getBody window #+ [element drawMeshedTriangleForce2]
        getBody window #+ [element drawMeshedCircleForce3]
        canvas # set' UI.strokeStyle "green"
        canvas # UI.beginPath
        forM_ (listLiaisonsCMaillage 5) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (concat (circlePointsOnly 5)))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (concat (circlePointsOnly 5)))) canvasSize)

        canvas # UI.stroke

    on UI.click drawMeshedCircleForce3 $ const $ do
        --getBody window #+ [element drawMeshedTriangleForce2]
        ---getBody window #+ [element drawMeshedCircleForce3]
        canvas # set' UI.strokeStyle "red"
        canvas # UI.beginPath
        forM_ (listLiaisonsCMaillage 5) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (resultatCircleMaillage 5))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (resultatCircleMaillage 5))) canvasSize)
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

    on UI.click readShape $ const $ do
        canvas # set' UI.strokeStyle "green"
        canvas # UI.beginPath
        let shape = read content :: Shape
        forM_ (listLiaisonsShapeMaillage 5 shape) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (concat (shapePointsOnly 5 shape)))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (concat (shapePointsOnly 5 shape)))) canvasSize)
        canvas # UI.stroke