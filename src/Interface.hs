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

interface = do
    startGUI defaultConfig
        { 
            jsStatic = Just "static"
        } setup
    putStrLn "setup done"
canvasSize = 400


setup :: Window -> UI ()
setup window = do
    return window # set title "Canvas - Examples"

    --pointsRef <- UI.liftIO (newIORef [] :: IO (IORef [Point]))
    canvas <- UI.canvas
        # set UI.height (canvasSize)
        # set UI.width  (3*canvasSize)
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    drawRects <- UI.button #+ [string "Add some rectangles."]
    drawTriangleForce  <- UI.button #+ [string "Apply force on triangle"]
    drawCircleForce  <- UI.button #+ [string "Apply force on circle"]
    drawTriangle <- UI.button #+ [string "Add triangle."]
    drawMeshedTriangleForce1  <- UI.button #+ [string "Apply force on meshed (once) triangle"]
    drawMeshedTriangle1 <- UI.button #+ [string "Add meshed(once) triangle."]
    drawMeshedCircle3 <- UI.button #+ [string "Add meshed(3) circle."]
    drawMeshedCircleForce3 <- UI.button #+ [string "Apply force on meshed (3) circle"]
    drawMeshedTriangleForce2  <- UI.button #+ [string "Apply force on meshed (twice) triangle"]
    drawMeshedTriangle2 <- UI.button #+ [string "Add meshed(twice) triangle."]
    sliderMesh  <- UI.input # set UI.type_ "range"
                           # set (attr "min") (show 0)
                           # set (attr "max") (show 5)
                           # set value (show 0)
    sliderForce  <- UI.input # set UI.type_ "range"
                           # set (attr "min") (show 0)
                           # set (attr "max") (show 10)
                           # set value (show 0)
    meshForce <- UI.button #+ [string "Apply force on mesh"]
    drawGraph <- UI.button #+ [string "Add graph"]
    drawGraphForce <- UI.button #+ [string "Add force on graph"]
    drawPie   <- UI.button #+ [string "Must have pie!"]
    clear     <- UI.button #+ [string "Clear the canvas."]
    button    <- UI.button #+ [string "fetch points."]

    getBody window #+
        [ UI.div #. "wrapper" #+
            [UI.div #. "canvas_div" #+ [element canvas]
            , UI.div #. "buttons_div " #+
                    [ column [element canvas]
                    , element drawTriangle
                    , element drawGraph
                    , element drawMeshedCircle3
                    , element drawMeshedTriangle1
                    , element drawMeshedTriangle2
                    , element button
                    , element drawMeshedCircleForce3
                    , column[element clear
                    , element sliderMesh
                    , element sliderForce]
                    
                    ]
            ]
        ]
        
    on UI.mousedown canvas $ \(x, y) -> do
      --UI.liftIO $ modifyIORef' pointsRef ((x, y) :)
      canvas # UI.lineTo (x,y)
      canvas # UI.stroke



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
     -- draw the triangle, with meshing (once)
    on UI.click drawMeshedTriangle1 $ const $ do
        getBody window #+ [element drawMeshedTriangleForce1]
        canvas # set' UI.strokeStyle "black"
        canvas # UI.beginPath
        forM_ (listLiaisonsMaillage 1) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (concat (trianglePointsOnly 1)))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (concat (trianglePointsOnly 1)))) canvasSize)
        canvas # UI.stroke

    -- draw meshed (once) triangle force
    on UI.click drawMeshedTriangleForce1 $ const $ do
        canvas # set' UI.strokeStyle "red"
        canvas # UI.beginPath
        forM_ (listLiaisonsMaillage 1) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (resultatTriangleMaillage 1))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (resultatTriangleMaillage 1))) canvasSize)
        canvas # UI.stroke

     -- draw the triangle, with meshing (twice)
    on UI.click drawMeshedTriangle2 $ const $ do
        getBody window #+ [element drawMeshedTriangleForce2]
        canvas # set' UI.strokeStyle "black"
        canvas # UI.beginPath
        forM_ (listLiaisonsMaillage 2) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (concat (trianglePointsOnly 2)))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (concat (trianglePointsOnly 2)))) canvasSize)
        canvas # UI.stroke

    -- draw meshed (twice) triangle force
    on UI.click drawMeshedTriangleForce2 $ const $ do
        canvas # set' UI.strokeStyle "red"
        canvas # UI.beginPath
        forM_ (listLiaisonsMaillage 2) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (resultatTriangleMaillage 2))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (resultatTriangleMaillage 2))) canvasSize)
        canvas # UI.stroke

    on UI.click drawMeshedCircle3 $ const $ do
        --getBody window #+ [element drawMeshedTriangleForce2]
        getBody window #+ [element drawMeshedCircleForce3]
        canvas # set' UI.strokeStyle "green"
        canvas # UI.beginPath
        forM_ (listLiaisonsCMaillage 4) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (concat (circlePointsOnly 4)))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (concat (circlePointsOnly 4)))) canvasSize)
        canvas # UI.stroke

    on UI.click drawMeshedCircleForce3 $ const $ do
        --getBody window #+ [element drawMeshedTriangleForce2]
        ---getBody window #+ [element drawMeshedCircleForce3]
        canvas # set' UI.strokeStyle "red"
        canvas # UI.beginPath
        forM_ (listLiaisonsCMaillage 4) $ \[x,y] -> do
            canvas # UI.moveTo (zoomPoint (findIndex x (forInterface (resultatCircleMaillage 4))) canvasSize)
            canvas # UI.lineTo (zoomPoint (findIndex y (forInterface (resultatCircleMaillage 4))) canvasSize)
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