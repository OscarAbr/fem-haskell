module Interpret where

import ExampleMaillage
import Operations
import Fem 
import Maillage(lins, set, points)

data Shape = Circle Double Double Double
    | Rectangle Double Double Double Double
    | Hole Shape Shape
    | Inter Shape Shape
    | Union Shape Shape
    | Exclude Shape
    | Mutual Shape Shape deriving(Show)



net :: Shape -> [[[Double]]] -> [[[Double]]]
net s (l:ls)
    | contain s l = l:net s ls
    | otherwise = net s ls
net s [] = []



contain :: Shape-> [[Double]] -> Bool
contain (Circle x y r) (l:ls) = (sqrt((x-head l)*(x-head l) + (y - last l)*(y - last l)) < r) && contain (Circle x y r) ls
contain (Rectangle x y w h) (l:ls) = (head l < x+w) && (head l > x) && (last l < y+h) && (last l > y) && contain (Rectangle x y w h) ls
contain (Inter s1 s2) l = (contain s1 l) && (contain s2 l)
contain (Union s1 s2) l = (contain s1 l) || (contain s2 l)
contain (Mutual s1 s2) l = (contain (Union s1 s2) l) && (not (contain (Inter s1 s2) l))
contain (Hole s1 s2) l = (contain s1 l) && not (contain (Inter s1 s2) l)
contain (Exclude s) l = not (contain s l)
contain s [] = True



--equivalent de triangleMaillage n
--- smiley
-- c n = net (Union (Union (Circle 2.2 3.0 0.3) (Circle 3.8 3.0 0.3))(Inter (Circle 3.0 3.0 1.2) (Circle 3.0 1.0 1.2))) (triangleMaillage n)
--voiture pleine
-- c n = net (Union (Union (Rectangle 3.0 2.0 4.0 3.0) (Rectangle 6.7 2.0 2.0 1.5))(Union (Circle 4.1 1.9 0.7) (Circle 7.1 1.9 0.7))) (triangleMaillage n)
--voiture fenêtre
--c n = net (Hole (Union (Union (Rectangle 3.0 2.0 4.0 3.0) (Rectangle 6.7 2.0 2.0 1.5))(Union (Circle 4.1 1.9 0.7) (Circle 7.1 1.9 0.7))) (Rectangle 4.0 3.0 2.7 1.5)) (triangleMaillage n)

c n = net (Hole (Union (Union (Rectangle 3.0 2.0 4.0 3.0) (Rectangle 6.7 2.0 2.0 1.5))(Union (Circle 4.1 1.9 0.7) (Circle 7.1 1.9 0.7))) (Rectangle 4.0 3.0 2.7 1.5)) (triangleMaillage n)
circlePointsIdx n = points (c n)
circlePointsOnly n = cleanIdx (circlePointsIdx n)
circleMaillageLines2 n = pairToList(triangleMaillageLinesNet n)
listLiaisonsCMaillage n = intMtoDoubleM (circleMaillageLines2 n)
kcircle n = total (intMtoDoubleM (circleMaillageLines2 n)) (circlePointsOnly n)

kcircleLim n = subMatrix[0,1,convToDouble(length (kcircle n) - 2),convToDouble(length (kcircle n) - 1)] (kcircle n)


-- équivalent de triangleMaillageLines n
triangleMaillageLinesNet n = set (lins (points (c n)) (c n))


testCFLim2 n = forceTriangleLim [[4.3125,1.25],[3.9375,1.25],[7.3125,1.25],[6.9375,1.25]] [[0.0,0.1],[0.0,0.1],[0.0,0.1],[0.0,0.1]] (circlePointsOnly n)

-- choc frontal [[8.625,3.0],[8.625,2.5]] [[-3.0,0.0],[-3.0,0.0]]
-- dos d'ane [[4.3125,1.25],[3.9375,1.25],[7.3125,1.25],[6.9375,1.25]] [[0.0,1.0],[0.0,1.0],[0.0,1.0],[0.0,1.0]]

--testFLim n = forceTriangleLim [[1.0,1.74],[0.5,0.87],[1.5,0.87],[1.0,0.0]] [[0.0,-1.0],[-0.4,0.1],[0.4,0.1],[0.0,-1.0]] (init(tail(trianglePointsOnly n)))
--testFLim2 n = forceTriangleLim [[1.0,1.74],[0.5,0.87],[1.5,0.87],[1.0,0.0]] [[0.0,-1.0],[-0.4,0.1],[0.4,0.1],[0.0,-1.0]] (trianglePointsOnly n)
--uiCircle n = concat (deplacementMatrix (kcircleLim n) (testFLim n))


uCircle2 n = concat (deplacementMatrix (kcircle n) (testCFLim2 n))

resultatCircleMaillage n = zipWith (+) (concat (circlePointsOnly n)) (uCircle2 n)





