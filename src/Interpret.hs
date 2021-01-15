module Interpret where

import ExampleMaillage
import Operations
import Fem 
import Maillage(lins, set, points)

data Shape = Circle Double Double Double
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
contain (Circle x y r) (l:ls) = (sqrt((x-head l)*(x-head l) + (y - last l)*(y - last l)) < r) && (contain (Circle x y r) ls)
contain (Inter s1 s2) l = (contain s1 l) && (contain s2 l)
contain (Union s1 s2) l = (contain s1 l) || (contain s2 l)
contain (Mutual s1 s2) l = (contain (Union s1 s2) l) && (not (contain (Inter s1 s2) l))
contain (Exclude s) l = not (contain s l)
contain s [] = True



--equivalent de triangleMaillage n
c1 = Circle 3.0 1.8 1.8
c2 = Circle 2.4375 2.2733375000000002 0.5
c3 = Circle 3.5625 2.2733375000000002 0.5

c4 = Circle 3.0 1.0 0.6
c5 = Inter (Circle 3.0 (-0.25) 1.5) (Circle 3.0 2.1 1.5 ) 
c8 = Inter (Exclude (Union (Union (c3) (c2)) c5) ) c1 
c n = net ( c8) (triangleMaillage n)
circlePointsIdx n = points (c n)
circlePointsOnly n = cleanIdx (circlePointsIdx n)
circleMaillageLines2 n = pairToList(triangleMaillageLinesNet n)
listLiaisonsCMaillage n = intMtoDoubleM (circleMaillageLines2 n)
kcircle n = total (intMtoDoubleM (circleMaillageLines2 n)) (circlePointsOnly n)

kcircleLim n = subMatrix[0,1,convToDouble(length (kcircle n) - 2),convToDouble(length (kcircle n) - 1)] (kcircle n)


-- équivalent de triangleMaillageLines n
triangleMaillageLinesNet n = set (lins (points (c n)) (c n))


testCFLim2 n = forceTriangleLim [[4.5,1.29905]] [[1.0,0.0]] (circlePointsOnly n)

--testFLim n = forceTriangleLim [[1.0,1.74],[0.5,0.87],[1.5,0.87],[1.0,0.0]] [[0.0,-1.0],[-0.4,0.1],[0.4,0.1],[0.0,-1.0]] (init(tail(trianglePointsOnly n)))
--testFLim2 n = forceTriangleLim [[1.0,1.74],[0.5,0.87],[1.5,0.87],[1.0,0.0]] [[0.0,-1.0],[-0.4,0.1],[0.4,0.1],[0.0,-1.0]] (trianglePointsOnly n)
--uiCircle n = concat (deplacementMatrix (kcircleLim n) (testFLim n))


uCircle2 n = concat (deplacementMatrix (kcircle n) (testCFLim2 n))

resultatCircleMaillage n = zipWith (+) (concat (circlePointsOnly n)) (uCircle2 n)





