module Interpret where

import ExampleMaillage
import Operations
import Fem 
import Maillage(lins, set, points)

data Shape = Circle Double Double Double
    | Square 
    | Inter Shape Shape
    | Union Shape Shape deriving(Show)



net :: Shape -> [[[Double]]] -> [[[Double]]]
net s (l:ls)
    | contain s l = l:net s ls
    | otherwise = net s ls
net s [] = []



contain :: Shape-> [[Double]] -> Bool
contain (Circle x y r) (l:ls) = (sqrt((x-head l)*(x-head l) + (y - last l)*(y - last l)) < r) && (contain (Circle x y r) ls)
contain (Inter s1 s2) l = (contain s1 l) && (contain s2 l)
contain (Union s1 s2) l = (contain s1 l) || (contain s2 l)
contain s [] = True



--equivalent de triangleMaillage n
c n = net (Circle 1.0 0.87 0.5 )(triangleMaillage n)
circlePointsIdx n = points (c n)
circlePointsOnly n = cleanIdx (circlePointsIdx n)
circleMaillageLines2 n = pairToList(triangleMaillageLinesNet n)
listLiaisonsCMaillage n = intMtoDoubleM (circleMaillageLines2 n)
kcircle n = total (intMtoDoubleM (circleMaillageLines2 n)) (circlePointsOnly n)

kcircleLim n = subMatrix[0,1,convToDouble(length (kcircle n) - 2),convToDouble(length (kcircle n) - 1)] (kcircle n)


-- équivalent de triangleMaillageLines n
triangleMaillageLinesNet n = set (lins (points (c n)) (c n))


testCFLim2 n = forceTriangleLim [[1.0,0.87]] [[0.0,-1.0]] (circlePointsOnly n)

--testFLim n = forceTriangleLim [[1.0,1.74],[0.5,0.87],[1.5,0.87],[1.0,0.0]] [[0.0,-1.0],[-0.4,0.1],[0.4,0.1],[0.0,-1.0]] (init(tail(trianglePointsOnly n)))
--testFLim2 n = forceTriangleLim [[1.0,1.74],[0.5,0.87],[1.5,0.87],[1.0,0.0]] [[0.0,-1.0],[-0.4,0.1],[0.4,0.1],[0.0,-1.0]] (trianglePointsOnly n)
--uiCircle n = concat (deplacementMatrix (kcircleLim n) (testFLim n))


uCircle2 n = concat (deplacementMatrix (kcircle n) (testCFLim2 n))

resultatCircleMaillage n = zipWith (+) (concat (circlePointsOnly n)) (uCircle2 n)





