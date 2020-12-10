module Fem where

import Operations
import Invert



partiel :: Double -> Double -> Matrix -> Double -> Matrix
partiel x y locale n = map(\j -> line j 0 n) [0..(n-1)]
 where line i j n 
            | j == n = [] 
            | (i == 2*x && j == 2*x)      = coeff 0 0 locale: line i (j+1) n
            | (i == 2*x+1 && j == 2*x)    = coeff 1 0 locale: line i (j+1) n
            | (i == 2*x && j == 2*x+1)    = coeff 0 1 locale: line i (j+1) n
            | (i == 2*x+1 && j == 2*x+1)  = coeff 1 1 locale: line i (j+1) n

            | (i == 2*y && j == 2*y)      = coeff 2 2 locale: line i (j+1) n
            | (i == 2*y+1 && j == 2*y)    = coeff 3 2 locale: line i (j+1) n
            | (i == 2*y && j == 2*y+1)    = coeff 2 3 locale: line i (j+1) n
            | (i == 2*y+1 && j == 2*y+1)  = coeff 3 3 locale: line i (j+1) n

            | (i == 2*x && j == 2*y)      = coeff 0 2 locale : line i (j+1) n
            | (i == 2*x && j == 2*y+1)    = coeff 0 3 locale : line i (j+1) n
            | (i == 2*x+1 && j == 2*y)    = coeff 1 2 locale : line i (j+1) n
            | (i == 2*x+1 && j == 2*y+1)  = coeff 1 3 locale : line i (j+1) n

            | (i == 2*y && j == 2*x)      = coeff 2 0 locale : line i (j+1) n
            | (i == 2*y+1 && j == 2*x)    = coeff 3 0 locale : line i (j+1) n
            | (i == 2*y && j == 2*x+1)    = coeff 2 1 locale : line i (j+1) n
            | (i == 2*y+1 && j == 2*x+1)  = coeff 3 1 locale : line i (j+1) n
            | otherwise = 0.0 : line i (j+1) n


-- Rigidity matrix
total::[[Double]] -> [[Double]] -> Matrix
total [] points = genMatrix (\(x,y) -> 0.0) (convToDouble(length points)*2)
total (x:liaisons) points = plus (partiel (head x) (last x) (localRigidity points 1.0 (head x) (last x)) (convToDouble(length points)*2)) (total liaisons points)



distance ::(Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = sqrt((x1-x2)*(x1-x2) + (y1 - y2)*(y1 - y2))
globalRigidity = genMatrix (\(i,j) -> 0.0) 6

-- calcule le vecteur unitaire porté par la poutre (liaison entre 2 points) par deux points (point A vers B)
unitaire :: [Double] -> [Double] -> [Double]
unitaire (x0:(y0:_)) (x1:(y1:_)) = map (*(1/(distance(x0,y0) (x1, y1)))) (zipWith (-) [x1,y1] [x0,y0])



-- fonction [a1,a2,a3] [b1,b2,b3] = [a1*[b1,b2,b3], a2*[b1,b2,b3]...]	   
prod :: [Double] -> [Double] -> Matrix  
prod [x] l2 = (map(*x) l2) : []
prod (x:xs) l2 = (map (*x) l2): prod xs l2




localRigidity :: Matrix -> Double -> Double -> Double -> Matrix
localRigidity positionMatrix rigidityK i j = multk (rigidityK/(distance(x0,y0) (x1, y1))) (prod  n n)
 where p0 = findIndex i positionMatrix
       p1 = findIndex j positionMatrix
       x0 = findIndex 0 p0
       y0 = findIndex 1 p0
       x1 = findIndex 0 p1
       y1 = findIndex 1 p1
       n1 = unitaire p0 p1
       minusn1 = map(*(-1)) n1
       n = concat[minusn1, n1]  


listPoint = [[0.0,0.0],[1.0,1.73],[2.0,0.0]]


listLiaison = [[0.0,1],[1.0,2.0]]


deplacementMatrix :: Matrix -> Matrix -> Matrix
deplacementMatrix k f = mult (inverse k) f

-- to be improved...
insertZeros :: [Double] -> [Double]
insertZeros m = [0.0,0.0] ++ m ++ [0.0,0.0]

csvX :: [Double] -> [Double]
csvX [] = []
csvX l = head (take 2 l) : csvX (drop 2 l)

csvY :: [Double] -> [Double]
csvY [] = []
csvY l = last (take 2 l) : csvY (drop 2 l)

k = total listLiaison listPoint
k2 = subMatrix[0,1,4,5] k

f2 = [[0.1],[0.0]]

ui = concat (deplacementMatrix k2 f2)
u = insertZeros ui

resultat = zipWith (+) (concat listPoint) u

beforeResultat = concat listPoint

forInterface:: [Double] -> [(Double,Double)]
forInterface [] = []
forInterface l = (head (take 2 l), last (take 2 l)) : (forInterface (drop 2 l))

zoomPoint:: (Double,Double)-> Int -> (Double, Double)
zoomPoint (x,y) z = (100*x,fromIntegral z - 100*y)
