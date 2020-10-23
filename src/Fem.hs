module Fem where

import Operations

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

total::[[Double]] -> [[Double]] -> Matrix
total [] points = genMatrix (\(x,y) -> 0.0) (convToDouble(length points)*2)
total (x:liaisons) points = plus (partiel (head x) (last x) (localRigidity points 1.0 (head x) (last x)) (convToDouble(length points)*2)) (total liaisons points)



distance ::(Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = sqrt((x1-x2)*(x1-x2) + (y1 - y2)*(y1 - y2))
globalRigidity = genMatrix (\(i,j) -> 0.0) 6

-- calcule la normale
normale :: [Double] -> [Double] -> [Double]
normale p0 p1 = map (*(1/(distance(x0,y0) (x1, y1)))) (zipWith (-) p1 p0)
 where x0 = findIndex 0 p0
       y0 = findIndex 1 p0
       x1 = findIndex 0 p1
       y1 = findIndex 1 p1



-- fonction [a1,a2,a3] [b1,b2,b3] = [a1*[b1,b2,b3], a2*[b1,b2,b3]...]	   
fonction :: [Double] -> [Double] -> Matrix  
fonction [x] l2 = (map(*x) l2) : []
fonction (x:xs) l2 = (map (*x) l2): fonction xs l2




localRigidity :: Matrix -> Double -> Double -> Double -> Matrix
localRigidity positionMatrix rigidityK i j = multk (rigidityK/(distance(x0,y0) (x1, y1))) (fonction  n n)
 where p0 = findIndex i positionMatrix
       p1 = findIndex j positionMatrix
       x0 = findIndex 0 p0
       y0 = findIndex 1 p0
       x1 = findIndex 0 p1
       y1 = findIndex 1 p1
       n1 = normale p0 p1
       minusn1 = map(*(-1)) n1
       n = concat[minusn1, n1]  


listPoints = [[0.0,0.0],[1.0,1.73],[2.0,0.0]]


listLiaisons = [[0.0,1],[1.0,2.0]]
